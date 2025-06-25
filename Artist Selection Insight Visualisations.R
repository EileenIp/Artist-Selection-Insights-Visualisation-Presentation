# Load required libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(stats)
library(ggridges)
library(viridis)
library(RColorBrewer)
library(scales)
library(tidyr)
library(stringr)
library(forcats)

# Read the Data
load("MSD.RData")

# Check distribution of major and minor chord variable as percentages
prop.table(table(MSD$mode)) * 100

# Data Filtering
filtered_data <- MSD %>%
  group_by(artist_name) %>%
  filter(n() >= 4) %>% # Only artists with ≥4 songs
  ungroup() %>%
  # Max duration constraint (4 mins), must be between 2005 and 2010, and 
  # use only major mode songs
  filter(duration <= 240, year >= 2005, year <= 2010, mode==1)

# Check N/A values
na_counts <- sapply(filtered_data, function(x) sum(is.na(x)))
print(na_counts)

# Define columns to impute
cols_to_impute <- c("artist_7digitalid", "artist_familiarity", "artist_hotttnesss", 
                    "end_of_fade_in", "release_7digitalid", "song_hotttnesss", 
                    "tempo", "track_7digitalid", "year")

# Efficiently imputes missing values using artist-specific mean, else overall mean
efficient_impute <- function(data, columns_to_impute) {
  
  df_imputed <- data
  
  for (col in columns_to_impute) {
    if (!col %in% names(df_imputed)) {
      cat("Column", col, "not found. Skipping.\n")
      next
    }
    
    # Calculate overall mean
    overall_mean <- mean(df_imputed[[col]], na.rm = TRUE)
    
    # Calculate artist-specific means
    artist_means <- df_imputed %>%
      filter(!is.na(.data[[col]])) %>%
      group_by(artist_name) %>%
      summarise(artist_mean = mean(.data[[col]], na.rm = TRUE), .groups = 'drop')
    
    # Impute using dplyr
    if (col %in% c("year", "time_signature")) {
      # For year and time_signature columns, round to nearest integer
      df_imputed <- df_imputed %>%
        left_join(artist_means, by = "artist_name") %>%
        mutate(
          !!col := case_when(
            is.na(.data[[col]]) & !is.na(artist_mean) ~ round(artist_mean),
            is.na(.data[[col]]) & is.na(artist_mean) ~ round(overall_mean),
            TRUE ~ .data[[col]]
          )
        ) %>%
        select(-artist_mean)
      
      cat("Completed efficient imputation for", col, "(with rounding)\n")
    } else {
      # For other columns, no rounding
      df_imputed <- df_imputed %>%
        left_join(artist_means, by = "artist_name") %>%
        mutate(
          !!col := case_when(
            is.na(.data[[col]]) & !is.na(artist_mean) ~ artist_mean,
            is.na(.data[[col]]) & is.na(artist_mean) ~ overall_mean,
            TRUE ~ .data[[col]]
          )
        ) %>%
        select(-artist_mean)
      
      cat("Completed efficient imputation for", col, "\n")
    }
  }
  
  return(df_imputed)
}

# Apply the imputation
processed_data <- efficient_impute(filtered_data, cols_to_impute)

# Recheck for NA values
na_counts <- sapply(processed_data, function(x) sum(is.na(x)))
print(na_counts)

# Drop any rows still containing missing values
processed_data <- processed_data[complete.cases(processed_data),]

# Custom theme
custom_theme <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 15)),
      plot.subtitle = element_text(size = 12, color = "gray40", hjust = 0, margin = margin(b = 20)),
      axis.title = element_text(size = 11, face = "bold"),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# PLOT 1: Line plot showing the yearly averages

# Compute yearly averages of artist/song metrics
plot1 <- processed_data %>%
  group_by(year) %>%
  summarize(
    mean_artist_hotttnesss = mean(artist_hotttnesss, na.rm = TRUE),
    mean_artist_familiarity = mean(artist_familiarity, na.rm = TRUE),
    mean_song_hotttnesss = mean(song_hotttnesss, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -year, names_to = "metric", values_to = "value") %>%
  mutate(
    metric_clean = case_when(
      metric == "mean_artist_hotttnesss" ~ "Artist Hotness",
      metric == "mean_artist_familiarity" ~ "Artist Familiarity", 
      metric == "mean_song_hotttnesss" ~ "Song Hotness"
    )
  )

# Plot line chart of trends over time
ggplot(plot1, aes(x = year, y = value, color = metric_clean)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_text(data = plot1 %>% filter(year == min(year)),
            aes(label = percent(value, accuracy = 0.1)), 
            hjust=0.1, vjust = -0.5, size = 3.5, fontface = "bold", show.legend = FALSE) +
  geom_text(data = plot1 %>% filter(year == max(year)),
            aes(label = percent(value, accuracy = 0.1)), 
            hjust = -0.2, size = 3.5, fontface = "bold", show.legend = FALSE) +
  scale_color_viridis_d(option = "viridis", begin = 0.3, end = 0.8) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0.05, 0.15))) +
  scale_x_continuous(breaks = seq(2005, 2010, 1)) +
  labs(
    title = "Music Industry Metrics Trends (2005-2010)",
    subtitle = "These metrics set the threshold to be choosen in the selection. Artist familiarity shows the strongest performance over time and song hotness shows the largest growth.",
    x = NULL,
    y = "Average Score",
    color = "Metrics"
  ) +
  custom_theme() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )

# Filter data to those standards, only retain top-performing tracks
processed_data <- processed_data %>%
  filter(artist_familiarity >= 0.60, artist_hotttnesss >= 0.45, song_hotttnesss >= 0.55)

# PLOT 2: Stacked bar chart of top 20 artists ranked by artist familiarity and its cumulative song hotness

# Aggregate artist-level metrics by year
top_artists_data <- processed_data %>%
  group_by(year, artist_name) %>%
  summarise(
    avg_artist_familiarity = mean(artist_familiarity),
    avg_song_hotttness = mean(song_hotttnesss),
    avg_artist_hotttnesss = mean(artist_hotttnesss), 
    avg_duration = mean(duration),
    avg_loudness = mean(loudness),
    avg_tempo = mean(tempo),
    avg_end_of_fade_in = mean(end_of_fade_in),
    avg_start_of_fade_out = mean(start_of_fade_out),
    avg_year = mean(year),
    count = n(),
    .groups = "drop"
  ) %>%
  # Keep only artists with +4 songs in multiple years for better visualisation
  group_by(artist_name) %>%
  filter(n() >= 4) %>%
  ungroup() %>%
  # Select top 20 artists based on average familiarity
  arrange(desc(avg_artist_familiarity)) %>%
  filter(artist_name %in% unique(artist_name)[1:20]) 

# Calculate cumulative song hotness for each artist
artist_cum_hotness <- processed_data %>%
  filter(artist_name %in% unique(top_artists_data$artist_name)) %>%
  group_by(artist_name) %>%
  summarise(cum_song_hotttness = sum(song_hotttnesss)) %>%
  arrange(desc(cum_song_hotttness)) %>%
  mutate(rank = row_number(),
         highlight = rank <= 10) %>%
  filter(rank <= 10)

# Create the plot with annotations
ggplot(top_artists_data, aes(x = reorder(artist_name, avg_artist_familiarity), 
                             y = avg_song_hotttness, fill = factor(year))) +
  geom_col(alpha = 0.85, width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", avg_song_hotttness)), 
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 3) +
  # Add annotations for top 10 artists by cumulative hotness
  geom_text(data = artist_cum_hotness %>% filter(highlight),
            aes(x = artist_name, y = -0.05, 
                label = paste0("#", rank)),
            inherit.aes = FALSE,
            hjust = 1, vjust = 0.5,
            color = "darkgreen", size = 3.5) +
  scale_fill_viridis_d(name = "Year", option = "viridis", begin = 0.2, end = 0.9) +
  coord_flip() + # Flip axes for better readability
  labs(
    title = "Song's Hotness of Top 20 Artist Familiarity (2005-2010)",
    subtitle = "Stacked by year showing cumulative song hotness. Top 10 artists by cumulative hotness are annotated.",
    x = NULL,
    y = "Song Hotness Score",
    fill = "Year",
  ) +
  custom_theme() +
  theme(
    axis.text.y = element_text(size = 10),
    legend.position = "right",
    panel.grid.minor.x = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.y = element_blank()
  )

# PLOT 3: Comparison of Top Four Songs by Top Ten Artists

# Create a unique artist-title dataset
artist_title_data <- processed_data %>%
  group_by(artist_name, title) %>%
  slice(1) %>%
  ungroup()

# Get top 10 artists based on cumulative song hotness
top_artists <- processed_data %>%
  filter(artist_name %in% unique(top_artists_data$artist_name)) %>%
  group_by(artist_name) %>%
  summarise(
    avg_artist_familiarity = mean(artist_familiarity, na.rm = TRUE),
    avg_artist_hotttnesss = mean(artist_hotttnesss, na.rm = TRUE), 
    avg_song_hotttnesss = mean(song_hotttnesss, na.rm = TRUE),
    cum_song_hotttness = sum(song_hotttnesss, na.rm = TRUE),
    number_of_songs = n()
  ) %>%
  ungroup() %>%
  arrange(desc(cum_song_hotttness)) %>%
  mutate(rank = row_number(), # Rank artists by cumulative hotness
       highlight = rank <= 10) %>% # Flag top 10 artists
  filter(rank <= 10)

# Select top 4 songs per top artist by artist familiarity
top_four_songs_data <- artist_title_data %>%
  inner_join(top_artists, by = "artist_name") %>%
  group_by(artist_name) %>%
  arrange(desc(avg_artist_familiarity)) %>%
  slice_head(n = 4) %>%
  select(artist_name, avg_artist_familiarity, song_hotttnesss, title) %>%
  group_by(artist_name) %>%
  mutate(
    song_rank = rank(-song_hotttnesss, ties.method = "first"),
    # Truncate long titles to improve readability
    short_title = ifelse(nchar(title) > 20, paste0(substr(title, 1, 18), "..."), title),
    song_label = short_title,
    # Position for labels
    y_position = song_hotttnesss * 100
  ) %>%
  ungroup()

# Create the horizontal grouped bar chart
ggplot(top_four_songs_data, aes(x = artist_name, y = y_position, fill = factor(song_rank))) +
  geom_col(position = position_dodge(width = 0.8), alpha = 0.8, width = 0.7) +
  # Add short song title labels next to bars
  geom_text(aes(label = song_label),
            position = position_dodge(width = 0.8),
            hjust = -0.1, 
            size = 3, 
            color = "#333333", fontface = "bold") +
  scale_fill_viridis_d(name = "Song Rank", option = "viridis", begin = 0.3, end = 0.8,
                       labels = c("1st", "2nd", "3rd", "4th")) + # Add padding above bars
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = function(x) paste0(x, "%")) + # Format y-axis as percentage
  coord_flip() +
  labs(
    title = "Top Four Songs by Top Ten Artists (2005-2010)",
    subtitle = "Showing top four hottest songs for each artist ranked by artist familiarity.",
    x = NULL,
    y = "Song Hotness (%)",
    fill = "Song Rank"
  ) +
  custom_theme() +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )

# PLOT 4: Correlation Heatmap of Song Features and Ratings

# Select relevant numerical columns from the dataset
num_processed_data <- processed_data %>%
  select_if(is.numeric) %>%
  select(tempo, end_of_fade_in, artist_hotttnesss, artist_familiarity, 
         song_hotttnesss, start_of_fade_out, duration, loudness)

# Convert to matrix format for correlation computation
matrix_num_processed_data <- as.matrix(num_processed_data)

# Compute correlation matrix and round to 2 decimal places
corr_mat <- round(cor(matrix_num_processed_data),2)

#  Reorder matrix using hierarchical clustering on correlation distance
dist <- as.dist((1-corr_mat)/2)
hc <- hclust(dist)
corr_mat <-corr_mat[hc$order, hc$order] # Reorder rows and columns

# Melt the matrix into long format for plotting
melted_corr_mat <- melt(corr_mat)

# Prepare aesthetics for text color based on correlation strength
plot4 <- melted_corr_mat %>%
  mutate(
    abs_value = abs(value),
    text_color = ifelse(abs_value > 0.5, "white", "black")
  )

# Plot the heatmap
ggplot(plot4, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.2) + # Create heatmap tiles
  geom_text(aes(label = sprintf("%.2f", value), color = text_color), 
            size = 3, fontface = "bold") + # Add correlation values
  scale_fill_gradient2(low = "#3B528BFF", mid = "white", high = "#75D054FF",
                       midpoint = 0, name = "Correlation") +
  scale_color_identity() + # Use exact color values from text_color column
  labs(
    title = "Correlation Matrix of Song Features and Ratings (2005-2010)",
    subtitle = "Correlation Matrix to show relationship patterns of song features and artists ratings.",
    x = NULL, y = NULL
  ) +
  custom_theme() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    panel.grid = element_blank(),
    legend.position = "right"
  )

# PLOTS 5-9: Trend Lines for Top 15 Artists by Artist Familiarity

# Filter top 15 artists by average artist familiarity
top_artists_info <- top_artists_data %>%
  group_by(artist_name) %>%
  mutate(overall_avg_hotness = mean(avg_artist_familiarity)) %>%
  ungroup() %>%
  arrange(desc(overall_avg_hotness)) %>%
  filter(artist_name %in% unique(artist_name)[1:15])

# Function for creating trend plots by metric
create_trend_plot <- function(data, y_var, y_label, plot_title) {
  ggplot(data, aes(x = year, y = !!sym(y_var), color = artist_name)) +
    geom_line(linewidth = 1.1, alpha = 0.7) + # Smoother and more readable trend lines
    geom_point(size = 2, alpha = 0.8) + # Points for individual year values
    scale_color_viridis_d(option = "viridis") + # Use perceptually uniform color scale
    scale_x_continuous(breaks = unique(data$year)) + # Clean year axis
    labs(
      title = plot_title,
      subtitle = "Trends across years for top 15 artists by artist familiarity.",
      x = NULL,
      y = y_label,
      color = "Artist"
    ) +
    custom_theme() +
    theme(
      legend.position = "bottom",  # Place legend below for space efficiency
      panel.grid.major.x = element_line(color = "gray95")
    ) 
}

# Generate one plot for each song feature

# Plot 5: End of fade-in trends
create_trend_plot(top_artists_info, "avg_end_of_fade_in", 
                                    "End of Fade In (seconds)", 
                                    "End of Fade In Trends for Top 15 Artists (2005-2010)")

# Plot 6: Loudness trends
create_trend_plot(top_artists_info, "avg_loudness", 
                                    "Loudness (dB)", 
                                    "Loudness Trends for Top 15 Artists (2005-2010)")

# Plot 7: Start of fade-out trends
create_trend_plot(top_artists_info, "avg_start_of_fade_out", 
                                    "Start of Fade Out (seconds)", 
                                    "Start of Fade Out Trends for Top 15 Artists (2005-2010)")

# Plot 8: Tempo trends
create_trend_plot(top_artists_info, "avg_tempo", 
                                    "Tempo (BPM)", 
                                    "Tempo Trends for Top 15 Artists (2005-2010)")

# Plot 9: Duration trends
create_trend_plot(top_artists_info, "avg_duration", 
                                    "Duration (seconds)", 
                                    "Song Duration Trends for Top 15 Artists (2005-2010)")

# PLOT 10: Key Distribution Over the Years (2005–2010)

# Summarise the number of songs by key for each year
key_year_summary <- processed_data %>%
  group_by(year, key) %>%
  summarise(
    count = n(), # Count how many songs used into each key per year
    .groups = "drop"
  )  

# Map key integers (0–11) to human-readable note names
plot10 <- key_year_summary %>%
  mutate(key_name = case_when(
    key == 0 ~ "C", key == 1 ~ "C#", key == 2 ~ "D", key == 3 ~ "D#",
    key == 4 ~ "E", key == 5 ~ "F", key == 6 ~ "F#", key == 7 ~ "G",
    key == 8 ~ "G#", key == 9 ~ "A", key == 10 ~ "A#", key == 11 ~ "B"
  ))

#  Create the line plot showing distribution of keys over years
ggplot(plot10, aes(x = year, y = count, color = key_name)) + 
  geom_line(linewidth = 1.2, alpha = 0.8) + # Trend lines for each key
  geom_point(size = 2, alpha = 0.9) +
  scale_color_viridis_d(name = "Musical Key", option = "viridis") +
  scale_x_continuous(breaks = unique(plot10$year)) + # Ensure clean year ticks
  labs(
    title = "Major Key Distribution Over the Years (2005-2010)",
    subtitle = "Popularity of different major keys used in popular songs across the years from artists above the threshold.",
    x = NULL,
    y = "Number of Songs",
    color = "Key"
  ) +
  custom_theme() +
  theme(legend.position = "right")

# PLOT 11: Time Signature Distribution Over the Years (2005–2010)

# Summarise number of songs per time signature for each year
plot_11 <- processed_data %>%
  group_by(year, time_signature) %>%
  summarise(
    count = n(), # Count number of songs for each time signature per year
    .groups = "drop"
  ) %>%
  # Create readable label for each time signature
  mutate(time_sig_label = paste0(time_signature, "/4"))

# Create the line plot for time signature distribution
ggplot(plot_11, aes(x = year, y = count, color = time_sig_label)) +
  geom_line(linewidth = 1.5, alpha = 0.8) + # Draw trend lines per time signature
  geom_point(size = 3, alpha = 0.9) +
  scale_color_viridis_d(name = "Time Signature", option = "viridis") +
  scale_x_continuous(breaks = unique(plot_11$year)) +
  scale_y_continuous(labels = comma_format()) + # Format y-axis numbers with commas
  labs(
    title = "Time Signature Distribution Over the Years (2005-2010)",
    subtitle = "Popularity of time signatures used in popular songs across the years from artists above the threshold.",
    x = NULL,
    y = "Number of Songs",
    color = "Time Signature"
  ) +
  custom_theme() +
  theme(legend.position = "right")

# PLOT 12: Density ridges plot showing the tempo distribution and the highest peak (mode) for each time signature

# Prepare summary dataset for plotting
plot12 <- processed_data %>%
  group_by(year, artist_name, time_signature) %>%
  summarise(count = n(), # Count number of songs for each artist and time signature per year
            avg_tempo = mean(tempo), # Average tempo per artist per year per time signature
            .groups = "drop") %>%
  mutate(time_sig_label = factor(paste0(time_signature, "/4"),
                                 levels = paste0(sort(unique(time_signature)), "/4")), # Create readable time signature label
                                 time_sig_label = fct_rev(time_sig_label)) %>%   # Convert to factor and reverse for plotting order
           filter(!is.na(time_signature))  
         
# Function to find the mode (highest peak) of a distribution
find_mode <- function(x) {
 dens <- density(x)
 dens$x[which.max(dens$y)] # Return x value with highest density
}

# Calculate mode tempos for each time signature
mode_tempos <- plot12 %>%
 group_by(time_sig_label) %>%
 summarise(mode_tempo = find_mode(avg_tempo))

# Create the density ridge plot
ggplot(plot12, aes(x = avg_tempo, y = time_sig_label, fill = time_sig_label)) +
 geom_density_ridges(
   alpha = 0.7, 
   scale = 0.9, 
   bandwidth = 5,  
   rel_min_height = 0.01  
 ) +
 # Add red dashed lines for mode tempo (peak) per time signature
 geom_segment(
   data = mode_tempos,
   aes(x = mode_tempo, xend = mode_tempo, 
       y = as.numeric(time_sig_label), yend = as.numeric(time_sig_label) + 0.9),
   color = "red", linetype = "dashed", size = 0.7
 ) +
 # Annotate each mode with BPM value
 geom_text(
   data = mode_tempos,
   aes(x = mode_tempo, y = time_sig_label, 
       label = paste("Peak:", round(mode_tempo), "BPM")),
   hjust = -0.1,  # Position text to the right of the mode line
   vjust = -0.5,  # Adjust vertical position
   size = 3,
   color = "black",
   inherit.aes = FALSE,
   fontface = "bold"
 ) +
  # Improve color palette and aesthetics
 scale_fill_viridis_d(
   name = "Time Signature",  
   option = "viridis",
   direction = -1
 ) +
  # Add plot labels and custom theming
 labs(
   title = "Tempo Distribution by Time Signature (2005-2010)",
   subtitle = "Density curves showing patterns between tempo and time signatures. Red dashed lines indicate the most common tempo (peak) for each time signature.",
   x = "Tempo (BPM)",
   y = "Time Signature"
 ) +
 custom_theme() +
 theme(
   legend.position = "none",
   panel.grid.major.y = element_blank(),
   plot.subtitle = element_text(lineheight = 1.1),
 )
