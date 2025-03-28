library(readr)
library(ggplot2)
library(plotly)
library(readr)
library(dplyr)
library(tibble)
library(gt)
library(corrplot)
library(dplyr)
library(knitr)
library(kableExtra)
library(lme4)

Spotify_2024_data <- read_csv("C:/Users/joshh/Downloads/Spotify Most Streamed Songs.csv")
Spotify_2024_data <- read_csv("C:/Users/jh1376/OneDrive - University of Exeter/MSc Applied Data Science and Statistics/MTHM501 - Working with Data/Working with Data - Coursework/Spotify Most Streamed Songs.csv")
View(Spotify_2024_data)

## note to change the column names so they are more presentable

str(Spotify_2024_data)

Spotify_2024_data <- na.omit(Spotify_2024_data) # Removes rows with NA values

  



##----------------------------------------------------- Data Wrangling -------------------------------------------------------------##


unique(Spotify_2024_data$streams)

Spotify_2024_data$`danceability_%` <- as.numeric(Spotify_2024_data$`danceability_%`)

Spotify_2024_data$streams <- as.numeric(gsub(",", "", as.character(Spotify_2024_data$streams)))

Spotify_2024_data$streams <- as.numeric(gsub(",", "", Spotify_2024_data$streams))

sum(is.na(Spotify_2024_data$streams))

Spotify_2024_data <- na.omit(Spotify_2024_data)



# Extract the first artist (artists separated by commas)
Spotify_2024_data <- Spotify_2024_data %>%
  mutate(first_artist = sapply(strsplit(`artist(s)_name`, ","), `[`, 1))

# Calculate the number of unique first artists
num_unique_first_artists <- Spotify_2024_data %>%
  summarise(unique_first_artists = n_distinct(first_artist)) %>%
  pull(unique_first_artists)


num_unique_first_artists



##-------------------------------------------------------------- Making Tables ------------------------------------------------------##

library(dplyr)
library(knitr)
library(kableExtra)


# Create individual summaries for each relevant metric
bpm_summary <- data.frame(
  Metric = "BPM",
  Mean = mean(Spotify_2024_data$bpm, na.rm = TRUE),
  Median = median(Spotify_2024_data$bpm, na.rm = TRUE),
  SD = sd(Spotify_2024_data$bpm, na.rm = TRUE),
  Min = min(Spotify_2024_data$bpm, na.rm = TRUE),
  Max = max(Spotify_2024_data$bpm, na.rm = TRUE),
  Unique_Artists = n_distinct(Spotify_2024_data$first_artist)
)

streams_summary <- data.frame(
  Metric = "Streams",
  Mean = mean(Spotify_2024_data$streams, na.rm = TRUE),
  Median = median(Spotify_2024_data$streams, na.rm = TRUE),
  SD = sd(Spotify_2024_data$streams, na.rm = TRUE),
  Min = min(Spotify_2024_data$streams, na.rm = TRUE),
  Max = max(Spotify_2024_data$streams, na.rm = TRUE),
  Unique_Artists = n_distinct(Spotify_2024_data$first_artist) 
)

danceability_summary <- data.frame(
  Metric = "Danceability (%)",
  Mean = mean(Spotify_2024_data$`danceability_%`, na.rm = TRUE),
  Median = median(Spotify_2024_data$`danceability_%`, na.rm = TRUE),
  SD = sd(Spotify_2024_data$`danceability_%`, na.rm = TRUE),
  Min = min(Spotify_2024_data$`danceability_%`, na.rm = TRUE),
  Max = max(Spotify_2024_data$`danceability_%`, na.rm = TRUE),
  Unique_Artists = n_distinct(Spotify_2024_data$first_artist)
)

# Combine the individual summaries into a single table
summary_table <- bind_rows(bpm_summary, streams_summary, danceability_summary)

# Format the table to display large numbers with commas
summary_table$Mean <- scales::comma(summary_table$Mean)
summary_table$Median <- scales::comma(summary_table$Median)
summary_table$SD <- scales::comma(summary_table$SD)
summary_table$Min <- scales::comma(summary_table$Min)
summary_table$Max <- scales::comma(summary_table$Max)

# Present the table using kableExtra for clean formatting
summary_table %>%
  kbl(col.names = c("Metric", "Mean", "Median", "Standard Deviation", "Min", "Max", "Unique Artists"), 
      caption = "Summary Statistics for Key Metrics") %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed", "responsive"))



# Select the relevant columns for correlation analysis (e.g., numerical attributes)
correlation_data <- Spotify_2024_data %>%
  select(streams, bpm, `danceability_%`, `energy_%`, `acousticness_%`, `valence_%`)  # Choose columns

# Calculate the correlation matrix (assuming the data is prepared)
correlation_matrix <- cor(correlation_data, use = "complete.obs")

# Convert the correlation matrix to a data frame
correlation_df <- as.data.frame(correlation_matrix)

# Assuming correlation_df is the data frame with the correlation matrix
correlation_table <- correlation_df %>%
  rownames_to_column(var = "Metric") %>%
  mutate(across(-Metric, ~ cell_spec(signif(.x, 3), 
                                     color = "white", 
                                     background = spec_color(.x, option = "D", end = 0.9)))) %>%
  knitr::kable(format = "html", escape = FALSE, 
               caption = "Color Coded Correlation Matrix of Song Attributes") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(1, bold = TRUE)

# Display the table in an HTML viewer
correlation_table


install.packages("corrplot")
library(corrplot)

# Calculate the correlation matrix (assuming correlation_data is prepared)
correlation_matrix <- cor(correlation_data, use = "complete.obs")

# Create a heatmap of the correlation matrix
corrplot(correlation_matrix, method = "color", type = "upper", 
         col = colorRampPalette(c("red", "white", "blue"))(200),
         addCoef.col = "black", # Add correlation coefficients on the plot
         tl.col = "black", tl.srt = 45, # Rotate and color axis labels
         number.cex = 0.7)  # Text size for correlation values



##------------------------------------------------------ Making Scatterplot graph ---------------------------------------------------##


library(ggplot2)

# Create a boxplot to show the distribution of streams by key
boxplot <- ggplot(Spotify_2024_data, aes(x = factor(key), y = streams)) +
  geom_boxplot(aes(fill = factor(key))) +  # Boxplot with key on x-axis and streams on y-axis
  labs(title = "Song Streams by Key", x = "Key of the Song", y = "Number of Streams") +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend as it's redundant

# Convert the ggplot object to an interactive plot using plotly
interactive_plot <- ggplotly(boxplot)

# Display the interactive plot
interactive_plot

# Save the interactive plot as an HTML file
library(htmlwidgets)

saveWidget(interactive_plot, "interactive_boxplot.html", selfcontained = TRUE)

# Open the HTML file in the default browser
browseURL("interactive_boxplot.html")

mean_streams_by_key <- Spotify_2024_data %>%
  group_by(key) %>%
  summarize(mean_streams = mean(streams, na.rm = TRUE))

# Create a bar plot to show mean streams by key
ggplot(mean_streams_by_key, aes(x = factor(key), y = mean_streams)) +
  geom_bar(stat = "identity", aes(fill = factor(key))) +  # Bar plot
  labs(title = "Mean Song Streams by Key", x = "Key of the Song", y = "Mean Number of Streams") +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas
  theme_minimal() +
  theme(legend.position = "none")


# Create the scatter plot with streams on the x-axis

ggplot(Spotify_2024_data, aes(x = streams, y = bpm, color = `danceability_%`)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_gradient(low = "yellow", high = "blue") + # Color gradient for danceability
  labs(title = "Mapping BPM with Number of Streams and Danceability",
       x = "Number of Streams",
       y = "BPM",
       color = "Danceability (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))





##---------------------------------------------- Iteration for interactive scatter plot ----------------------------------------------##

Spotify_2024_data$streams <- as.numeric(gsub(",", "", as.character(Spotify_2024_data$streams)))

# Remove rows with NA values in streams
Spotify_2024_data <- na.omit(Spotify_2024_data)

# Create the ggplot object
p <- ggplot(Spotify_2024_data, aes(x = streams, y = bpm, color = `danceability_%`, text = track_name)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_gradient(low = "yellow", high = "blue") + 
  labs(title = "Mapping BPM with Number of Streams and Danceability",
       x = "Number of Streams",
       y = "BPM",
       color = "Danceability (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# Convert the ggplot object to an interactive plotly object
interactive_plot <- ggplotly(p, tooltip = "text")

# Display the interactive plot
interactive_plot

# Replace the log scale with a continuous scale for x-axis
p <- p + 
  scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()), 
                     limits = c(0, max(Spotify_2024_data$streams) * 1.05), 
                     expand = expansion(mult = c(0.05, 0.05))) +  # Adjusting the limits and adding padding
  scale_size_continuous(range = c(3, 8))  # Standardize dot sizes

# Convert to interactive plotly object
interactive_plot <- ggplotly(p, tooltip = "text")

# Save and display the interactive plot
saveWidget(interactive_plot, "interactive_scatter_plot_no_log.html", selfcontained = TRUE)
browseURL("interactive_scatter_plot_no_log.html")



install.packages("scales")
library(scales)


####### test without log scale ##########

Spotify_2024_data$`energy_%` <- as.numeric(as.character(Spotify_2024_data$`energy_%`))

# Calculate averages for BPM and streams
bpm_avg <- mean(Spotify_2024_data$bpm, na.rm = TRUE)
streams_avg <- mean(Spotify_2024_data$streams, na.rm = TRUE)

# Create the plot without log scale
p <- ggplot(Spotify_2024_data, aes(x = streams, y = bpm, color = `danceability_%`, 
                                   text = paste("Track: ", track_name, 
                                                "<br>BPM: ", bpm, 
                                                "<br>Streams: ", streams, 
                                                "<br>Danceability: ", `danceability_%`))) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed") +  # Trend line
  geom_hline(yintercept = bpm_avg, linetype = "dotted", color = "red") +  # Horizontal average line for BPM
  geom_vline(xintercept = streams_avg, linetype = "dotted", color = "red") +  # Vertical average line for streams
  scale_color_viridis_c(option = "plasma") +  # Color palette
  labs(title = "Mapping BPM with Number of Streams and Danceability",
       x = "Number of Streams",
       y = "BPM",
       color = "Danceability (%)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))

# Replace the log scale with a continuous scale for x-axis
p <- p + 
  scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()), 
                     limits = c(0, max(Spotify_2024_data$streams) * 1.05), 
                     expand = expansion(mult = c(0.05, 0.05))) +  # Adjusting the limits and adding padding
  scale_size_continuous(range = c(3, 8))  # Standardize dot sizes

# Convert to interactive plotly object
interactive_plot <- ggplotly(p, tooltip = "text")

# Save and display the interactive plot
saveWidget(interactive_plot, "interactive_scatter_plot_no_log.html", selfcontained = TRUE)
browseURL("interactive_scatter_plot_no_log.html")


### Plot with trends ###

library(zoo)

Spotify_2024_data <- Spotify_2024_data %>%
  arrange(streams) %>%
  mutate(moving_avg_bpm = zoo::rollmean(bpm, k = 10, fill = NA))

Spotify_2024_data$'energy_%' <- as.numeric(Spotify_2024_data$'energy_%')
            

# Create the plot
p <- ggplot(Spotify_2024_data, aes(x = streams, y = bpm, color = `danceability_%`, size = `energy_%`, 
                                   text = paste("Track: ", track_name, 
                                                "<br>BPM: ", bpm, 
                                                "<br>Streams: ", streams, 
                                                "<br>Danceability: ", `danceability_%`, 
                                                "<br>Energy: ", `energy_%`))) +
  geom_point(alpha = 0.5) +  # Points with reduced transparency
  scale_color_viridis_c(option = "plasma") +  # Color scale
  scale_size_continuous(range = c(2, 6)) +  # Size scale for dots
  
  # Custom formatting for the x-axis to show billions
  scale_x_continuous(labels = label_number(scale = 1e-9, suffix = "B")) +  # Show in billions
  
  labs(title = "Mapping BPM with Number of Streams, Danceability, and Energy",
       x = "Number of Streams (in billions)",
       y = "BPM",
       color = "Danceability (%)",
       size = "Energy (%)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  
  # Add the moving average line with improved visibility
  geom_line(aes(x = streams, y = moving_avg_bpm), color = "blue", size = 2) +  # Thicker line for moving average
  geom_point(aes(x = streams, y = moving_avg_bpm), color = "blue", size = 3, shape = 21, fill = "blue") +  # Points for the moving average
  
  # Optional linear trend line
  geom_smooth(method = "lm", se = FALSE, color = "orange", linetype = "solid", size = 1.5)  # Linear trend line

# Convert to interactive plotly object
interactive_plot <- ggplotly(p, tooltip = "text")

# Save the updated interactive plot
saveWidget(interactive_plot, "interactive_scatter_plot_with_moving_average.html", selfcontained = TRUE)
browseURL("interactive_scatter_plot_with_moving_average.html")


## The blue trend scatter can demonstrate that most successful spotify songs average between 100-150 BPM. ##


# Calculate mean values for BPM and Streams
mean_bpm <- mean(Spotify_2024_data$bpm, na.rm = TRUE)
mean_streams <- mean(Spotify_2024_data$streams, na.rm = TRUE)

# Assuming Spotify_2024_data has been prepared with moving_avg_bpm
# Create the plot
p <- ggplot(Spotify_2024_data, aes(x = streams, y = bpm, 
                                   color = `danceability_%`, 
                                   size = `energy_%`, 
                                   text = paste("Track: ", track_name, 
                                                "<br>BPM: ", bpm, 
                                                "<br>Streams: ", streams, 
                                                "<br>Danceability: ", `danceability_%`, 
                                                "<br>Energy: ", `energy_%`))) +
  geom_point(alpha = 0.6) +  # Slightly larger point size and increased transparency
  scale_color_viridis_c(option = "plasma", begin = 0.2, end = 0.8) +  # Color scale with adjusted range
  scale_size(range = c(2, 6), guide = "none") +  # Size scale without guide
  
  # Custom formatting for the x-axis to show billions
  scale_x_continuous(labels = label_number(scale = 1e-9, suffix = "B"), 
                     breaks = seq(0, max(Spotify_2024_data$streams), by = 0.5e9)) +  # Custom breaks
  scale_y_continuous(breaks = seq(0, max(Spotify_2024_data$bpm), by = 25)) +  # Custom breaks for BPM
  
  labs(title = "Mapping BPM with Number of Streams, Danceability, and Energy",
       x = "Number of Streams (in billions)",
       y = "BPM",
       color = "Danceability (%)",
       size = "Energy (%)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_line(color = "grey", size = 0.5),# Add grid lines
        panel.grid.minor = element_blank()) +# Optional: Remove minor grid lines
  
  # Add the moving average line with improved visibility
  geom_line(aes(x = streams, y = moving_avg_bpm), color = "blue", size = 2) +  # Thicker line for moving average
  geom_point(aes(x = streams, y = moving_avg_bpm), color = "blue", size = 3, shape = 21, fill = "blue") +  # Points for the moving average
  
  geom_hline(yintercept = mean_bpm, color = "red", linetype = "dashed", size = 1) +  # Mean BPM line
  
 
   # Optional linear trend line
  geom_smooth(method = "lm", se = FALSE, color = "orange", linetype = "solid", size = 1.5) 

# Convert to interactive plotly object
interactive_plot <- ggplotly(p, tooltip = "text")

# Save the updated interactive plot
saveWidget(interactive_plot, "interactive_scatter_plot_with_moving_average.html", selfcontained = TRUE)
browseURL("interactive_scatter_plot_with_moving_average.html")


# Extract the first artist (artists separated by commas)
Spotify_2024_data <- Spotify_2024_data %>%
  mutate(first_artist = sapply(strsplit(`artist(s)_name`, ","), `[`, 1))

# Calculate the number of unique first artists
num_unique_first_artists <- Spotify_2024_data %>%
  summarise(unique_first_artists = n_distinct(first_artist)) %>%
  pull(unique_first_artists)


num_unique_first_artists


####-------------------------------------------------------- Hierachical Models ----------------------------------------------------####

library(lme4)

# Log-transforming the streams variable
Spotify_2024_data$log_streams <- log(Spotify_2024_data$streams + 1)  # adding 1 to avoid log(0)

# Rebuild hierarchical model with log-streams
hierarchical_model <- lmer(bpm ~ log_streams + `danceability_%` + (1 | first_artist), data = Spotify_2024_data)


# Plot random effects for artists
ranef_df <- as.data.frame(ranef(hierarchical_model)$first_artist)
colnames(ranef_df) <- c("Intercept")
ranef_df$Artist <- rownames(ranef(hierarchical_model)$first_artist)

# Plot Random Effects (Artist Intercepts)
library(ggplot2)
ggplot(ranef_df, aes(x = Artist, y = Intercept)) +
  geom_point() +
  coord_flip() +  # Flipping for readability
  labs(title = "Random Intercepts by Artist", x = "Artist", y = "Intercept (BPM)") +
  theme_minimal()



# Plot model fit: predicted vs actual BPM
Spotify_2024_data$predicted_bpm <- predict(hierarchical_model)

ggplot(Spotify_2024_data, aes(x = bpm, y = predicted_bpm)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +  # Line y=x for perfect fit
  labs(title = "Actual vs Predicted BPM", x = "Actual BPM", y = "Predicted BPM") +
  theme_minimal()




# Display the summary
summary(hierarchical_model)

hierarchical_model_streams <- lmer(streams ~ bpm + `danceability_%` + (1 | first_artist), data = Spotify_2024_data)
summary(hierarchical_model_streams)

# Load the lmerTest package
library(lmerTest)

# Refit the model using lmerTest
hierarchical_model_with_p <- lmer(streams ~ bpm + `danceability_%` + (1 | first_artist), data = Spotify_2024_data)

# Summary will now include p-values
summary(hierarchical_model_with_p)


# Predict streams based on the model
Spotify_2024_data$predicted_streams <- predict(hierarchical_model_with_p)

# Plot actual vs predicted streams
ggplot(Spotify_2024_data, aes(x = streams, y = predicted_streams)) +
  geom_point(alpha = 0.5) +  # Scatter plot of actual vs predicted
  geom_abline(slope = 1, intercept = 0, color = "red") +  # Perfect fit line (y = x)
  labs(title = "Actual vs Predicted Streams", x = "Actual Streams", y = "Predicted Streams") +
  theme_minimal()

# Visualize the fixed effects using the sjPlot package
library(sjPlot)

# Plot the fixed effects
plot_model(hierarchical_model_with_p, type = "est", show.values = TRUE, value.offset = 0.3) +
  theme_minimal() +
  labs(title = "Fixed Effects: Influence of BPM and Danceability on Streams", 
       y = "Estimate", x = "Fixed Effects")

# Fit a hierarchical model with random intercepts for artists
hierarchical_model <- lmer(streams ~ bpm + `danceability_%` + (1 | first_artist), data = Spotify_2024_data)

# Summary of the model
summary(hierarchical_model)

# Compare AIC values for the lm model and the lmer mixed model
AIC(simpler_model, hierarchical_model_with_p)

# Compare BIC values
BIC(simpler_model, hierarchical_model_with_p)

# Predict streams using the model
Spotify_2024_data$predicted_streams <- predict(hierarchical_model)

# Plot actual vs predicted streams
ggplot(Spotify_2024_data, aes(x = streams, y = predicted_streams)) +
  geom_point(alpha = 0.5) +  # Scatter plot of actual vs predicted
  geom_abline(slope = 1, intercept = 0, color = "red") +  # Line y=x for perfect fit
  labs(title = "Actual vs Predicted Streams", x = "Actual Streams", y = "Predicted Streams") +
  theme_minimal()
library(sjPlot)

# Plot fixed effects estimates from the model
plot_model(hierarchical_model, type = "est", show.values = TRUE, value.offset = 0.3) +
  theme_minimal() +
  labs(title = "Fixed Effects: Influence of BPM and Danceability on Streams", 
       y = "Effect Size (Estimate)", x = "Fixed Effects")

# Extract random effects (intercepts for each artist)
ranef_df <- as.data.frame(ranef(hierarchical_model)$first_artist)
colnames(ranef_df) <- c("Intercept")
ranef_df$Artist <- rownames(ranef(hierarchical_model)$first_artist)

# Plot the random intercepts for artists
ggplot(ranef_df, aes(x = reorder(Artist, Intercept), y = Intercept)) +
  geom_point() +
  coord_flip() +  # Flip axes for readability
  labs(title = "Random Intercepts by Artist", x = "Artist", y = "Intercept (Streams)") +
  theme_minimal()


## note to make another model and compare it against the hierarchical model to prove hpothesis testing
## note to also change column names perhaps with replace