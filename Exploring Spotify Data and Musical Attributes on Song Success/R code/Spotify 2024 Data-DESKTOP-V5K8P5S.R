library(readr)
library(ggplot2)
library(plotly)
library(readr)
library(dplyr)
library(tibble)
Spotify_2024_data <- read_csv("C:/Users/joshh/Downloads/Spotify Most Streamed Songs.csv")
View(Spotify_2024_data)

## note to change the column names so they are more presentable

str(Spotify_2024_data)

Spotify_2024_data <- na.omit(Spotify_2024_data) # Removes rows with NA values

  
Spotify_2024_data$`danceability_%` <- as.numeric(Spotify_2024_data$`danceability_%`)


##------------------------- Data Wrangling ------------------------------##

unique(Spotify_2024_data$streams)


Spotify_2024_data$streams <- as.numeric(gsub(",", "", as.character(Spotify_2024_data$streams)))

sum(is.na(Spotify_2024_data$streams))

Spotify_2024_data <- na.omit(Spotify_2024_data)


Spotify_2024_data$streams <- as.numeric(gsub(",", "", Spotify_2024_data$streams))

# Extract the first artist (artists separated by commas)
Spotify_2024_data <- Spotify_2024_data %>%
  mutate(first_artist = sapply(strsplit(`artist(s)_name`, ","), `[`, 1))

# Calculate the number of unique first artists
num_unique_first_artists <- Spotify_2024_data %>%
  summarise(unique_first_artists = n_distinct(first_artist)) %>%
  pull(unique_first_artists)


num_unique_first_artists

library(zoo)

Spotify_2024_data <- Spotify_2024_data %>%
  arrange(streams) %>%
  mutate(moving_avg_bpm = zoo::rollmean(bpm, k = 10, fill = NA))

Spotify_2024_data$'energy_%' <- as.numeric(Spotify_2024_data$'energy_%')


##--------------------------- Making Tables ----------------------------##


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

valence_summary <- data.frame(
  Metric = "Valence (%)",
  Mean = mean(Spotify_2024_data$`valence_%`, na.rm = TRUE),
  Median = median(Spotify_2024_data$`valence_%`, na.rm = TRUE),
  SD = sd(Spotify_2024_data$`valence_%`, na.rm = TRUE),
  Min = min(Spotify_2024_data$`valence_%`, na.rm = TRUE),
  Max = max(Spotify_2024_data$`valence_%`, na.rm = TRUE),
  Unique_Artists = n_distinct(Spotify_2024_data$first_artist)
)

energy_summary <- data.frame(
  Metric = "Energy (%)",
  Mean = mean(Spotify_2024_data$`energy_%`, na.rm = TRUE),
  Median = median(Spotify_2024_data$`energy_%`, na.rm = TRUE),
  SD = sd(Spotify_2024_data$`energy_%`, na.rm = TRUE),
  Min = min(Spotify_2024_data$`energy_%`, na.rm = TRUE),
  Max = max(Spotify_2024_data$`energy_%`, na.rm = TRUE),
  Unique_Artists = n_distinct(Spotify_2024_data$first_artist)
)

# Combine the individual summaries into a single table
summary_table <- bind_rows(bpm_summary, streams_summary, danceability_summary, valence_summary, energy_summary)

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


# Making Colour Coded Correlation Matrix of Song Attributes
library(gt)
library(corrplot)

correlation_table <- correlation_df %>%
  rownames_to_column(var = "Metric") %>%
  mutate(across(-Metric, ~ cell_spec(signif(.x, 3),  # Round to 3 significant figures
                                     color = "white", 
                                     background = spec_color(.x, option = "D", end = 0.9)))) %>%
  knitr::kable(format = "html", escape = FALSE, 
               caption = "Colour Coded Correlation Matrix of Song Attributes") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(1, bold = TRUE)

# Display the table in an HTML viewer
correlation_table

# Create a correlation plot
corrplot(correlation_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

View(correlation_matrix)




##-------------------- Making Scatterplot graph -------------------------##


library(ggplot2)

# Create a boxplot to show the distribution of streams by key
ggplot(Spotify_2024_data, aes(x = factor(key), y = streams)) +
  geom_boxplot(aes(fill = factor(key))) +  # Boxplot with key on x-axis and streams on y-axis
  labs(title = "Song Streams by Key", x = "Key of the Song", y = "Number of Streams") +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend as it's redundant

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


## Iteration

ggplot(Spotify_2024_data, aes(x = streams, y = bpm, color = `danceability_%`)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_gradient(low = "yellow", high = "blue") + 
  labs(title = "Mapping Songs BPM with their Number of Streams and Danceability",
       x = "Number of Streams",
       y = "BPM",
       color = "Danceability (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))


##------------- Iteration for interactive scatter plot ---------------------##

Spotify_2024_data$streams <- as.numeric(gsub(",", "", as.character(Spotify_2024_data$streams)))

# Remove rows with NA values in streams
Spotify_2024_data <- na.omit(Spotify_2024_data)


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

# Create a fresh plot using the updated data with the moving_avg_bpm column
p <- ggplot(Spotify_2024_data, aes(x = streams, y = bpm, color = `danceability_%`, size = `energy_%`, 
                                   text = paste("Track: ", track_name, 
                                                "<br>BPM: ", bpm, 
                                                "<br>Streams: ", streams, 
                                                "<br>Danceability: ", `danceability_%`, 
                                                "<br>Energy: ", `energy_%`))) +
  geom_point(alpha = 0.7) +  # Plot the points
  scale_color_viridis_c(option = "plasma") +  # Color scale
  scale_size_continuous(range = c(2, 6)) +  # Size scale for dots
  
  # Updated label formatting for the x-axis
  scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_si(""))) +  
  
  labs(title = "Mapping BPM with Number of Streams, Danceability, and Energy",
       x = "Number of Streams",
       y = "BPM",
       color = "Danceability (%)",
       size = "Energy (%)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  
  # Add the moving average line
  geom_line(aes(x = streams, y = moving_avg_bpm), color = "red", size = 1.2)  # Add moving average line

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
  scale_y_continuous(breaks = seq(min(Spotify_2024_data$bpm), max(Spotify_2024_data$bpm), by = 10)) +  # Custom breaks for BPM
  
  labs(title = "Mapping BPM with Number of Streams, Danceability, and Energy",
       x = "Number of Streams (in billions)",
       y = "BPM",
       color = "Danceability (%)",
       size = "Energy (%)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_line(color = "grey", size = 0.5),  # Add grid lines
        panel.grid.minor = element_blank()) +  # Optional: Remove minor grid lines
  
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




####-------------------------- Hierachical Models -------------------------####

library(lme4)

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

# Plot the relationship between log(streams) and bpm
ggplot(Spotify_2024_data, aes(x = log_streams, y = bpm)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "BPM vs Log(Streams)", x = "Log(Streams)", y = "BPM") +
  theme_minimal()

# Plot model fit: predicted vs actual BPM
Spotify_2024_data$predicted_bpm <- predict(hierarchical_model)

ggplot(Spotify_2024_data, aes(x = bpm, y = predicted_bpm)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +  # Line y=x for perfect fit
  labs(title = "Actual vs Predicted BPM", x = "Actual BPM", y = "Predicted BPM") +
  theme_minimal()


# Log-transforming the streams variable
Spotify_2024_data$log_streams <- log(Spotify_2024_data$streams + 1)  # adding 1 to avoid log(0)

# Rebuild hierarchical model with log-streams
hierarchical_model <- lmer(bpm ~ log_streams + `danceability_%` + (1 | first_artist), data = Spotify_2024_data)

# Display the summary
summary(hierarchical_model)

hierarchical_model_streams <- lmer(streams ~ bpm + `danceability_%` + (1 | first_artist), data = Spotify_2024_data)
summary(hierarchical_model_streams)

# Load the lmerTest package
library(lmerTest)

# Refit the model using lmerTest
hierarchical_model_with_p <- lmer(streams ~ bpm + `danceability_%` + (1 | first_artist), 
                                  data = Spotify_2024_data)

# Summary will now include p-values
summary(hierarchical_model_with_p)

library(broom.mixed)
library(kableExtra)

# Tidy the hierarchical model output
tidy_model <- tidy(hierarchical_model_streams)

# Select and format columns for readability
tidy_model_table <- tidy_model %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    Term = term,
    Estimate = estimate,
    `Std. Error` = std.error,
    `t value` = statistic,
    `P-value` = p.value
  ) %>%
  # Apply significant figure formatting to 4 digits
  mutate(across(c(Estimate, `Std. Error`, `t value`, `P-value`), ~ signif(.x, 4)))

# Create a formatted table with kableExtra
tidy_model_table %>%
  knitr::kable("html", escape = FALSE, caption = "Hierarchical Model Summary") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(1, bold = TRUE) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#4CAF50") %>%  # Style header
  add_header_above(c(" " = 1, "Model Summary Statistics" = 4))

#### - fitting more models for hierarchical modelling with stream - ####

# Remove NA values from Spotify_2024_data and then fit the model
Spotify_2024_data <- na.omit(Spotify_2024_data)

# Fit the model again with the cleaned data if needed
hierarchical_model_streams <- lmer(streams ~ bpm + `danceability_%` + (1 | first_artist), data = Spotify_2024_data)

# Ensure predictions are made only on rows used in the model
Spotify_2024_data <- Spotify_2024_data[!is.na(Spotify_2024_data$bpm) & !is.na(Spotify_2024_data$`danceability_%`), ]

# Assign predicted values
Spotify_2024_data$predicted_streams <- predict(hierarchical_model_streams, newdata = Spotify_2024_data)

library(ggplot2)

# Plot actual vs predicted streams with formatted axes in billions
ggplot(Spotify_2024_data, aes(x = streams, y = predicted_streams)) +
  geom_point(alpha = 0.5) +  # Scatter plot of actual vs predicted
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Line y=x for perfect fit
  scale_x_continuous(
    labels = label_number(scale = 1e-9, suffix = "B"),  # Convert to billions with "B" suffix
    breaks = pretty_breaks(n = 5)  # Set approximately 5 breaks on the x-axis
  ) +
  scale_y_continuous(
    labels = label_number(scale = 1e-9, suffix = "B"),  # Convert to billions with "B" suffix
    breaks = pretty_breaks(n = 5)  # Set approximately 5 breaks on the y-axis
  ) +
  labs(
    title = "Actual vs Predicted Streams", 
    x = "Actual Streams (Billions)", 
    y = "Predicted Streams (Billions)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),  # Adds padding to the x-axis title
    axis.title.y = element_text(margin = margin(r = 10))   # Adds padding to the y-axis title
  )


## Fitting a simple linear model and comparing it against the hierarchical model ##

# Simple linear model without random effects
simple_model_streams <- lm(streams ~ bpm + `danceability_%`, data = Spotify_2024_data)

# Hierarchical model with random intercepts for artists
hierarchical_model_streams <- lmer(streams ~ bpm + `danceability_%` + (1 | first_artist), data = Spotify_2024_data)

# Compare AIC and BIC values for both models
AIC(simple_model_streams, hierarchical_model_streams)
BIC(simple_model_streams, hierarchical_model_streams)

library(performance)

# Calculate R² values for the hierarchical model
r2_hierarchical <- r2_nakagawa(hierarchical_model_streams)

# Calculate R² for the simple linear model
r2_simple <- summary(simple_model_streams)$r.squared

# Display R² values
r2_hierarchical  # Marginal and Conditional R² for the hierarchical model
r2_simple  # R² for the simple linear model

# Create a summary table for AIC, BIC, and R² values
comparison_table <- data.frame(
  AIC = c(AIC(simple_model_streams), AIC(hierarchical_model_streams)),
  BIC = c(BIC(simple_model_streams), BIC(hierarchical_model_streams)),
  `R² (Marginal)` = c(r2_simple, r2_hierarchical$R2_marginal),
  `R² (Conditional)` = c(NA, r2_hierarchical$R2_conditional)
)

# Rename columns for neat presentation
colnames(comparison_table) <- c("AIC", "BIC", "R² (Marginal)", "R² (Conditional)")

# Format the table with kableExtra
library(knitr)
library(kableExtra)

comparison_table %>%
  knitr::kable("html", escape = FALSE, caption = "Model Comparison for Streams") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#4CAF50") %>%
  add_header_above(c("Model Comparison Statistics" = 5)) %>%
  column_spec(3:4, bold = TRUE, width = "8em") %>%  # Optional width adjustment
  row_spec(1:2, color = "black")  # Adjust row color for readability

## Both models predicted vs actual side by side ##


# Remove rows with missing values in relevant columns
Spotify_2024_data_clean <- Spotify_2024_data %>%
  filter(!is.na(streams) & !is.na(bpm) & !is.na(`danceability_%`))

# Fit models with the cleaned data
simple_model_streams <- lm(streams ~ bpm + `danceability_%`, data = Spotify_2024_data_clean)
hierarchical_model_streams <- lmer(streams ~ bpm + `danceability_%` + (1 | first_artist), data = Spotify_2024_data_clean)

# Make predictions with the cleaned dataset
Spotify_2024_data_clean$predicted_simple <- predict(simple_model_streams)
Spotify_2024_data_clean$predicted_hierarchical <- predict(hierarchical_model_streams)

# Ensure predictions are made only for rows used in the model
Spotify_2024_data$predicted_simple <- predict(simple_model_streams, newdata = Spotify_2024_data)
Spotify_2024_data$predicted_hierarchical <- predict(hierarchical_model_streams, newdata = Spotify_2024_data)

# Simple Model Plot with Billions on Axes
plot_simple <- ggplot(Spotify_2024_data, aes(x = streams / 1e9, y = predicted_simple / 1e9)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  scale_x_continuous(labels = label_number(scale = 1, suffix = "B")) +
  scale_y_continuous(labels = label_number(scale = 1, suffix = "B")) +
  labs(title = "Simple Model: Actual vs Predicted", x = "Actual Streams (Billions)", y = "Predicted Streams (Billions)") +
  theme_minimal()

# Hierarchical Model Plot with Billions on Axes
plot_hierarchical <- ggplot(Spotify_2024_data, aes(x = streams / 1e9, y = predicted_hierarchical / 1e9)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  scale_x_continuous(labels = label_number(scale = 1, suffix = "B")) +
  scale_y_continuous(labels = label_number(scale = 1, suffix = "B")) +
  labs(title = "Hierarchical Model: Actual vs Predicted", x = "Actual Streams (Billions)", y = "Predicted Streams (Billions)") +
  theme_minimal()

# Calculate residuals for both models
Spotify_2024_data$residuals_simple <- Spotify_2024_data$streams - Spotify_2024_data$predicted_simple
Spotify_2024_data$residuals_hierarchical <- Spotify_2024_data$streams - Spotify_2024_data$predicted_hierarchical

# Plot residuals for Simple Model with improved x-axis readability
residuals_simple_plot <- ggplot(Spotify_2024_data, aes(x = residuals_simple / 1e6)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  scale_x_continuous(labels = label_number(scale = 1, suffix = "M"), breaks = pretty_breaks(n = 5)) +
  labs(title = "Residuals for Simple Model", x = "Residuals (Millions)", y = "Count") +
  theme_minimal()

# Plot residuals for Hierarchical Model with improved x-axis readability
residuals_hierarchical_plot <- ggplot(Spotify_2024_data, aes(x = residuals_hierarchical / 1e6)) +
  geom_histogram(bins = 30, fill = "green", alpha = 0.7) +
  scale_x_continuous(labels = label_number(scale = 1, suffix = "M"), breaks = pretty_breaks(n = 5)) +
  labs(title = "Residuals for Hierarchical Model", x = "Residuals (Millions)", y = "Count") +
  theme_minimal()

# Arrange all plots in a grid of 2x2
grid.arrange(plot_simple, plot_hierarchical, residuals_simple_plot, residuals_hierarchical_plot, ncol = 2)

# Coefficient Comparison
simple_model_tidy <- tidy(simple_model_streams)
hierarchical_model_tidy <- tidy(hierarchical_model_streams)

# Add model labels
simple_model_tidy$model <- "Simple Model"
hierarchical_model_tidy$model <- "Hierarchical Model"

# Combine data
combined_tidy <- rbind(simple_model_tidy, hierarchical_model_tidy)

# Plot effect sizes side by side
ggplot(combined_tidy, aes(x = term, y = estimate, color = model)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2, position = position_dodge(width = 0.5)) +
  labs(title = "Coefficient Comparison: Simple vs Hierarchical Model", x = "Predictors", y = "Estimate") +
  theme_minimal()
