##### Separate file for hierarchical model #####

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
  row_spec(0, bold = TRUE, color = "white", background = "#8DA0CB") %>%  # Style header
  add_header_above(c(" " = 1, "Model Summary Statistics" = 4))

# Tidy the hierarchical model output
tidy_hierarchical <- tidy(hierarchical_model_streams)

# Select and rename columns for readability
tidy_hierarchical_table <- tidy_hierarchical %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    Term = term,
    Estimate = estimate,
    `Std. Error` = std.error,
    `t value` = statistic,
    `P-value` = p.value
  )%>%
mutate(across(c(Estimate, `Std. Error`, `t value`, `P-value`), ~ signif(.x, 4)))


# Highlight p-value and estimate for danceability
tidy_hierarchical_table <- tidy_hierarchical_table %>%
  mutate(
    Estimate = ifelse(Term == "`danceability_%`", cell_spec(Estimate, color = "blue", bold = TRUE), Estimate),
    `P-value` = ifelse(Term == "`danceability_%`", cell_spec(`P-value`, color = "red", bold = TRUE), `P-value`)
  )

# Create a formatted table with kableExtra
tidy_hierarchical_table %>%
  knitr::kable("html", escape = FALSE, caption = "Hierarchical Model Summary") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(1, bold = TRUE) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#6BAED6")  # Header styling

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
aic_values <- c(AIC(simple_model_streams), AIC(hierarchical_model_streams))
bic_values <- c(BIC(simple_model_streams), BIC(hierarchical_model_streams))

library(performance)

# Calculate R² values for the hierarchical model
r2_hierarchical <- r2_nakagawa(hierarchical_model_streams)

# Calculate R² for the simple linear model
r2_simple <- summary(simple_model_streams)$r.squared

# Create a summary table for AIC, BIC, and R² values
comparison_table <- data.frame(
  Model = c("Simple Model", "Hierarchical Model"),
  AIC = aic_values,
  BIC = bic_values,
  `R² (Marginal)` = c(r2_simple, r2_hierarchical$R2_marginal),
  `R² (Conditional)` = c(NA, r2_hierarchical$R2_conditional)
)

# Rename columns for neat presentation
colnames(comparison_table) <- c("Model", "AIC", "BIC", "R² (Marginal)", "R² (Conditional)")

# Format the table with kableExtra
library(knitr)
library(kableExtra)
library(gridExtra)
library(scales)

comparison_table %>%
  knitr::kable("html", escape = FALSE, caption = "Model Comparison for Streams") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#6BAED6") %>%
  add_header_above(c("Model Comparison Statistics" = 6)) %>%  # Change to 6 to match the columns
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
