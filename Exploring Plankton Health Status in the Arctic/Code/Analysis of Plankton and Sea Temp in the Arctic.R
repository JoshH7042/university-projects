library(dplyr)
library(ggplot2)
# Install if not already installed
if (!require(scales)) install.packages("scales")
if (!require(gridExtra)) install.packages("gridExtra")

# Load libraries

library(scales)
library(gridExtra)


##Plankton and Sea Temperature Analysis  

plankton_data <- read.csv("C:/Users/joshh/OneDrive - University of Exeter/MSc Applied Data Science and Statistics/MTHM507 - Commicating Data Science/Plankton DATA.csv", 
                          header = TRUE, 
                          stringsAsFactors = FALSE)


sea_temp_data <- read.csv("C:\\Users\\joshh\\OneDrive - University of Exeter\\MSc Applied Data Science and Statistics\\MTHM507 - Commicating Data Science\\sea Temperature DATA.csv", 
                          header = TRUE, 
                          stringsAsFactors = FALSE)

# List all objects in the environment
ls()

# Check the first few rows to confirm data was loaded
head(plankton_data)
head(sea_temp_data)

colnames(plankton_data)
colnames(sea_temp_data)


arctic_sea_temp <- dplyr::select(sea_temp_data, X, TA2)
arctic_plankton <- dplyr::select(plankton_data, PA2Date, PA2)

# Check the first few rows
head(arctic_plankton)
head(arctic_sea_temp)

# Check the structure of the data
str(arctic_plankton)
str(arctic_sea_temp)

# Check column names
colnames(arctic_plankton)

# Rename the columns
arctic_plankton <- arctic_plankton %>% 
  rename(Date = PA2Date, Plankton = PA2)

# Confirm the renaming
colnames(arctic_plankton)

# Check the unique values in the Plankton column
unique(arctic_plankton$Plankton)

# Remove rows with NA values in Plankton
arctic_plankton <- arctic_plankton %>% filter(!is.na(Plankton))

# Confirm the cleaning
sum(is.na(arctic_plankton$Plankton))

# Check structure and first few rows
str(arctic_plankton)
head(arctic_plankton)

# Convert Date to Date format
arctic_plankton$Date <- as.Date(arctic_plankton$Date, format="%d/%m/%Y")

# Confirm the conversion
str(arctic_plankton)
head(arctic_plankton)


arctic_plankton$Plankton <- as.numeric(arctic_plankton$Plankton)

# Plot Arctic Plankton Data
ggplot(arctic_plankton, aes(x = Date, y = Plankton)) +
  geom_line(color = "blue") +
  labs(title = "Arctic Plankton Chlorophyll (mg/m3) Changes Over Time",
       x = "Date",
       y = "Plankton Intensity (CHL-a)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


arctic_sea_temp$Date <- as.Date(arctic_sea_temp$Date, format="%d/%m/%Y")

colnames(arctic_sea_temp)
str(arctic_sea_temp)

# Remove the first row which contains headers
arctic_sea_temp <- arctic_sea_temp[-1, ]

# Confirm the removal
head(arctic_sea_temp)

arctic_sea_temp <- arctic_sea_temp %>% 
  rename(Date = X, SeaTemp = TA2)
colnames(arctic_sea_temp)

arctic_sea_temp$Date <- as.Date(arctic_sea_temp$Date, format="%d/%m/%Y")

arctic_sea_temp$SeaTemp <- as.numeric(arctic_sea_temp$SeaTemp)

sum(is.na(arctic_sea_temp$SeaTemp))

ggplot(arctic_sea_temp, aes(x = Date, y = SeaTemp)) +
  geom_line(color = "red") +
  labs(title = "Arctic Sea Temperature (TA2) Over Time",
       x = "Date",
       y = "Sea Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Merge Data for Combined Visualization
arctic_data <- merge(arctic_plankton, arctic_sea_temp, by = "Date")

# Plot Combined Arctic Data
ggplot(arctic_data, aes(x = Date)) +
  geom_line(aes(y = Plankton, color = "Plankton Intensity (CHL-a)")) +
  geom_line(aes(y = SeaTemp, color = "Sea Temperature (°C)")) +
  labs(title = "Arctic Plankton and Sea Temperature Over Time",
       x = "Date",
       y = "Value") +
  scale_color_manual(name = "Legend", values = c("blue", "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plot with Dual Y-Axis and Smoothing
ggplot() +
  # Sea Temperature Line and Trend
  geom_line(data = arctic_data, aes(x = Date, y = SeaTemp), color = "red", alpha = 0.5) +
  geom_smooth(data = arctic_data, aes(x = Date, y = SeaTemp), method = "loess", color = "darkred", se = FALSE, linetype = "dashed") +
  scale_y_continuous(name = "Sea Temperature (°C)", sec.axis = sec_axis(~ ., name = "Plankton Intensity (CHL-a)")) +
  
  # Plankton Line and Trend (Scaled)
  geom_line(data = arctic_data, aes(x = Date, y = Plankton * 3), color = "blue", alpha = 0.5) +
  geom_smooth(data = arctic_data, aes(x = Date, y = Plankton * 3), method = "loess", color = "darkblue", se = FALSE, linetype = "dashed") +
  
  labs(title = "Trends in Arctic Plankton Intensity and Sea Temperature",
       x = "Date") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  scale_x_date(labels = date_format("%Y"), breaks = "2 years") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



# Plankton Trend Plot
p1 <- ggplot(arctic_data, aes(x = Date, y = Plankton)) +
  geom_line(color = "blue", alpha = 0.5) +
  geom_smooth(method = "loess", color = "darkblue", se = FALSE, linetype = "dashed") +
  labs(title = "Trend in Arctic Plankton Intensity (CHL-a)",
       x = "Date",
       y = "Plankton Intensity (CHL-a)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

# Sea Temperature Trend Plot
p2 <- ggplot(arctic_data, aes(x = Date, y = SeaTemp)) +
  geom_line(color = "red", alpha = 0.5) +
  geom_smooth(method = "loess", color = "darkred", se = FALSE, linetype = "dashed") +
  labs(title = "Trend in Arctic Sea Temperature (°C)",
       x = "Date",
       y = "Sea Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

# Arrange both plots side by side
grid.arrange(p1, p2, ncol = 2)


# Plot Arctic Plankton Data with Enhanced Analysis
ggplot(arctic_data, aes(x = Date, y = Plankton)) +
  geom_line(color = "blue", alpha = 0.5, size = 0.6) +
  geom_smooth(method = "loess", color = "darkblue", linetype = "dashed", size = 1, se = FALSE) +
  geom_smooth(method = "lm", color = "black", linetype = "solid", size = 0.8, se = FALSE) +
  labs(title = "Arctic Plankton Intensity (CHL-a) Trend",
       subtitle = "LOESS Smoothing (Dashed) and Linear Trend (Solid)",
       x = "Date",
       y = "Plankton Intensity (CHL-a)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic")) +
  scale_x_date(labels = date_format("%Y"), breaks = "2 years") +
  annotate("text", x = as.Date("2015-01-01"), y = 2.5, label = "No Significant Increase", color = "black", size = 4)


# Plot Arctic Sea Temperature Data with Enhanced Analysis
ggplot(arctic_data, aes(x = Date, y = SeaTemp)) +
  geom_line(color = "red", alpha = 0.5, size = 0.6) +
  geom_smooth(method = "loess", color = "darkred", linetype = "dashed", size = 1, se = FALSE) +
  geom_smooth(method = "lm", color = "black", linetype = "solid", size = 0.8, se = FALSE) +
  labs(title = "Arctic Sea Temperature Trend",
       subtitle = "LOESS Smoothing (Dashed) and Linear Trend (Solid)",
       x = "Date",
       y = "Sea Temperature (°C)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic")) +
  scale_x_date(labels = date_format("%Y"), breaks = "2 years") +
  annotate("text", x = as.Date("2015-01-01"), y = 6.5, label = "Gradual Warming", color = "black", size = 4)


# Run Granger Causality Test from SeaTemp to Plankton
granger_test_SeaTemp_to_Plant <- grangertest(Plankton ~ SeaTemp, order = 10, data = arctic_data)

# Run Granger Causality Test from Plankton to SeaTemp
granger_test_Plant_to_SeaTemp <- grangertest(SeaTemp ~ Plankton, order = 10, data = arctic_data)

# View results
print(granger_test_SeaTemp_to_Plant)
print(granger_test_Plant_to_SeaTemp)

# Create a data frame for the Granger causality results
granger_results <- data.frame(
  Test_Direction = c("SeaTemp to Plant", "Plant to SeaTemp"),
  Model_1 = c("Plankton ~ Lags(Plankton, 1:10) + Lags(SeaTemp, 1:10)",
              "SeaTemp ~ Lags(SeaTemp, 1:10) + Lags(Plankton, 1:10)"),
  Model_2 = c("Plankton ~ Lags(Plankton, 1:10)",
              "SeaTemp ~ Lags(SeaTemp, 1:10)"),
  F_Statistic = c(-10, -10),
  P_Value = c("5.0407 × 10^-6", "4.5243 × 10^-6"),
  stringsAsFactors = FALSE
)

# View the table
print(granger_results)

# Load the knitr package for kable
library(knitr)

# Create the data frame
granger_results <- data.frame(
  Test_Direction = c("SeaTemp to Plant", "Plant to SeaTemp"),
  Model_1 = c("Plankton ~ Lags(Plankton, 1:10) + Lags(SeaTemp, 1:10)",
              "SeaTemp ~ Lags(SeaTemp, 1:10) + Lags(Plankton, 1:10)"),
  Model_2 = c("Plankton ~ Lags(Plankton, 1:10)",
              "SeaTemp ~ Lags(SeaTemp, 1:10)"),
  F_Statistic = c(-10, -10),
  P_Value = c("5.0407 × 10^-6", "4.5243 × 10^-6"),
  stringsAsFactors = FALSE
)

# Print the kable table
kable(granger_results, format = "markdown", caption = "Granger Causality Test Results")
# Load the gt package for better table styling
library(gt)

# Create the data frame
granger_results <- data.frame(
  Test_Direction = c("SeaTemp to Plant", "Plant to SeaTemp"),
  Model_1 = c("Plankton ~ Lags(Plankton, 1:10) + Lags(SeaTemp, 1:10)",
              "SeaTemp ~ Lags(SeaTemp, 1:10) + Lags(Plankton, 1:10)"),
  Model_2 = c("Plankton ~ Lags(Plankton, 1:10)",
              "SeaTemp ~ Lags(SeaTemp, 1:10)"),
  F_Statistic = c(-10, -10),
  P_Value = c("5.0407 × 10^-6", "4.5243 × 10^-6"),
  stringsAsFactors = FALSE
)

# Create and style the gt table
granger_results %>%
  gt() %>%
  tab_header(
    title = "Granger Causality Test Results"
  ) %>%
  cols_label(
    Test_Direction = "Test Direction",
    Model_1 = "Model 1",
    Model_2 = "Model 2",
    F_Statistic = "F Statistic",
    P_Value = "P Value"
  )
