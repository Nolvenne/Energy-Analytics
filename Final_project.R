#Name: Leama Tah & Chikka Gowthami

install.packages('fpp3') #for tsibble
install.packages("tsibble")
#install.packages("latex2exp")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("fable")
install.packages("readxl")
#install.packages("writexl")
install.packages("feasts")
install.packages("forecast")
#install.packages("ggfortify")
install.packages("neuralnet")


#load libraries
library(fpp3)
#library(ggfortify)
library(tsibble)
library(ggplot2)
library(fabletools)
library(fable)
#library(latex2exp)
library(MASS)
library(dplyr)
library(readxl)
library(lubridate)
#library(writexl)
library(feasts)
library(forecast)
library(neuralnet)


# Step 1: Load the dataset
# Load the dataset from Excel
Elec_Data <- read_excel("/Users/meme/Desktop/EA_Project/Electricty_Turkey.xlsx")
# View the dataset structure and summary
head(Elec_Data)
str(Elec_Data)

# Step 2: Add proper Date sequence
# Create a sequence of months from January 1976 to December 2010
date_sequence <- seq(from = as.Date("1976-01-01"), to = as.Date("2010-12-01"), by = "month")

# Add this sequence as a new column to the datas et.
Elec_Data$Date <- date_sequence

# Remove the existing 'Date (Monthly)' column to avoid confusion
Elec_Data$`Date (Montly)`<- NULL

head(Elec_Data)#verification

# Step 3: Rename columns for clarity
Elec_Data <- Elec_Data %>%
  rename(
    Gross_Income = `Gross Income`,
    Load_hourly = `Load (hourly) Mwh`,
    Immediate_load = `Immediate load Mwh`,
    Import = `Import Gwh`,
    Export = `Export Gwh`,
    Gross_Production_GWH = `Gross Production GWH`,
    Transmitted_energy_GWH = `Transmitted energy GWH`,
    Net_Electricity_Consumption = `Net Electricity Consumption Kwh`,
    Gross_Demand_KWh = `T.C. Elektricity consumption (Gross Demand) Kwh`,
    Lost_Electricity_KWh = `Lost Electricty Kwh`
  )
# View the dataset after renaming 
head(Elec_Data)

# Step 4: Check and handle missing values
# Check for missing values
if (any(is.na(Elec_Data))) {
  print("Handling missing values")
  Elec_Data <- na.omit(Elec_Data)  # Removes rows with NA values
}

#transform the data set into tibble
data_E <- Elec_Data |>
  mutate(Month = yearmonth(Date)) |>
  as_tsibble(index = Month)

head(data_E)

# Step 5: Exploratory Data Analysis (EDA)
# Plot Gross Production over time
autoplot(data_E, Gross_Production_GWH) +
  labs(title = "Turkey Gross_Production in Gwh",
       subtitle = "year 1976 to 2010",
       y = "in Kwh")

# Compute and visualize the correlation matrix
cor_matrix <- cor(data_E[, sapply(data_E, is.numeric)], use = "complete.obs")
corrplot::corrplot(cor_matrix, method = "circle", type = "upper", tl.cex = 0.8)

# Analyze the distribution of Gross Production
ggplot(data_E, aes(x = Gross_Production_GWH)) +
  geom_histogram(binwidth = 50, fill = "steelblue", color = "black", alpha = 0.8) +
  geom_density(aes(y = ..count.. * 50), color = "red", size = 1) +  # Add density curve
  labs(
    title = "Distribution of Gross Production",
    subtitle = "Histogram with Density Overlay",
    x = "Gross Production (GWH)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center title
    plot.subtitle = element_text(hjust = 0.5, face = "italic"),  # Center subtitle
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Boxplot to identify outliers
ggplot(data_E, aes(y = Gross_Production_GWH)) +
  geom_boxplot(fill = "orange", color = "black", outlier.color = "red", outlier.size = 2) +
  labs(
    title = "Boxplot of Gross Production",
    subtitle = "Visualizing Distribution and Outliers",
    y = "Gross Production (GWH)",
    x = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_blank(),  # Remove x-axis labels for aesthetics
    axis.ticks.x = element_blank()  # Remove x-axis ticks for aesthetics
  )

#check missing data 

any(is.na(data_E))


#STL decomposition
E_decomp<-data_E|>
  model(stl=STL(Gross_Production_GWH ~ season(period = 12), robust = TRUE)) |>
  components()
E_decomp|>autoplot()
# Extract components
trend <- E_decomp$time.series[, "trend"]
seasonal <- E_decomp$time.series[, "seasonal"]
residual <-E_decomp$time.series[, "remainder"]
# Plot the components
autoplot(data_E, series = "Original") +
  autolayer(trend, series = "Trend", color = "blue") +
  autolayer(seasonal, series = "Seasonal", color = "red") +
  autolayer(residual, series = "Residuals", color = "green") +
  labs(title = "STL Components", x = "Time", y = "Gross Production (GWH)") +
  theme_minimal()



# Step 6: Train-Test Split
train_ratio <- 0.8
train_size <- floor(train_ratio * nrow(data_E))  
train_data <- data_E[1:train_size, ]  # First portion for training
test_data <- data_E[(train_size + 1):nrow(data_E), ]  # Remaining portion for testing
# Check the split
nrow(train_data)  # Number of rows in training set
nrow(test_data)   # Number of rows in testing set


#Model 1: ARIMA 
#let chech is the data is stationary?
train_data |>
  gg_tsdisplay(Gross_Production_GWH, plot_type='partial')
#we will do first difference to make it stationary
train_data |>
  gg_tsdisplay(difference(Gross_Production_GWH), plot_type='partial')

#choose the model best AMRIMA model
fit<-train_data|>
  model(
  arima012011 = ARIMA(Gross_Production_GWH~ pdq(0,1,2) + PDQ(0,1,1)),
  arima210011 = ARIMA(Gross_Production_GWH ~ pdq(2,1,0) + PDQ(0,1,1)),
  auto = ARIMA(Gross_Production_GWH, stepwise = FALSE, approx = FALSE)
)
  
fit|> pivot_longer(everything(), names_to = "Model name",
                        values_to = "Orders")

glance(fit) |> arrange(AICc) |> select(.model:BIC)

fit|>select(auto)|>gg_tsresiduals(lag=36)

augment(fit) |>
  filter(.model == "auto") |>
  features(.innov, ljung_box, lag=24, dof=4)

#forecast on the test dataset
Arima_forecast<-forecast(fit, h="10 years") |>
  filter(.model=='auto') 

Arima_forecast|>autoplot(test_data) +
  labs(title = "forecast: Turkey Gross_Production",
       y="in GWH")

#chech model accuraty 
arima_model<-fit$auto

accuracy_metrics <- accuracy(Arima_forecast, test_data)
print(accuracy_metrics)



#Model 2: ETS
fit_ETS <- train_data|>
  model(`Holt's method` = ETS(Gross_Production_GWH ~ error("A") +
                                trend("A") + season("N")))

fc <- fit_ETS |>
  forecast(h = "10 years")

#forecast with test data
fc |>
  autoplot(test_data) +
  geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(fit_ETS)) +
  labs(y="in GWH", title="ETS: forecast of Turkey Gross_Production") +
  guides(colour = "none")

#check accuratly
accuracy_ETS <- accuracy(fc, test_data)
print(accuracy_ETS)


#Model 3: Naive Drift

# Fit the models
Drift_fit <- train_data|>
  model(
    Drift = NAIVE(Gross_Production_GWH ~ drift())
  )

#forecast for the next 10 years
D_fc<-Drift_fit|>
  forecast(h = "10 years")

D_fc|>
  autoplot(test_data) +
  geom_line(aes(y = .fitted), colour = "black",
            data = augment(fit_ETS)) +
  labs(y="in GWH", title="NAIVE_Drift: forecast of Turkey Gross_Production") +
  guides(colour = "none")

#check accuracy
accuracy_Drift <- accuracy(D_fc, test_data)
print(accuracy_Drift)


#model 4: ANN

# Normalize function to scale data
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Load the dataset and normalize numeric columns
numeric_columns <- c(
  "Gross_Income", "Population", "Load_hourly", "Immediate_load", 
  "Import", "Export", "Gross_Production_GWH", "Transmitted_energy_GWH", 
  "Net_Electricity_Consumption", "Gross_Demand_KWh", "Lost_Electricity_KWh"
)

# Normalize numeric columns only
data_norm <- data_E
data_norm[numeric_columns] <- as.data.frame(lapply(data_E[numeric_columns], normalize))

# Split dataset into training and testing sets
set.seed(123)  # For reproducibility
n <- nrow(data_norm)
train_indices <- sample(1:n, size = 0.8 * n)
train_data <- data_norm[train_indices, ]
test_data <- data_norm[-train_indices, ]

# Define the formula for the neural network
formula <- as.formula("Gross_Production_GWH~Net_Electricity_Consumption+Gross_Income + Population + Load_hourly + Immediate_load +
                       Import + Export + Transmitted_energy_GWH + Gross_Demand_KWh + Lost_Electricity_KWh")

# Train the neural network
nn_model <- neuralnet(
  formula = formula,
  data = train_data,
  hidden = c(5, 3),  # Two hidden layers with 5 and 3 neurons
  linear.output = TRUE,  # Use linear output for regression
  stepmax = 1e6  # Maximum training steps
)

# Plot the trained network
plot(nn_model)

# Prepare test features
test_features <- test_data[, c(
  "Gross_Income", "Population", "Load_hourly", "Immediate_load", 
  "Import", "Export", "Gross_Production_GWH", "Transmitted_energy_GWH", 
  "Gross_Demand_KWh", "Lost_Electricity_KWh"
)]

# Ensure test_features is a data frame
test_features <- as.data.frame(test_features)

# Make predictions
predictions <- compute(nn_model, test_features)$net.result
print(predictions)
# Evaluate performance
actual <- test_data$Gross_Production_GWH
# Evaluate metrics
mse <- mean((actual - predictions)^2)
mae <- mean(abs(actual - predictions))
mape <- mean(abs((actual - predictions) / actual)) * 100
msle <- mean((log1p(actual) - log1p(predictions))^2)

# Combine metrics into a data frame
metrics_df <- data.frame(
  Metric_1 = c("MSE", "MAE", "MAPE", "MSLE"),
  Value_1 = c(mse, mae, mape, msle)
)
# Print the metrics data frame
print(metrics_df)

# Add model identifiers to each accuracy output
accuracy_metrics <- accuracy_metrics %>% mutate(Model = "ARIMA")
accuracy_ETS <- accuracy_ETS |>mutate(Model = "ETS")
accuracy_Drift <- accuracy_Drift |> mutate(Model = "Drift")

# Combine the data frames for ARIMA, ETS, and Drift
combined_metrics <- bind_rows(accuracy_metrics, accuracy_ETS, accuracy_Drift)

# Add ANN metrics
metrics_df <- metrics_df |>
  mutate(
    Model = "ANN",              # Specify the model
    .type = "Test",             # Specify the data type
    RMSE = ifelse(Metric_1 == "MSE", sqrt(Value_1), NA),  # Compute RMSE from MSE
    MAE = ifelse(Metric_1 == "MAE", Value_1, NA),
    MAPE = ifelse(Metric_1 == "MAPE", Value_1, NA)
  )

# Add ANN metrics to the combined table
comparison_table <- bind_rows(combined_metrics, metrics_df)

# Print the updated comparison table
print(comparison_table)

#visually compare model performance
# Reshape the data to long format
comparison_long <- comparison_table |>
  pivot_longer(cols = c(RMSE, MAE, MAPE), names_to = "Metric", values_to = "Value")

# Create the bar plot
ggplot(comparison_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(
    title = "Model Performance Comparison",
    x = "Model",
    y = "Metric Value",
    fill = "Metric"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

