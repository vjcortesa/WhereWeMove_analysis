# Load necessary libraries
library(readxl)
library(readr)
# Load dplyr for data manipulation
library(dplyr)
# data plot
library(ggplot2)
library(tidyr)
library(tibble)
library(purrr)
library(stringr)
library(forcats)
library(tidyverse)

# 1. Reading the original dataset
game <- read_excel("Game data.xlsx")
write_csv(game, "Game data.csv")
game <- read_csv("Game data.csv")

# Extracting the headers of the original dataset for comparison
column_headers <- colnames(game)

# 2. Analysing how players distribute their average income among the game choices

# 2.1 Define filtered range for comparison with the analysis from the Capita Selecta
lower_bound <- 17
upper_bound <- 24

# Drop rows where PlayerID is between lower_bound and upper_bound (exclusive upper bound)
game_filtered <- game[!(game$PlayerID > lower_bound & game$PlayerID < upper_bound), ]

# Define the fields to include
income_dist_fields <- c("PlayerID", "Round", "Income", "LivingCost", "IncreaseSatisfaction",
                        "Savings", "SpendableIncome", "PayingDebt", "PayingSatisfaction", "PayingMeasures")

# Select those columns from the filtered dataset
income_dist_data <- game_filtered[, income_dist_fields]
# Replace zeros with NA in numeric columns only
income_dist_data_wz <- income_dist_data
numeric_cols <- sapply(income_dist_data_wz, is.numeric)
income_dist_data_wz[numeric_cols] <- lapply(income_dist_data_wz[numeric_cols], function(x) ifelse(x == 0, NA, x))

# 2.2 Preparing the dataset for calculations
# 2.2.1 Calculate satisfaction costs in euros (not in points)
income_dist_data_wz$PayingSatisfactionCosts <- income_dist_data_wz$PayingSatisfaction * income_dist_data_wz$IncreaseSatisfaction

# 2.2.2 Convert "nan" strings to NA
income_dist_data[income_dist_data == "nan"] <- NA

# 2.3 Calculate the mean values for the complete dataset
income_dist_plt <- income_dist_data %>%
  group_by(Income) %>%
  summarise(
    Ave_LivingCost = round(mean(LivingCost, na.rm = TRUE), 2),
    Ave_Savings = round(mean(Savings, na.rm = TRUE), 2),
    Ave_Spendable = round(mean(SpendableIncome, na.rm = TRUE), 2),
    Ave_PayigdDebt = round(mean(PayingDebt, na.rm = TRUE), 2),
    Ave_PayingSat = round(mean(PayingSatisfaction, na.rm = TRUE), 2),
    Ave_PayingMes = round(mean(PayingMeasures, na.rm = TRUE), 2)
  ) %>%
  ungroup()
# 2.4 Filtering the dataset
area_income <- income_dist_plt %>% select(Income, Ave_Spendable)
bars_expenses <- income_dist_plt %>% select(Ave_LivingCost, Ave_PayigdDebt, Ave_PayingSat, Ave_PayingMes)
line_savings <- income_dist_plt %>% select(Income, Ave_Savings)
# 2.4.1 Plotting the income as an area chart
area_income_long <- area_income %>%
  pivot_longer(cols = -Income, names_to = "Type", values_to = "Value")
# 2.4.2 Preparing expenses for stacked bar
bars_expenses_long <- income_dist_plt %>%
  select(Income, Ave_LivingCost, Ave_PayigdDebt, Ave_PayingSat, Ave_PayingMes) %>%
  pivot_longer(cols = -Income, names_to = "ExpenseType", values_to = "ExpenseValue")
# 2.4.3 Combine savings line

line_savings <- income_dist_plt %>% select(Income, Ave_Savings)
 
ggplot() +
  # Area plot for income
  +     geom_area(data = area_income_long, aes(x = Income, y = Value, fill = Type), alpha = 0.5, position = "identity") +
  # Line plot for savings
  + geom_line(data = line_savings, aes(x = Income, y = Ave_Savings), color = "#B3A484", size = 1.2) +
  # Stacked bar for expenses
  + geom_bar(data = bars_expenses_long, aes(x = Income, y = ExpenseValue, fill = ExpenseType), stat = "identity", position = "stack") +
  # Customizing the plot
  scale_fill_manual(values = c(
    "Ave_Spendable" = "#E1BB70",
    "Income" = "#e0cca4",
    "Ave_LivingCost" = "#cccccc",
    "Ave_PayigdDebt" = "#9B9A9A",
    "Ave_PayingSat" = "#dfaba3",
    "Ave_PayingMes" = "#9dd6db"
  )) +
  labs(
    title = "Average player income distribution among game choices",
    x = "Welfare classes",
    y = "Game Currency (k)",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
# Save the plot
ggsave("IncomeDistribution.png", width = 12, height = 6, dpi = 300)

 

