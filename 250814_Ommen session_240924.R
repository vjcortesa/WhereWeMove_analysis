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
library(ggpattern)
library(shiny)

# 0. Naming conventions
#f, filtered
#s, selection
#ave, average
#_plt plot
#formatted#ds dataset

# 0. Questions to answer with the dataset
Question_id <- c(1, 2, 3)
Question <- c("How did players spend their money in average?", 
                 "Which measures did players choose at every round?",
                 "How did welfare type affect their choices?")
Plotype <- c("IncomeDistribution", 
                 "Measuresdist_welfare",
                 "Measuresdist_round")

dataanalysis <- data.frame(
  Question_id,
  Question,
  Plotype
)

# 1. Reading the tables to retrieve the playerround data
gameversion <- read_csv("250814_housinggame-tables\\gameversion.csv")
gamesession <- read_csv("250814_housinggame-tables\\gamesession.csv")
group <- read_csv("250814_housinggame-tables\\group.csv")
groupround <- read_csv("250814_housinggame-tables\\groupround.csv")
playerround <- read_csv("250814_housinggame-tables\\playerround.csv")
welfaretype <- read_csv("250814_housinggame-tables\\welfaretype.csv")
scenario <- read_csv("250814_housinggame-tables\\scenario.csv")
player <- read_csv("250814_housinggame-tables\\player.csv")

# 1a. Creating datasets for the dynamic filtering of the app (not a priority now)
fgamesession <- gamesession %>% select(id,name)
fgroup <- group %>% select(id,name,gamesession_id)
fplayer <- player %>% select(id,code,group_id)

# 2. Add the necessary variables per table to plot per player, table and per session averages
## per session average, adding the name, remaned to the group to avoid name overlap
gamesession <- gamesession %>% rename(gamesession_name = name)
group <- group %>%
  inner_join(gamesession %>% select(id, gamesession_name), by = c("gamesession_id" = "id"))
## per table average, adding the group name to the groupround
groupround <- groupround %>% 
  inner_join(group %>% select(id, name, gamesession_id, gamesession_name), by = c("group_id" = "id"))
## renamed name in groupround for variable consistency
groupround <- groupround %>% rename(group_name = name)
#added to playerround all the variables to change datasets per round, group and per session averages
playerround <- playerround %>%
  inner_join(groupround %>% select(id, round_number, group_id, group_name, gamesession_id, gamesession_name), by = c("groupround_id" = "id"))
##added to playerround the player code to change datasets per player
playerround <- playerround %>% 
  inner_join(player %>% select(id, code), by = c("player_id" = "id"))
##added to playerround the player round for the plots that need it

# 3. Filtering the playerround dataset to analyse
## filtering criteria
 ds_gameversion_name <- "Game Version 2024-09 English"
 ds_gamesession_name <-"Ommen 24-09-2024"
## get relational ids of the dataset
 ds_gameversion_id<-gameversion$id[gameversion$name==ds_gameversion_name]
 ds_gamesession_id<-gamesession$id[gamesession$gamesession_name==ds_gamesession_name]
 ds_scenario_id<-scenario$id[scenario$gameversion_id==ds_gameversion_id]
 
##filter tables
 f_playerround <- playerround %>% filter(gamesession_name == ds_gamesession_name)
 #   inner_join(fs_groupround, by = "id")
 
# 4. Filtering the playerround dataset for the income distribution plot
 ## dataset variables relevant for the income distribution plot
 income_dist_var <- c("id", "player_id","code", "groupround_id", "round_number", 
                         "round_income", "living_costs", "cost_taxes", "mortgage_payment",
                         "cost_measures_bought", "cost_satisfaction_bought",
                         "cost_fluvial_damage", "cost_pluvial_damage",
                         "spendable_income",
                         "paid_debt")
 ## Select those columns from the filtered dataset
 income_dist <- f_playerround %>% select(all_of(income_dist_var))
 

# 5. Make a function to build the plot

plot_income_dist <- function(Question_id, question_name, variables, dataset, session_name, group_name, player_name, round) {
  # Plot title definition
  plot_title <- dataanalysis$Question[dataanalysis$Question_id ==Question_id]
  plot_subtitle <- paste("Session:", session_name, "Group:", group_name, "Player:", player_name, "Round_", round)
  plot_name <- paste("IncomeDistribution_","Session_",session_name, "Group_", group_name, "Player_", player_name,"Round_", round,".png")
    # Define dataset based on input criteria
  
  if (session_name != "all") {
      dataset <- dataset %>% filter(gamesession_name == session_name)
  }
  # Select variables columns from the filtered dataset
  dataset <- dataset %>% select(all_of(variables))
  dataset$income_minus_living <- dataset$round_income - dataset$living_costs
  view(dataset)
  if (group_name != "all") {
   dataset <- dataset %>% filter(group_name == group_name)
  }
  #Reference dataset to draw area and line
  income_dist_plt_ref <- dataset %>%
    group_by(round_income) %>% 
    summarise(
      ave_income_minus_living = round(mean(income_minus_living, na.rm = TRUE), 2),
      ave_Spendable = round(mean(spendable_income, na.rm = TRUE), 2)
    )
  if (player_name != "all") {
     dataset <- dataset %>% filter(code == player_name)
   }
  view(dataset)
  income_dist <- dataset
  # Calculate the mean values per dataset variable
  income_dist_plt <- income_dist %>%
    group_by(round_income) %>%
    summarise(
      ave_income_minus_living = round(mean(income_minus_living, na.rm = TRUE), 2),
      ave_Spendable = round(mean(spendable_income, na.rm = TRUE), 2),
      ave_mortgage = round(mean(mortgage_payment, na.rm = TRUE), 2),
      ave_taxes = round(mean(cost_taxes, na.rm = TRUE), 2),
      ave_debt = round(mean(paid_debt, na.rm = TRUE), 2),
      ave_measures = round(mean(cost_measures_bought, na.rm = TRUE), 2),
      ave_satisfaction = round(mean(cost_satisfaction_bought, na.rm = TRUE), 2),
      ave_fluvial_damage  = round(mean(cost_fluvial_damage, na.rm = TRUE), 2),
      ave_pluvial_damage = round(mean(cost_pluvial_damage, na.rm = TRUE), 2),
    ) %>%
    ungroup()
  
  View(income_dist)

  # Categorise the income distribution per plot category
  line_spendable = income_dist_plt_ref %>% select(ave_Spendable)
  bars_expenses <- income_dist_plt %>% select(ave_debt, ave_mortgage, ave_taxes, ave_satisfaction, ave_measures, ave_fluvial_damage, ave_pluvial_damage)
  area_income <- income_dist_plt_ref %>% select(ave_income_minus_living)
  
  # Adding an index to plot the area and bars together
  line_spendable$Index <- seq_len(nrow(line_spendable))
  bars_expenses$Index <- seq_len(nrow(bars_expenses))
  area_income$Index <- seq_len(nrow(area_income))
  
  # Set x range of the plot
  # Calculate limits
  x_min <- min(area_income$Index) -0.5 #starts from zero
  x_max <- max(area_income$Index) + 0.5
  w = 0.9
  # Formatting the dataset to plot per category
  bars_expenses_formatted <- bars_expenses %>%
    pivot_longer(cols = -Index, names_to = "Type", values_to = "Value")
  
  area_income_formatted <- area_income %>%
    pivot_longer(cols = -Index, names_to = "Type", values_to = "Value")
  View(area_income_formatted)
  
  # Formatting the dataset to stack the bars following the given order
  bars_expenses_formatted$Type <- factor(
    bars_expenses_formatted$Type,
    levels = c(
      "ave_satisfaction",
      "ave_fluvial_damage",
      "ave_pluvial_damage",
      "ave_measures",
      "ave_debt",
      "ave_taxes",
      "ave_mortgage"
    )
  )
  plot <- ggplot() +
    geom_area(data = area_income_formatted,
              aes(x = Index, y = Value, fill = Type),
              alpha = 0.6
    ) +
    geom_bar(data = bars_expenses_formatted,
             aes(x = Index, y = Value, fill = Type),
             stat = "identity",
             position = "stack",
             width = w
    ) +
    geom_line(
      data = line_spendable,
      aes(x = Index,
          y = ave_Spendable,
          color = "ave_Spendable"),
      size = 1.2) +
    labs(
            title = plot_title,
            subtitle = plot_subtitle,
            color = "Category"
            ) +
    # Custom fill colors to what is plotted in the legend
    scale_color_manual(
      name = "Round Spendable \n Income",
      values = c(
        "ave_Spendable" = "black"),
      labels = c(
        "ave_Spendable" = "Round income - costs")
      ) +
    scale_fill_manual(
      name = "Round Budget",
      values = c(
        "ave_income_minus_living" = "#E1BB70",
        "ave_debt" = "black",
        "ave_satisfaction" = "#dfaba3",
        "ave_measures" = "white",
        "ave_mortgage" = "#cccccc",
        "ave_taxes" = "#dddddd",
        "ave_fluvial_damage" = "#79A2C5",
        "ave_pluvial_damage" = "#79BCC5"),
      labels = c(
        "ave_income_minus_living" = "Income - Living costs",
        "ave_debt" = "Start savings (+) / debt (-)",
        "ave_satisfaction" = "Satisfaction costs",
        "ave_measures" = "Measures costs",
        "ave_mortgage" = "Mortgage costs",
        "ave_taxes" = "Taxes costs",
        "ave_fluvial_damage" = "River damage costs",
        "ave_pluvial_damage" = "Rain damage costs")
      ) +
    #Y-axis formatting
    scale_y_continuous(
      labels = function(y) y / 1000,
      name = "Game Currency (k)"
    ) +
    scale_x_continuous(
      name = "Welfare Classes",
      breaks = c(1, 2, 3, 4, 5, 6),
      labels = c("Very Low", "Low", "Low-average", "High-average", "High", "Very High")
      #limits = c(x_min, x_max)
    ) +
    theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            plot.title.position = "plot"
            )
    print(plot)
    ggsave(plot_name, width = 12, height = 6, dpi = 300)
  return(plot)
}  
#plot_income_dist <- function(Question_id, question_name, variables, dataset, 
#                             session_name, group, player, round)
question_plt <- dataanalysis$Question[dataanalysis$Question_id ==Question_id]
plot_income_dist (1,question_plt, income_dist_var, f_playerround, 
                  f_playerround$gamesession_name[1], "all", "all", "all")

plot_income_dist (1,question_plt, income_dist_var, f_playerround, 
                   f_playerround$gamesession_name[1], "table1", "all", "all")
 
plot_income_dist (1,question_plt, income_dist_var, f_playerround, 
                   f_playerround$gamesession_name[1], "table1", "t1p1", "all")
 
# f_playerround_group_player_round <- f_playerround_group_player %>% filter(round_number == 0)
# plot_income_dist (1,question_plt, income_dist_var, f_playerround_group_player, 
#                   f_playerround$gamesession_name[1], "table1", "t1p1", 0)



