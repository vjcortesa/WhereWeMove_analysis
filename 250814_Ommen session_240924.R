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
library(grid)
library(png)

# 0. Naming conventions
#f, filtered
#s, selection
#ave, average
#_plt plot
#formatted#ds dataset

# Questions to answer from the game session collected data
Question_id <- c(1, 2, 3)
Question <- c("How did players spend their money in average?", 
                 "Which measures did players choose at every round?",
                 "How did welfare type affect their choices?")
# Plots to create for answering those questions
Plotype <- c("IncomeDistribution", 
                 "Measuresdist_welfare",
                 "Measuresdist_round")

dataanalysis <- data.frame(
  Question_id,
  Question,
  Plotype
)

# 1. Read the database table to create accordingly dataframes
gameversion <- read_csv("250814_housinggame-tables\\gameversion.csv")
gamesession <- read_csv("250814_housinggame-tables\\gamesession.csv")
group <- read_csv("250814_housinggame-tables\\group.csv")
groupround <- read_csv("250814_housinggame-tables\\groupround.csv")
playerround <- read_csv("250814_housinggame-tables\\playerround.csv")
welfaretype <- read_csv("250814_housinggame-tables\\welfaretype.csv")
scenario <- read_csv("250814_housinggame-tables\\scenario.csv")
player <- read_csv("250814_housinggame-tables\\player.csv")

# 1a. Create filtered dataframes for the dynamic selection of the session, group and player to plot in the app (not a priority now)
fgamesession <- gamesession %>% select(id,name)
fgroup <- group %>% select(id,name,gamesession_id)
fplayer <- player %>% select(id,code,group_id)

# 2. Add the variables to plot per session, group and player
## Add the session name variable remaned to avoid name overlap with the group name variable
gamesession <- gamesession %>% rename(gamesession_name = name)
## Merge the gamesession selection into the group by the group = gamesession id
## Inner_join keeps only the rows that have matching values in both data frames
group <- group %>%
  inner_join(gamesession %>% select(id, gamesession_name), by = c("gamesession_id" = "id"))
## Merge the group selection into the groupround by the groupround = group id
groupround <- groupround %>% 
  inner_join(group %>% select(id, name, gamesession_id, gamesession_name), by = c("group_id" = "id"))
## Rename name variable in groupround for variable naming consistency
groupround <- groupround %>% rename(group_name = name)
## Add to playerround the groupround selection to filter per round, group and session id and names by playerround = groupround id
playerround <- playerround %>%
  inner_join(groupround %>% select(id, round_number, group_id, group_name, gamesession_id, gamesession_name), by = c("groupround_id" = "id"))
## Add to playerround the player code to filter datasets per player by playerround = player id
playerround <- playerround %>% 
  inner_join(player %>% select(id, code), by = c("player_id" = "id"))

# 3. Filter the playerround dataframe to analyse the last gamesession in Ommen
## Filter criteria
 ds_gameversion_name <- "Game Version 2024-09 English"
 ds_gamesession_name <-"Ommen 24-09-2024"
## Get relational ids of the dataset, needed? to delete probably
 ds_gameversion_id<-gameversion$id[gameversion$name==ds_gameversion_name]
 ds_gamesession_id<-gamesession$id[gamesession$gamesession_name==ds_gamesession_name]
 ds_scenario_id<-scenario$id[scenario$gameversion_id==ds_gameversion_id]
 
##filter tables
 f_playerround <- playerround %>% filter(gamesession_name == ds_gamesession_name)

# 4. Filter the playerround dataset for the income distribution plot
 ## variables selection for the income distribution plot
 income_dist_var <- c("id", "player_id","code", "groupround_id", "round_number", 
                         "round_income", "living_costs", "paid_debt",
                         "profit_sold_house","spent_savings_for_buying_house",
                         "cost_taxes", "mortgage_payment",
                         "cost_measures_bought", "cost_satisfaction_bought",
                         "cost_fluvial_damage", "cost_pluvial_damage",
                         "spendable_income"
                         )
 ## Select those columns from the filtered playerround 
 income_dist <- f_playerround %>% select(all_of(income_dist_var))
 ## Sort per player code (ascending)
 income_dist <- arrange(income_dist, code)
 
# 5. Calculate the round costs to check the spendable income coding in  the gameplay website
 # "paid_debt" not used in the calculations because is taken already when the spendable income comes as a negative value
 #If either column has NA, the sum will also be NA unless the sum is done this way
 income_dist$calculated_costs <- rowSums(income_dist[, c("living_costs", 
                                                        "cost_taxes",
                                                        "spent_savings_for_buying_house",
                                                        "mortgage_payment",
                                                        "cost_measures_bought",
                                                        "cost_satisfaction_bought",
                                                        "cost_fluvial_damage",
                                                        "cost_pluvial_damage"
                                        )], na.rm = TRUE)
# 6. Calculate the spendable income
 income_dist$calculated_spendable <-  income_dist$spendable_income
 for (i in 1:nrow(income_dist)) {
   if (income_dist$round_number[i] != "0") {
     income_dist$calculated_spendable[i] <- sum(income_dist$calculated_spendable[i-1],
                                                income_dist$round_income[i],
                                                income_dist$profit_sold_house[i],
                                                -income_dist$calculated_costs[i],
                                                na.rm = TRUE)   }
 } 
 # I do not expect NA values as spendable income has a starting value according to the welfare type in round 0
 income_dist$difference_spendable <- income_dist$spendable_income - income_dist$calculated_spendable
 
 # 7. Filter the playerround dataset for the satisfaction distribution plot
   ## variables selection for the satisfaction distribution plot
   satisfaction_dist_var <- c("id", "player_id","code", "groupround_id", "round_number",
                              "satisfaction_move_penalty", "satisfaction_fluvial_penalty", "satisfaction_pluvial_penalty", "satisfaction_debt_penalty",
                              "satisfaction_house_rating_delta", "satisfaction_house_measures", "satisfaction_personal_measures", "satisfaction_total"
   )
   ## Select those columns from the filtered playerround 
   satisfaction_dist <- f_playerround %>% select(all_of(satisfaction_dist_var))
   ## Sort per player code (ascending)
   satisfaction_dist <- arrange(satisfaction_dist, code)
 # 8. Calculate the satisfaction total to check the coding in  the gameplay website
   #If either column has NA, the sum will also be NA unless the sum is done this way
   #na.rm = TRUE remove or ignore NA (missing) values when performing calculations
   satisfaction_dist$calculated_penalties <- rowSums(satisfaction_dist[, c("satisfaction_move_penalty", 
                                                                           "satisfaction_pluvial_penalty",
                                                                           "satisfaction_fluvial_penalty",
                                                                           "satisfaction_debt_penalty"
                                                                           )], na.rm = TRUE)
   
 # 9. Calculate the satisfaction total
   satisfaction_dist$calculated_satisfaction <-  satisfaction_dist$satisfaction_total
   for (i in 1:nrow(satisfaction_dist)) {
      if (satisfaction_dist$round_number[i] != "0") {
        satisfaction_dist$calculated_satisfaction[i] <- sum(satisfaction_dist$satisfaction_total[i-1],
                                                            satisfaction_dist$satisfaction_house_rating_delta[i],
                                                            satisfaction_dist$satisfaction_house_measures[i],
                                                            satisfaction_dist$satisfaction_personal_measures[i],
                                                            - satisfaction_dist$calculated_penalties[i],
                                                   na.rm = TRUE)
      }
    } 
   satisfaction_dist$difference_total <- satisfaction_dist$satisfaction_total - satisfaction_dist$calculated_satisfaction
 # 10. Make a function to build the plot
   # plot_player <- data.frame(code = c("t1p1", "t1p2", "t1p3", "t1p4", "t1p5", "t1p6" , "t1p7"),
   #                           selected = c(1, 0, 1, 0, 0, 0, 0))
plot_income_dist <- function(Question_id, question_name, variables, dataset, session_name, group_name, round, players, rounds) {
  # a1) Filtering the dataset based on input criteria and variables to plot
  if (session_name != "all") {
    dataset <- dataset %>% filter(gamesession_name == session_name)
  }
  if (group_name != "all") {
    dataset <- dataset %>% filter(group_name == group_name)
  }
  if (round != "all") {
    dataset <- dataset %>% filter(round_number == round)
    print(paste("Round_", round))
  }
  dataset <- dataset %>% select(all_of(variables))
  # a2) Calculating the reference dataset with all players average
  ## mapply safely substracts ingnoring NAs in either column 
  ## na.rm = TRUE remove or ignore NA (missing) values when performing calculations.
  dataset$income_minus_living <- mapply(
    function(income, cost) sum(income, -cost, na.rm = TRUE),
    dataset$round_income,
    dataset$living_costs
  )
  dataset$profit_minus_spent_savings_house_moving <- mapply(
    function(profit, spent) sum(profit, -spent, na.rm = TRUE),
    dataset$profit_sold_house,
    dataset$spent_savings_for_buying_house
  )
  ##Reference dataset to draw area and line
  income_dist_plt_ref <- dataset %>%
    group_by(round_income) %>% 
    summarise(
      ave_income_minus_living = round(mean(income_minus_living, na.rm = TRUE), 2),
      ave_Spendable = round(mean(spendable_income, na.rm = TRUE), 2)
    )
  #a3) Filtering the dataset with all players plot
  player_plot <- ""
  for (i in 1:nrow(players)) {
    if (players$selected[i] != "0") {
      if (nchar(player_plot) == 0) {
        player_plot <- players$code[i]
        nplayer <- 1
        fdataset <- dataset %>% filter(code == players$code[i])
      } else {
        player_plot <- paste(player_plot, "-", players$code[i])
        nplayer <- nplayer +1
        fdataset <- rbind(fdataset, dataset[dataset$code == players$code[i], ])
      }
    }
  }
  if (nplayer == nrow(players)) {
    player_plot = "all"
  }
  #Filter the dataset according to the player(s) to plot
  income_dist <- fdataset
  view(income_dist)
  # a4) Plot title definition
  plot_title <- dataanalysis$Question[dataanalysis$Question_id ==Question_id]
  plot_subtitle <- paste("Session:", session_name, "Group:", group_name, "\nPlayer(s):", player_plot, "\nRound:", round)
  plot_name <- paste("IncomeDistribution_","Session_",session_name, "Group_", group_name, "Player_", player_plot,"Round_", round,".png")
  # Calculate the mean values per dataset variable
  income_dist_plt <- income_dist %>%
    group_by(round_income) %>%
    summarise(
      ave_income_minus_living = round(mean(income_minus_living, na.rm = TRUE), 2),
      ave_profit_minus_spent_savings_house_moving = round(mean(profit_minus_spent_savings_house_moving, na.rm = TRUE), 2),
      ave_mortgage = round(mean(mortgage_payment, na.rm = TRUE), 2),
      ave_taxes = round(mean(cost_taxes, na.rm = TRUE), 2),
      ave_debt = round(mean(paid_debt, na.rm = TRUE), 2),
      ave_measures = round(mean(cost_measures_bought, na.rm = TRUE), 2),
      ave_satisfaction = round(mean(cost_satisfaction_bought, na.rm = TRUE), 2),
      ave_fluvial_damage  = round(mean(cost_fluvial_damage, na.rm = TRUE), 2),
      ave_pluvial_damage = round(mean(cost_pluvial_damage, na.rm = TRUE), 2),
      ave_Spendable = round(mean(spendable_income, na.rm = TRUE), 2)
    ) %>%
    ungroup()
  
  # Categorise the income distribution per plot category
  line_spendable = income_dist_plt_ref %>% select(ave_Spendable)
  bars_expenses <- income_dist_plt %>% select(ave_debt, ave_mortgage, ave_taxes, ave_profit_minus_spent_savings_house_moving, ave_satisfaction, ave_measures, ave_fluvial_damage, ave_pluvial_damage)
  area_income <- income_dist_plt_ref %>% select(ave_income_minus_living)
  view(line_spendable)
  view(bars_expenses)
  view(area_income)
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
      "ave_mortgage",
      "ave_profit_minus_spent_savings_house_moving"
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
        "ave_profit_minus_spent_savings_house_moving" =  "#a3a3a3",
        "ave_mortgage" = "#cccccc",
        "ave_taxes" = "#dddddd",
        "ave_fluvial_damage" = "#79A2C5",
        "ave_pluvial_damage" = "#79BCC5"),
      labels = c(
        "ave_income_minus_living" = "Income - Living costs",
        "ave_debt" = "Debt costs",
        "ave_satisfaction" = "Satisfaction costs",
        "ave_measures" = "Measures costs",
        "ave_mortgage" = "Mortgage costs",
        "ave_profit_minus_spent_savings_house_moving" = "House transaction \nprofit (+) /spent savings(-)",
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
  player_icon <- readPNG("icons/Player.png")
  player_grob <- rasterGrob(player_icon, width = unit(1, "cm"), height = unit(1, "cm"))
  plot + annotation_custom(player_grob, xmin = 2.5, xmax = 2.5, ymin = -50, ymax = -10)
  
  
    print(plot)
    ggsave(plot_name, width = 12, height = 6, dpi = 300)
  return(plot)
}  
#plot_income_dist <- function(Question_id, question_name, variables, dataset, 
#                             session_name, group, player, round)
question_plt <- dataanalysis$Question[dataanalysis$Question_id ==Question_id]
#Filter the dataset per group name and make sorted lists for the codes, round and income values
f_playerround_list <- f_playerround %>%
  group_by(group_name) %>%
  arrange(group_name) %>%
  reframe(code = lapply(list(unique(code)), sort),
          round_number = lapply(list(unique(round_number)), sort),
          round_income = lapply(list(unique(round_income)), sort))

# Extract numeric part and sort the group_name
f_playerround_list_group_name <- f_playerround_list$group_name %>%
  tibble(name = .) %>%
  mutate(num = str_extract(name, "\\d+") %>% as.numeric()) %>%
  arrange(num) %>%
  pull(name)

#data frame with all groups available
f_playerround_list_group_name <- data.frame(group_name = unlist(f_playerround_list_group_name), stringsAsFactors = FALSE)
# Add the all option at the beginning of the group list
f_playerround_list_group_name <- rbind("all", f_playerround_list_group_name)
# for (i in 1:nrow(f_playerround_list_group_name)) {
#   row <- f_playerround_list_group_name[i,1]
#   row_list <- f_playerround_list %>% filter(group_name == row)
#   row_list_code <- data.frame(group_name = unlist(row_list$code), stringsAsFactors = FALSE)
#   df_zeros <- data.frame(selected = rep(0, nrow(row_list_code)))
#   row_list_code <- rbind(row_list_code, df_zeros)
#   
#   row_list_code$selected <- numeric(0)
# }
#data frame with all players available per table

plot_player <- data.frame(code = c("t1p1", "t1p2", "t1p3", "t1p4", "t1p5", "t1p6" , "t1p7"),
                          selected = c(1, 0, 1, 0, 0, 0, 0))
plot_round <- data.frame(round_number = c("0", "1", "2", "3"))

 plot_income_dist (1,question_plt, income_dist_var, f_playerround,
                   f_playerround$gamesession_name[1], "all", "all", plot_player)

 plot_income_dist (1,question_plt, income_dist_var, f_playerround,
                    f_playerround$gamesession_name[1], "table1", "all", plot_player)

 plot_income_dist (1,question_plt, income_dist_var, f_playerround,
                   f_playerround$gamesession_name[1], "table1", "all", plot_player)
 # plot_income_dist (1,question_plt, income_dist_var, f_playerround,
 #                   f_playerround$gamesession_name[1], "table1", "0", plot_player)
 # plot_income_dist (1,question_plt, income_dist_var, f_playerround,
 #                   f_playerround$gamesession_name[1], "table1", "1", plot_player)
 # plot_income_dist (1,question_plt, income_dist_var, f_playerround,
 #                   f_playerround$gamesession_name[1], "table1", "2", plot_player)
 # plot_income_dist (1,question_plt, income_dist_var, f_playerround,
 #                   f_playerround$gamesession_name[1], "table1", "3", plot_player)