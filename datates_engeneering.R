rm(list = ls())

shots_data <- read.csv("csv_file/clean_data.csv")

shots_data

library(dplyr)             # Load the package
df_grouped <- group_by(shots_data, player)

df_grouped$bodypart


df_summary <- summarize(df_grouped, pref_foot = names(which.max(table(bodypart))))
df <- mutate(shots_data, prefered_type = df_summary$pref_foot[match(shots_data$player, df_summary$player)])


