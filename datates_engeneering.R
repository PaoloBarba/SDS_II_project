rm(list = ls())
library(dplyr)             # Load the package
shots_data <- read.csv("csv_file/clean_data.csv")
df_grouped <- group_by(shots_data, player)
df_summary <- summarize(df_grouped, pref_foot = names(which.max(table(bodypart))))
shots <- mutate(shots_data, prefered_type = df_summary$pref_foot[match(shots_data$player, df_summary$player)])
save(shots , file = "shots.RData")

count_shot <- summarize(df_grouped, count_shot =  length(player) )


