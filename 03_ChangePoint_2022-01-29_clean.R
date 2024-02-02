## ======================================================================================
## CHANGE POINT ANALYSIS
## ======================================================================================

#group up by age band and gender
change_pt_data <- 
  aggregate(cbind(expected_death_60mth, Deaths, Population_Gender) ~ COUNTY_NAME + Year_Month, data = CA_Data, FUN = sum)
change_pt_data$Excess_Death_Rate = ((change_pt_data$Deaths - change_pt_data$expected_death_60mth) / change_pt_data$Population_Gender) * 1000

change_pt_data_pm2.5 <- aggregate(cbind(max_pm2.5) ~ COUNTY_NAME + Year_Month, data = CA_Data, FUN = mean)

# Use SQL for data manipulations to speed things up

change_point_mort_0 <- sqldf("SELECT Year_Month, COUNTY_NAME, avg(Excess_Death_Rate) AS mean 
               FROM change_pt_data 
               GROUP BY Year_Month, COUNTY_NAME")
change_point_mort <- acast(change_point_mort_0, Year_Month ~ COUNTY_NAME, fill = 0, value.var = "mean")

change_point_pm2.5 <- acast(change_pt_data_pm2.5, Year_Month ~ COUNTY_NAME, fill = 0, value.var = "max_pm2.5")

e.divisive(change_point_mort, sig.lvl = 0.1, R = 9, k = NULL, min.size = 2 , alpha = 1)
write.csv(change_point_mort, "change_point_mort.csv")

e.divisive(change_point_pm2.5, sig.lvl = 0.1, R = 9, k = NULL, min.size = 2 , alpha = 1)
write.csv(change_point_pm2.5, "change_point_pm2.5.csv")
