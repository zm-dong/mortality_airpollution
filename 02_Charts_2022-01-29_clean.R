## ======================================================================================
## Diagrams for analysis
## ======================================================================================

# Figure 1: Plot of minimum, maximum, and average PM2.5 concentrations aggregated by year, for all counties in California
plot_data <- melt(CA_Data[, c(4, 6:8)], id.vars = "Year")
plot_data <- rename(plot_data, PM2.5 = value)
plot_data %>%
  group_by(Year, variable) %>%
  summarise(annual_avg_pm2.5 = mean(PM2.5, na.rm=TRUE),
            sd = sd(PM2.5, na.rm=TRUE)) %>%
  ggplot(aes(x=Year, y=annual_avg_pm2.5, col=variable, group = variable)) +
  geom_line() +
  geom_errorbar(aes(ymin=annual_avg_pm2.5 - sd, ymax =annual_avg_pm2.5 + sd), width = 0.1, alpha = 0.5) +
  theme(legend.position = "bottom", aspect.ratio=15/35, axis.text.x = element_text(angle = 90, hjust = 1, size = 9, vjust = 0.5, margin = margin(2.5, 5, 5, 5))) +
  labs(y="Annual Average PM2.5 Concentrations", x="Year", colour = "Legend") + 
  scale_colour_discrete(name = "Metric", labels = c("Average PM2.5", "Maximum PM2.5", "Minimum PM2.5"))

# Figure 2: Relationship between PM2.5 concentration readings and wildfire events: San Diego County Only
San_Diego_Only <- CA_Data %>%
  filter(COUNTY_NAME == "San Diego") %>%
  group_by(Year) %>%
  summarise(Average_PM2.5 = mean(avg_pm2.5, na.rm=TRUE),
            Minimum_PM2.5 = mean(min_pm2.5, na.rm=TRUE),
            Maximum_PM2.5 = mean(max_pm2.5, na.rm=TRUE),
            #Deadly_Wildfire_Indicator = sum(Deadliest.Indicator, na.rm=TRUE),
            #Destructive_Wildfire_Indicator = sum(Destructive.Indicator, na.rm=TRUE),
            Largest_Wildfire_Indicator = sum(Largest.Indicator, na.rm=TRUE))

San_Deigo_Plot <- melt(San_Diego_Only[, c(1:5)], id="Year")
San_Deigo_Plot %>%
  group_by(Year, variable) %>%
  ggplot(aes(x=Year, y=value, col=variable, group=variable)) +
  geom_line() +
  theme(legend.position = "bottom", aspect.ratio=15/35, axis.text.x = element_text(angle = 90, hjust = 1, size = 9, vjust = 0.5, margin = margin(2.5, 5, 5, 5))) +
  labs(y="PM2.5 Concentrations / \n Count of Large Wildfires", x="Year", colour = "Legend") +
  scale_colour_discrete(name = "Metric", labels = c("Average PM2.5", "Minimum PM2.5", "Maximum PM2.5", "Largest Wildfire Indicator"))

# Figure 4: Plot of T90 and T10 for the Southwest Pacific Region over the period 1990 to 2019
plot_data_temp <- melt(CA_Data[, c(4, 26:27)], id="Year")
plot_data_temp <- rename(plot_data_temp, TemperatureIndex = value)
plot_data_temp %>%
  group_by(Year, variable) %>%
  summarise(mean_temp_index = mean(TemperatureIndex, na.rm = TRUE),
            sd = sd(TemperatureIndex, na.rm = TRUE)) %>%
  ggplot(aes(x=Year, y=mean_temp_index, col=variable, group=variable)) +
  geom_line() +
  geom_errorbar(aes(ymin=mean_temp_index - sd, ymax =mean_temp_index + sd), width = 0.1, alpha = 0.5) +
  scale_color_manual(values=c("blue", "red")) +
  theme(legend.position = "bottom", aspect.ratio=15/35, axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(y="Mean Temperature Index", x="Year", colour = "Legend")

# Figure 5: Death rate by year: actual and expected
plot_data2 <- melt(CA_Data[, c(5, 27, 29)], id="Year_Month")
plot_data2 %>%
  group_by(Year_Month, variable) %>%
  summarise(Average_Mortality = mean(value, na.rm=TRUE)) %>%
  ggplot(aes(x=Year_Month, y=Average_Mortality, col=variable, group=variable)) +
  geom_line()+
  theme(legend.position = "bottom", aspect.ratio=15/35, axis.text.x = element_text(angle = 90, hjust = 1, size = 9, vjust = 0.5, margin = margin(2.5, 5, 5, 5))) +
  labs(y="Average Death Rate per mille", x="Year and Month", colour = "Legend") +
  scale_colour_discrete(name = "Death Rate", labels = c("Actual", "Expected"))

# Figure 6: GAM partial plots
plot.gam(mod_gam4, select = 1, rug = TRUE, shade = TRUE, shade.col = "lightblue", scale = 0, xlab = "Max PM2.5 (t-1)", ylab = "")
plot.gam(mod_gam4, select = 2, rug = TRUE, shade = TRUE, shade.col = "lightblue", scale = 0, xlab = "Excess Death Rate (t-1)", ylab = "")
plot.gam(mod_gam4, select = 3, rug = TRUE, shade = TRUE, shade.col = "lightblue", scale = 0, xlab = "Low Temperature Extremes Index", ylab = "")
plot.gam(mod_gam4, select = 4, rug = TRUE, shade = TRUE, shade.col = "lightblue", scale = 0, xlab = "High Temperature Extreme Index", ylab = "")
plot.gam(mod_gam4, select = 5, rug = TRUE, shade = TRUE, shade.col = "lightblue", scale = 0, xlab = "Five Year Age Band", ylab = "")
plot.gam(mod_gam4, select = 6, rug = TRUE, shade = TRUE, shade.col = "lightblue", scale = 0, xlab = "County", ylab = "")
plot.gam(mod_gam4, select = 7, rug = TRUE, shade = TRUE, shade.col = "lightblue", scale = 0, xlab = "Year and Month", ylab = "")
