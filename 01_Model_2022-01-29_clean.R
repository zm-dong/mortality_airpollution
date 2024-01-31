## ======================================================================================
## LIBRARIES
## ======================================================================================
rm(list=ls())
packages <- c("data.table", "dplyr", "tidyverse", "ggplot2", "sf", "tibble", "lubridate", "stringr", "reshape2", "zoo", "mgcv", "stargazer", "Metrics", "sqldf", "ecp", "knitr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
options(scipen=999)

## ======================================================================================
## IMPORT DATA (including deaths in aggregate and deaths by cause)
## ======================================================================================
# Import Data Tables: Air pollution
air_pollution_CA_1999_2021 <- read.csv("PM2.5_California_1999_2021.csv")
deaths_CA_1999_2019 <- read.csv("Deaths_California_1999_2019.csv")
deaths_CA_1999_2019_lagged <- read.csv("Deaths_California_1999_2019_Lagged.csv")
# Import Population Data
population_CA_2000_2010 <- read.csv("Population_Intercensal_CA_2000_2010.csv")
population_CA_2011_2019 <- read.csv("Population_Estimates_CA_2011_2019.csv")
## Calculate Expected Deaths based on the rolling 5 year average
deaths_CA_1999_2019$Year_Month <- as.Date(deaths_CA_1999_2019$Year_Month, "%d/%m/%y")
deaths_CA_1999_2019_lagged$Forward.Year_Month <- as.Date(deaths_CA_1999_2019_lagged$Forward.Year_Month, "%d/%m/%y")

# Import Californian Wildfire Years and Temperature data from ACI
Year_Month_CA_Wildfire <- read.csv("CA_Wildfire_Events.csv")
Year_Month_CA_Wildfire$Year_Month <- as.Date(Year_Month_CA_Wildfire$Year_Month, "%d/%m/%y")

Temperature_ACI <- read.csv("Temperature_ACI.csv")
Temperature_ACI$Year_Month <- as.Date(Temperature_ACI$Year_Month, "%d/%m/%y")

## ======================================================================================
## MANIPULATIONS
## ======================================================================================

## AIR POLLUTION: PM2.5
# create monthly average PM2.5 table

air_pollution_CA_1999_2021$Year_Month <- as.Date(air_pollution_CA_1999_2021$Year_Month, "%d/%m/%y")

monthly_pm2.5_CA_1999_2021 <- air_pollution_CA_1999_2021 %>%
  group_by(STATE, COUNTY_CODE, COUNTY, Year_Month) %>%
  summarise(avg_pm2.5 = mean(Daily.Mean.PM2.5.Concentration), max_pm2.5 = max(Daily.Mean.PM2.5.Concentration), min_pm2.5 = min(Daily.Mean.PM2.5.Concentration)) %>%
  arrange(Year_Month, STATE, COUNTY_CODE, COUNTY)

monthly_pm2.5_CA_1999_2021 <- monthly_pm2.5_CA_1999_2021 %>%
  group_by(STATE, COUNTY_CODE, COUNTY) %>%
  mutate(avg_pm2.5_t_1 = lag(avg_pm2.5, n=1), 
         max_pm2.5_t_1 = lag(max_pm2.5, n=1), 
         max_pm2.5_t_2 = lag(max_pm2.5, n=2), 
         min_pm2.5_t_1 = lag(min_pm2.5, n=1))

## JOIN TABLES: DEATHS AND LAGGED DEATHS
deaths_joined <- left_join(deaths_CA_1999_2019, deaths_CA_1999_2019_lagged, 
                           by=c("Gender" = "Gender"
                                , "COUNTY_CODE" = "COUNTY_CODE"
                                , "Five.Year.Age.Groups.Code" = "Five.Year.Age.Groups.Code"
                                , "Year_Month" = "Forward.Year_Month"))

deaths_joined[is.na(deaths_joined)] = 0

monthly_deaths_CA_1999_2019 <- deaths_joined %>%
  group_by(Gender, COUNTY_CODE, Five.Year.Age.Groups.Code, Year_Month) %>%
  summarise(Deaths = sum(Deaths), Lagged.Deaths = sum(Lagged.Deaths))

#remapping age bands for consistency with population data
#first convert factor column to the character data type
monthly_deaths_CA_1999_2019$Five.Year.Age.Groups.Code <- as.character(monthly_deaths_CA_1999_2019$Five.Year.Age.Groups.Code)
monthly_deaths_CA_1999_2019$Five.Year.Age.Groups.Code[monthly_deaths_CA_1999_2019$Five.Year.Age.Groups.Code == "1"] <- "1-4"
monthly_deaths_CA_1999_2019$Five.Year.Age.Groups.Code[monthly_deaths_CA_1999_2019$Five.Year.Age.Groups.Code == "85-89"] <- "85+"
monthly_deaths_CA_1999_2019$Five.Year.Age.Groups.Code[monthly_deaths_CA_1999_2019$Five.Year.Age.Groups.Code == "90-94"] <- "85+"
monthly_deaths_CA_1999_2019$Five.Year.Age.Groups.Code[monthly_deaths_CA_1999_2019$Five.Year.Age.Groups.Code == "95-99"] <- "85+"
monthly_deaths_CA_1999_2019$Five.Year.Age.Groups.Code[monthly_deaths_CA_1999_2019$Five.Year.Age.Groups.Code == "100+"] <- "85+"

#write.csv(monthly_deaths_CA_1999_2019, "monthly_deaths_CA_1999_2019.csv")
#regroup the death count
monthly_deaths_CA_1999_2019 <- monthly_deaths_CA_1999_2019 %>%
  group_by(Gender, COUNTY_CODE, Five.Year.Age.Groups.Code, Year_Month) %>%
  summarise(Deaths = sum(Deaths, na.rm=TRUE),
            Lagged.Deaths = sum(Lagged.Deaths, na.rm=TRUE))

# ============================================================================================================================================================
# Population File - reformat to join to CA_Data
# ============================================================================================================================================================

#1999 year is using 2000 year April census population data
#2000 year is using 2000 year July census estimates

population_CA_2000_2010$Year <- ifelse(population_CA_2000_2010$YEAR %in% 1:11, population_CA_2000_2010$YEAR + 1998, 
                                       ifelse(population_CA_2000_2010$YEAR == 13, "2010", population_CA_2000_2010$YEAR))
population_CA_2000_2010 <- population_CA_2000_2010[!(population_CA_2000_2010$Year==12),]
population_CA_2000_2010 <- population_CA_2000_2010[!(population_CA_2000_2010$AGEGRP==0),]

population_CA_2000_2010$Five.Year.Age.Groups.Code <- ifelse(population_CA_2000_2010$AGEGRP == 1, "1-4", 
                                                            ifelse(population_CA_2000_2010$AGEGRP == 2, "5-9", 
                                                                   ifelse(population_CA_2000_2010$AGEGRP == 3, "10-14", 
                                                                          ifelse(population_CA_2000_2010$AGEGRP == 4, "15-19", 
                                                                                 ifelse(population_CA_2000_2010$AGEGRP == 5, "20-24", 
                                                                                        ifelse(population_CA_2000_2010$AGEGRP == 6, "25-29", 
                                                                                               ifelse(population_CA_2000_2010$AGEGRP == 7, "30-34",
                                                                                                      ifelse(population_CA_2000_2010$AGEGRP == 8, "35-39", 
                                                                                                             ifelse(population_CA_2000_2010$AGEGRP == 9, "40-44", 
                                                                                                                    ifelse(population_CA_2000_2010$AGEGRP == 10, "45-49", 
                                                                                                                           ifelse(population_CA_2000_2010$AGEGRP == 11, "50-54", 
                                                                                                                                  ifelse(population_CA_2000_2010$AGEGRP == 12, "55-59", 
                                                                                                                                         ifelse(population_CA_2000_2010$AGEGRP == 13, "60-64", 
                                                                                                                                                ifelse(population_CA_2000_2010$AGEGRP == 14, "65-69", 
                                                                                                                                                       ifelse(population_CA_2000_2010$AGEGRP == 15, "70-74", 
                                                                                                                                                              ifelse(population_CA_2000_2010$AGEGRP == 16, "75-79", 
                                                                                                                                                                     ifelse(population_CA_2000_2010$AGEGRP == 17, "80-84", 
                                                                                                                                                                            ifelse(population_CA_2000_2010$AGEGRP == 18, "85+", population_CA_2000_2010$AGEGRP))))))))))))))))))

population_CA_2000_2010 <- select (population_CA_2000_2010,-c(AGEGRP, YEAR))

# Population File 2011 - 2019

population_CA_2011_2019$Year <- population_CA_2011_2019$YEAR + 2007
population_CA_2011_2019 <- population_CA_2011_2019 %>%
  mutate(Year = as.character(Year))

# Drop the columns of the dataframe
population_CA_2011_2019 <- select (population_CA_2011_2019,-c(YEAR, SUMLEV, POPEST_MALE, POPEST_FEM))

long <- melt(population_CA_2011_2019, id.vars = c("STATE", "COUNTY", "STNAME", "CTYNAME", "Year"))

long$Five.Year.Age.Groups.Code <- 
  ifelse(grepl("AGE04_",long$variable),"1-4",
         ifelse(grepl("AGE59_",long$variable),"5-9",
                ifelse(grepl("AGE1014_",long$variable),"10-14", 
                       ifelse(grepl("AGE1519_",long$variable),"15-19",
                              ifelse(grepl("AGE2024_",long$variable),"20-24",
                                     ifelse(grepl("AGE2529_",long$variable),"25-29",
                                            ifelse(grepl("AGE3034_",long$variable),"30-34",
                                                   ifelse(grepl("AGE3539_",long$variable),"35-39",
                                                          ifelse(grepl("AGE4044_",long$variable),"40-44",     
                                                                 ifelse(grepl("AGE4549_",long$variable),"45-49",
                                                                        ifelse(grepl("AGE5054_",long$variable),"50-54",
                                                                               ifelse(grepl("AGE5559_",long$variable),"55-59",
                                                                                      ifelse(grepl("AGE6064_",long$variable),"60-64",
                                                                                             ifelse(grepl("AGE6569_",long$variable),"65-69",       
                                                                                                    ifelse(grepl("AGE7074_",long$variable),"70-74",
                                                                                                           ifelse(grepl("AGE7579_",long$variable),"75-79",     
                                                                                                                  ifelse(grepl("AGE8084_",long$variable),"80-84",
                                                                                                                         ifelse(grepl("AGE85PLUS_",long$variable),"85+",long$variable))))))))))))))))))

long$Gender <-
  ifelse(grepl("_MALE",long$variable),"M",
         ifelse(grepl("_FEM",long$variable),"F",long$variable))

temp <-  
  long %>%
  group_by(long$Five.Year.Age.Groups.Code, long$STATE, long$COUNTY, long$STNAME, long$CTYNAME, long$Year, long$Gender) %>%
  summarise(POPULATION = sum(value))

population_CA_2011_2019 <- temp %>% plyr::rename(c("long$Gender" = "GENDER"
                                                   , "long$Year" = "Year"
                                                   , "long$Five.Year.Age.Groups.Code" = "Five.Year.Age.Groups.Code"
                                                   , "long$STATE" = "STATE"
                                                   , "long$COUNTY" = "COUNTY"
                                                   , "long$STNAME" = "STNAME"
                                                   , "long$CTYNAME" = "COUNTY_NAME"))

population_CA_2011_2019$COUNTY_NAME <- gsub('.{7}$', '', population_CA_2011_2019$COUNTY_NAME)

monthly_pm2.5_CA_1999_2021$Year = substr(monthly_pm2.5_CA_1999_2021$Year_Month, 1, 4)

CA_joined <- full_join(population_CA_2000_2010, monthly_pm2.5_CA_1999_2021
                       , by=c("COUNTY" = "COUNTY_CODE"
                              , "Year" = "Year"
                              , "STNAME" = "STATE"
                              , "COUNTY_NAME" = "COUNTY"))

# there are 500 rows where there are NA readings for PM2.5 - this is either due to time (e.g. no records for San Francisco in 1999 January), or location (e.g. no readings for Yuba county 115)
#CA_joined <- CA_joined[complete.cases(CA_joined),]

CA_Data_Test <- left_join(CA_joined, population_CA_2011_2019
                          , by=c("COUNTY" = "COUNTY"
                                 , "COUNTY_NAME" = "COUNTY_NAME"
                                 , "Year" = "Year"
                                 , "STNAME" = "STNAME"))

CA_Data_Test$Five.Year.Age.Groups.Code <- ifelse(is.na(CA_Data_Test$Five.Year.Age.Groups.Code.x), CA_Data_Test$Five.Year.Age.Groups.Code.y, CA_Data_Test$Five.Year.Age.Groups.Code.x)

CA_Data_Test$GENDER <- ifelse(is.na(CA_Data_Test$GENDER.x), as.character(CA_Data_Test$GENDER.y), as.character(CA_Data_Test$GENDER.x))

CA_Data_Test$Population_Gender <- ifelse(is.na(CA_Data_Test$POPULATION.x), 0, CA_Data_Test$POPULATION.x) + ifelse(is.na(CA_Data_Test$POPULATION.y), 0, CA_Data_Test$POPULATION.y)

CA_Data_Test$STATE <- ifelse(is.na(CA_Data_Test$STATE.x), as.character(CA_Data_Test$STATE.y), as.character(CA_Data_Test$STATE.x))

CA_Data <- select (CA_Data_Test,-c(POPULATION.x, POPULATION.y, Five.Year.Age.Groups.Code.x, Five.Year.Age.Groups.Code.y, STATE.x, STATE.y, GENDER.x, GENDER.y))

CA_Data <- left_join(CA_Data, monthly_deaths_CA_1999_2019
                     , by=c("COUNTY" = "COUNTY_CODE"
                            , "GENDER" = "Gender"
                            , "Five.Year.Age.Groups.Code" = "Five.Year.Age.Groups.Code"
                            , "Year_Month" = "Year_Month"))

CA_Data <- mutate_at(CA_Data, c("Deaths", "Lagged.Deaths"), ~replace(., is.na(.), 0))
CA_Data <- CA_Data %>% filter(!is.na(Year_Month))

CA_Data$ID <- paste(as.character(CA_Data$COUNTY), as.character(CA_Data$GENDER), sep= "_")
CA_Data$ID <- paste(as.character(CA_Data$ID), as.character(CA_Data$Five.Year.Age.Groups.Code), sep= "_")

get.mav <- function(Dth_E, n=60) {
  if(is.na(Dth_E[1])) Dth_E[1] <- Dth_E
  Dth_E <- rollapply(Dth_E, width = n, mean, align = "right", partial = TRUE)
}

CA_Data <- with(CA_Data, CA_Data[order(ID, Year_Month),])
CA_Data$expected_death_60mth <- unlist(aggregate(Deaths~ID, CA_Data, get.mav, na.action = NULL, n=60)$Deaths)

## ======================================================================================  
## JOIN TO CALIFORNIA WILDFIRE EVENT YEARS AND ACI TEMPERATURE

Year_Month_CA_Wildfire <- select (Year_Month_CA_Wildfire,-c(Year, Month, Old_Year_Month))

CA_Data <- left_join(CA_Data, Year_Month_CA_Wildfire, by = c("Year_Month" = "Year_Month", "COUNTY_NAME" = "County"))
CA_Data <- left_join(CA_Data, Temperature_ACI, by = c("Year_Month" = "Year_Month"))

## ======================================================================================
## COSMETIC CHANGES (FOR PRESENTATIONAL PURPOSES)
## ======================================================================================

CA_Data$Deaths[is.na(CA_Data$Deaths)] <- 0
CA_Data$expected_death_60mth[is.na(CA_Data$expected_death_60mth)] <- 0

## CHANGE DEATH COUNT TO DEATH RATE PER MILLE
## NOTE THIS DOES NOT CHANGE THE RESULT SINCE EVERYTHING WILL BE IN RATES, RATHER THAN COUNTS
## EXPECTED DEATHS ALSO EXPRESSED AS RATES, removed split by cause
CA_Data$Death_Rate_Actual <- (CA_Data$Deaths/CA_Data$Population_Gender)*1000
CA_Data$Death_Rate_lagged_Actual <- (CA_Data$Lagged.Deaths/CA_Data$Population_Gender)*1000
#adopt 5 year avg as expected
CA_Data$Death_Rate_Expected <- (CA_Data$expected_death_60mth/CA_Data$Population_Gender)*1000
# Calculate Excess Death Rates
CA_Data$Excess_Death_Rate <- ((CA_Data$Deaths - CA_Data$expected_death_60mth) / CA_Data$Population_Gender) * 1000
CA_Data$Excess_Death_Rate_Lagged <- ((CA_Data$Lagged.Deaths - CA_Data$expected_death_60mth) / CA_Data$Population_Gender) * 1000

## ======================================================================================
## Models
## ======================================================================================

# First run LM and GAM
# compare models
# conclude that including nonlinear effects improves the model considerably
# try with additional explanatory variables

mod_lm2 <- lm(Excess_Death_Rate ~ max_pm2.5 + T10 + T90 + as.numeric(as.factor(Five.Year.Age.Groups.Code)) + GENDER + COUNTY + Year_Month , data = CA_Data)
mod_lm3 <- lm(Excess_Death_Rate ~ max_pm2.5 + Excess_Death_Rate_Lagged + T10 + T90 + as.numeric(as.factor(Five.Year.Age.Groups.Code)) + GENDER + COUNTY + Year_Month , data = CA_Data)
mod_lm4 <- lm(Excess_Death_Rate ~ max_pm2.5_t_1 + Excess_Death_Rate_Lagged + T10 + T90 + as.numeric(as.factor(Five.Year.Age.Groups.Code)) + GENDER + COUNTY + Year_Month, data = CA_Data)

RSS_lm2 <- c(crossprod(mod_lm2$residuals))
MSE_lm2 <- RSS_lm2 / length(mod_lm2$residuals)
RMSE_lm2 <- sqrt(MSE_lm2)

RSS_lm3 <- c(crossprod(mod_lm3$residuals))
MSE_lm3 <- RSS_lm3 / length(mod_lm3$residuals)
RMSE_lm3 <- sqrt(MSE_lm3)

RSS_lm4 <- c(crossprod(mod_lm4$residuals))
MSE_lm4 <- RSS_lm4 / length(mod_lm4$residuals)
RMSE_lm4 <- sqrt(MSE_lm4)

cat("MSE: ", MSE_lm2, " RMSE: ", RMSE_lm2)
cat("MSE: ", MSE_lm3, " RMSE: ", RMSE_lm3)
cat("MSE: ", MSE_lm4, " RMSE: ", RMSE_lm4)

summary(mod_lm3)

#results of linear model
stargazer(
  mod_lm2, mod_lm3, mod_lm4, type = "latex",
  title = "Excess Mortality (t) fit using linear model and prior period deaths",
  column.labels = c("Linear Model", "Linear Model AR(1)", "Linear Model AR(1) with lagged PM2.5"),
  dep.var.labels=c("Excess Mortality (t)"),
  covariate.labels=c("Maximum PM2.5 Concentration (t)", "Maximum PM2.5 Concentration (t-1)", "Excess Death Rate (t-1)", "Low Temperature Index (t)", "High Temperature Index (t)", "Age Band (t)","Gender","County","Month and Year (t)"),
  header=TRUE, # to get rid of r package output text
  single.row = FALSE, # to put coefficients and standard errors on same line
  column.sep.width = "3pt", # to reduce column width
  font.size = "small" # to make font size smaller
)


#RESULTS OF GAM

mod_gam2 <- gam(Excess_Death_Rate ~ s(max_pm2.5) + s(T10) + s(T90) + 
                  s(as.numeric(as.factor(Five.Year.Age.Groups.Code))) + GENDER + s(COUNTY) + s(as.numeric(Year_Month)) , data = CA_Data, method = "REML")
mod_gam3 <- gam(Excess_Death_Rate ~ s(max_pm2.5) + s(Excess_Death_Rate_Lagged) + s(T10) + s(T90) + 
                  s(as.numeric(as.factor(Five.Year.Age.Groups.Code))) + GENDER + s(COUNTY) + s(as.numeric(Year_Month)) , data = CA_Data, method = "REML")
mod_gam4 <- gam(Excess_Death_Rate ~ s(max_pm2.5_t_1) + s(Excess_Death_Rate_Lagged) + s(T10) + s(T90) + 
                  s(as.numeric(as.factor(Five.Year.Age.Groups.Code))) + GENDER + s(COUNTY) + s(as.numeric(Year_Month)) , data = CA_Data, method = "REML")

#table to compare the three models
stargazer(summary(mod_gam2)$p.table, summary(mod_gam3)$p.table, summary(mod_gam4)$p.table, summary=TRUE)

summary(mod_gam2)
summary(mod_gam3)
summary(mod_gam4)

#check diagnostics
gam.check(mod_gam2)
gam.check(mod_gam3)
gam.check(mod_gam4)

#plot results of the chosen model
plot(mod_gam4, rug = TRUE, shade = TRUE, shade.col = "lightblue", scale = 0)

#generate MSE and RMSE for summary table
RSS_gam2 <- c(crossprod(mod_gam2$residuals))
MSE_gam2 <- RSS_gam2 / length(mod_gam2$residuals)
RMSE_gam2 <- sqrt(MSE_gam2)

RSS_gam3 <- c(crossprod(mod_gam3$residuals))
MSE_gam3 <- RSS_gam3 / length(mod_gam3$residuals)
RMSE_gam3 <- sqrt(MSE_gam3)

RSS_gam4 <- c(crossprod(mod_gam4$residuals))
MSE_gam4 <- RSS_gam4 / length(mod_gam4$residuals)
RMSE_gam4 <- sqrt(MSE_gam4)

cat("MSE: ", MSE_gam2, " RMSE: ", RMSE_gam2)
cat("MSE: ", MSE_gam3, " RMSE: ", RMSE_gam3)
cat("MSE: ", MSE_gam4, " RMSE: ", RMSE_gam4)

plot(density(mod_lm4_resid))
qqnorm(mod_lm4_resid)

# ===========================================================================
# Out of Sample Errors (LM and GAM)
# ===========================================================================
# Random seed for reproducability
set.seed(123)
#create row index for CA_Data dataset
rows <- sample(nrow(CA_Data))
#reorder CA_Data dataset
shuffled_CA_Data <- CA_Data[rows, ]
#try 80:20 split
n_rows <- nrow(CA_Data)
split <- round(n_rows * 0.8)
#split data into 80:20
training <- shuffled_CA_Data[1:split, ]
testing <- shuffled_CA_Data[(split + 1):nrow(CA_Data), ]
#fit a model using training data and predict on test set
#linear model and gam
train_mod_lm4 <- lm(Excess_Death_Rate ~ max_pm2.5_t_1 + Excess_Death_Rate_Lagged + T10 + T90 + as.numeric(as.factor(Five.Year.Age.Groups.Code)) + GENDER + COUNTY + Year_Month , data = training)
train_mod_gam4 <- gam(Excess_Death_Rate ~ s(max_pm2.5_t_1) + s(Excess_Death_Rate_Lagged) + s(T10) + s(T90) + 
                  s(as.numeric(as.factor(Five.Year.Age.Groups.Code))) + GENDER + s(COUNTY) + s(as.numeric(Year_Month)) , data = training, method = "REML")

#predict using testing data
p_lm <- predict(train_mod_lm4, testing)
p_gam <- predict(train_mod_gam4, testing)

#calculate prediction errors
error_lm <- as.vector(testing$Excess_Death_Rate - p_lm)
error_gam <- as.vector(testing$Excess_Death_Rate - p_gam)

#also compare to a simple RW where excess deaths (t) = excess deaths (t-1)
# in this data set, excess deaths (t-1) is called "excess death rate lagged"

#RW
RW_error_sq <- as.vector(CA_Data$Excess_Death_Rate - CA_Data$Excess_Death_Rate_Lagged)^2
RMSE_RW <- sqrt(mean(RW_error_sq, na.rm = TRUE))

#calculate the out of sample RMSE for the lm and gam models
RMSE_out_lm4 <- sqrt(mean(error_lm^2, na.rm = TRUE))
RMSE_out_gam4 <- sqrt(mean(error_gam^2, na.rm = TRUE))

#Summarise and compare with in-sample RMSE.
cat("Linear Model RMSE (out of sample): ", RMSE_out_lm4, " GAM RMSE (out of sample): ", RMSE_out_gam4, " Random Walk RMSE: ", RMSE_RW)

#MODEL COMPARE: LM AND GAM
AIC(mod_lm4)
summary(mod_lm4)$sp.criterion
summary(mod_lm4)$r.sq

AIC(mod_gam4)
summary(mod_gam4)$sp.criterion
summary(mod_gam4)$r.sq

test <- anova(mod_lm4, mod_gam4, test = "Chisq")

stargazer(test)

# source: https://m-clark.github.io/generalized-additive-models/application.html

# ===========================================================================
# Third Model: XGB Regression
# ===========================================================================

# load xbg packages
library(xgboost)
library(recipes)
library(caret)

# ensure results can be reproduced
set.seed(1)

# set up raw training and application data
# columns to include in model
colnames_x <- c("Excess_Death_Rate"#, "Death_Rate_Actual" # Response Var
                , "Excess_Death_Rate_Lagged"
                , "GENDER"
                , "Five.Year.Age.Groups.Code"
                , "Year_Month"
                , "COUNTY" 
                #, "max_pm2.5" 
                , "max_pm2.5_t_1"
                , "Deadliest.Indicator"
                , "Destructive.Indicator"
                , "Largest.Indicator"
                , "T10" 
                , "T90"
                )

# filter data set: remove records where actual death rate is null - this is driven by population data age bands not aligning to mortality data, therefore a death rate cannot be calculated
CA_Data <- CA_Data %>% filter(!is.na(Excess_Death_Rate))
model_data <- CA_Data[, colnames_x]

model_data = as.data.frame(model_data)
model_data[] <- sapply(model_data, as.numeric)

# extract 20% of dataset as test data
indices = createDataPartition(model_data$Excess_Death_Rate, p=0.8, list = F)
train_data <- model_data[indices, ]
test_data <- model_data[-indices, ]

grid_tune <- expand.grid(
  nrounds = c(500), #number of trees
  max_depth = c(2, 4), #height of tree
  eta = c(0.025, 0.3), #learning rate c(0.025, 0.05, 0.1, 0.3),
  gamma = c(0, 1.0), #pruning should be tuned, 
  colsample_bytree = c(0.4, 1.0), #subsample ratio of columns for tree
  min_child_weight = c(1, 3), #the larger the more conservative the model; can be used as a stop
  subsample = c(0.5, 1.0) #used to prevent overfitting by sampling x%
)

train_control <- trainControl(method = "cv",
                              number = 3,
                              verboseIter = TRUE,
                              allowParallel = TRUE
)

xgb_tune <- train(x = train_data[,-1],
                  y = train_data[,1],
                  trControl = train_control,
                  tuneGrid = grid_tune,
                  method = "xgbTree",
                  verbose = TRUE)

xgb_tune

# Tuning parameter 'nrounds' was held constant at a value of 500
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were nrounds = 500, max_depth = 4, eta = 0.025, gamma =
#   1, colsample_bytree = 1, min_child_weight = 3 and subsample = 1.

xgb.pred <- predict (xgb_tune, test_data)

mse = mean((test_data$Excess_Death_Rate - xgb.pred)^2)
mae = caret::MAE(test_data$Excess_Death_Rate, xgb.pred)
rmse = caret::RMSE(test_data$Excess_Death_Rate, xgb.pred)

# print a summary of the fit
cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

d <- tibble(pred = predict (xgb_tune, test_data)
            , obs = test_data$Excess_Death_Rate) %>% 
  mutate(resid = pred - obs,
         resid_sq = resid^2)
sstot <- sum((d$pred - mean(d$obs))^2)
ssresid <- sum(d$resid_sq)
sprintf("percent variance explained, R^2: %1.1f%%", 100 * (1 - ssresid / sstot))

ggplot(d, aes(x = resid)) +
  geom_density(fill = 'grey50', color = 'white', alpha = 0.7) +
  theme_bw()

ggplot(d, aes(x = obs, y = pred, size = resid)) +
  geom_point() +
  theme_bw()

trellis.par.set(caretTheme())
plot(xgb_tune)

gbmImp <- varImp(xgb_tune, scale = FALSE, labels = c("Excess Death Rate Lagged", "Max PM2.5 Lagged", 
                                                     "County", "Low Temperature Index", "Year and Month", 
                                                     "High Temperature Index", "Indicator of Deadly Wildfire Event" 
                                                     , "Indicator of Destructive Wildfire Event", "Indicator of Large Wildfire Event"
                                                     , "Gender"))
gbmImp

ggplot(gbmImp) %>%
+ theme_minimal() %>%
+ scale_x_discrete(limits = c("GENDER", "Largest.Indicator","Five.Year.Age.Groups.Code"
                              , "Destructive.Indicator", "Deadliest.Indicator"
                              , "T90", "Year_Month", "T10", "COUNTY", "max_pm2.5_t_1"
                              , "Excess_Death_Rate_Lagged"),
                    labels = c("Gender", "Indicator of Large Wildfire Event", "Age Band" 
                               , "Indicator of Destructive Wildfire Event", "Indicator of Deadly Wildfire Event"
                               , "High Temperature Index", "Year and Month", "Low Temperature Index"
                               , "County", "Max PM2.5 Lagged", "Excess Death Rate Lagged"))



