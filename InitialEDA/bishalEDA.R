#Load the data by running the other code.
source("../ReadData.R")
sheet1 = read_fitbit_data("../../FitBit Data/", sheet = 1)
sheet2 = read_fitbit_data("../../FitBit Data/", sheet = 2)
org_data = merge(sheet2, sheet1, by = c("Date"), all.x = TRUE)
org_data$BMI_centered = org_data$BMI - 18
org_data$MinutesSedentary2 = (org_data$MinutesSedentary - 
                                mean(org_data$MinutesSedentary))
rm(sheet2, sheet1)
library('dplyr')


df1 = org_data[25:nrow(org_data),]

# creating new time variables
df1$day_of_week = weekdays(df1$Date)
df1$week = strftime(df1$Date, format = "%V")
df1$month = months(df1$Date)
df1$year =  as.numeric(substr(df1$Date,1,4))
df1$quarters = paste(df1$year,quarters(df1$Date),sep = "-")

dfMonthWeek = df1 %>%  group_by(year, month, day_of_week) %>% 
  summarize(avg_cal_MW = mean(CaloriesBurned),
            avg_sed_min_MW = mean(MinutesSedentary))
dfWeek = df1 %>%  group_by(year, month, week) %>% 
  summarize(avg_cal_W = mean(CaloriesBurned),
            avg_sed_min_W = mean(MinutesSedentary))
dfMonth = df1 %>%  group_by(year, month) %>% 
  summarize(avg_cal_M = mean(CaloriesBurned),
            avg_sed_min_M = mean(MinutesSedentary))
dfQuarter = df1 %>%  group_by(quarters) %>% 
  summarize(avg_cal_Q = mean(CaloriesBurned),
            avg_sed_min_Q = mean(MinutesSedentary))


dfMerged = merge(merge(merge(merge(df1, dfMonth, by = c("year", "month"), all.x = TRUE),
                 dfMonthWeek, by = c("year", "month", "day_of_week"),
                 all.x = TRUE),
                 dfQuarter, by = c("quarters"), all.x = TRUE),
                 dfWeek, by = c("year", "month", "week"), all.x = TRUE)%>% 
  arrange(Date)


# filtering time by day of week to plot trends
days = unique(dfMerged$day_of_week)
for (ii in days){
  assign(paste0("",ii), dfMerged %>% filter(dfMerged$day_of_week == ii))
}

#ploting at a daily level by days of week
colors = 1:7
lineStyle = 1:7

#plot calories burnt
plot(dfMerged$CaloriesBurned, type = "l")

# Daily Plot
plot(Monday$CaloriesBurned, type = "l", col = colors[2], lty = lineStyle[2],
     main = "Day of week calories plot")
lines(Sunday$CaloriesBurned, col = colors[1], lty = lineStyle[1])
lines(Tuesday$CaloriesBurned, col = colors[3], lty = lineStyle[3])
lines(Wednesday$CaloriesBurned, col = colors[4], lty = lineStyle[4])
lines(Thursday$CaloriesBurned, col = colors[5], lty = lineStyle[5])
lines(Friday$CaloriesBurned, col = colors[6], lty = lineStyle[6])
lines(Saturday$CaloriesBurned, col = colors[7], lty = lineStyle[7])
legend(90,2620, legend = days, col = colors, lty = lineStyle)



# Monthly-dayofweek Plot
plot(Sunday$avg_cal_MW, col = "green", type = "l", lty = lineStyle[1],
     main = "Monthly average for day of week calories plot")
lines(Monday$avg_cal_MW, col = "chocolate4", lty = lineStyle[2])
lines(Tuesday$avg_cal_MW, col = "brown4", lty = lineStyle[3])
lines(Wednesday$avg_cal_MW, col = "blue4", lty = lineStyle[4])
lines(Thursday$avg_cal_MW, col = "azure4", lty = lineStyle[5])
lines(Friday$avg_cal_MW, col = "aquamarine4", lty = lineStyle[6])
lines(Saturday$avg_cal_MW, col = "darkviolet", lty = lineStyle[7])
legend(90,2620, legend = days, col = colors, lty = lineStyle)


#Calories burnt patterns over different averaged times
xlim = c(0,820)
ylim = c(2000,4500)
plot(dfMerged$avg_cal_Q, col = "green", type = "l", lty = lineStyle[1],
     xlim = xlim, ylim = ylim, xlab = "", ylab = "", xaxt = 'n')
par(new = TRUE)
plot(dfMerged$avg_cal_M, col = "red", type = "l", lty = lineStyle[2],
     xlim = xlim, ylim = ylim, xlab = "", ylab = "", xaxt = 'n')
par(new = TRUE)
plot(dfMerged$avg_cal_W, col = "black", type = "l", lty = lineStyle[3], xaxt = 'n',
     xlim = xlim, ylim = ylim, xlab = "Time",
     ylab = " Calories burnt", main = "Patterns in calories burnt")
legend(1, 4500, legend = c("Quarterly", "Monthly", "Weekly"),
       lty = lineStyle[1:3], col = c("green", "red", "black"))


#Sedentary minutes patterns over different averaged times
xlim = c(0,820)
ylim = c(400,900)
plot(dfMerged$avg_sed_min_Q, col = "green", type = "l", lty = lineStyle[1],
     xlim = xlim, ylim = ylim, xlab = "", ylab = "", xaxt = 'n')
par(new = TRUE)
plot(dfMerged$avg_sed_min_M, col = "red", type = "l", lty = lineStyle[2],
     xlim = xlim, ylim = ylim, xlab = "", ylab = "", xaxt = 'n')
par(new = TRUE)
plot(dfMerged$avg_sed_min_W, col = "black", type = "l", lty = lineStyle[3], xaxt = 'n',
     xlim = xlim, ylim = ylim, xlab = "Time",
     ylab = " Sedentary minutes", main = "Patterns in sedentary minutes")
legend(1, 900, legend = c("Quarterly", "Monthly", "Weekly"),
       lty = lineStyle[1:3], col = c("green", "red", "black"))

plot(dfMerged$MinutesSedentary, type = "l", xlim =  xlim, ylim = ylim)

plot(dfMerged$CaloriesBurned, dfMerged$MinutesSedentary)

firstModel = step(lm(CaloriesBurned ~ MinutesSedentary2 + MinutesLightlyActive +
                  MinutesFairlyActive + 
                    MinutesVeryActive + BMI_centered, data = dfMerged),
                  direction = "both")
summary(firstModel)

car::vif(firstModel)

# Multivariate model and testing causality
install.packages("vars")
library("vars")
library('tseries')

varData = dfMerged[, c("CaloriesBurned", "MinutesSedentary2", 
                              "MinutesLightlyActive", "MinutesFairlyActive",
                              "MinutesVeryActive", "BMI_centered")]
varSimple = dfMerged[, c("CaloriesBurned", "MinutesSedentary")]


#For simple VAR model
# testing stationarity of data series
for (i in 1:ncol(varSimple)){
  cat("adf test for", colnames(varSimple)[i],":\n")
  print(adf.test(ts(varSimple[i])))
}


# all variables stationary
# building model
varModel = VAR(varSimple, type = "const", lag.max = 10, ic = 'HQ')

# testing Granger Causality
causality(varModel, cause = "MinutesSedentary")$Granger


# testing stationarity of data series
for (i in 1:ncol(varData)){
  cat("adf test for", colnames(varData)[i],":\n")
  print(adf.test(ts(varData[i])))
}

### Result - The model concludes that there is not added information in the variable
# for Minutes Sedentary and that it does not help predict the future values of calories burnt
# for quick intro: read https://stats.stackexchange.com/questions/111005/var-and-granger-causality-test




# TESTING BUT NOT SURE ABOUT THE IMPLICATION AND INTERPRETATION SO DO 
# NOT USE IF THE COMMENT IS STILL PRESENT LATER
# all variables stationary except BMI_centered
# Converting BMI_centered to stationary series
# adding one item to have the same length in all series
varData$BMI = c(diff(varData$BMI_centered)[1],diff(varData$BMI_centered))

# building model
varModel = VAR(varData, type = "const", lag.max = 20, ic = 'HQ')
