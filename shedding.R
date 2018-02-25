## Assignment 1 ## 
## Renewables in electricity market ## 

## Workspace directory
setwd("~/Documents/DTU/B_Semester-2/31761_Renew_ElectricityMarkets/Assignments/Assignment1/New_try")

## Install packages 
# install.packages("ggplot2")
# install.packages("lpSolve")
# install.packages("lpSolveAPI")


## Read packages if already installed 
library(ggplot2)
library(lpSolve)
library(linprog)
library(lpSolveAPI)


## read data ## 
Avedovre = read.csv("data/Avedovre.csv", header = TRUE, sep =";", skip = 1)
BlueWater = read.csv("data/BlueWater.csv", header = TRUE, sep =";", skip = 1)
FlexiGas = read.csv("data/FlexiGas.csv", header = TRUE, sep =";", skip = 1)
Nuke22 = read.csv("data/Nuke22.csv", header = TRUE, sep =";", skip = 1)
Peako =  read.csv("data/Peako.csv", header = TRUE, sep =";", skip = 1)
RoskildeCHP = read.csv("data/RoskildeCHP.csv", header = TRUE, sep =";", skip = 1)

## Import / export ##
NorwayImport = read.csv("data/NorwayImport.csv", header = TRUE, sep =";", skip = 1)
GermanyExport = read.csv("data/GermanyExport.csv", header = TRUE, sep = ";", skip =0)
SwedenExport = read.csv("data/SwedenExport.csv", header = TRUE, sep = ";", skip =0)

## Prognosis consumption ## 
Consumption = read.csv("data/consumption-prognosis_2018_hourly.csv", header = TRUE, sep =",", skip = 2)
## Prognosis wind power ## 
Wind = read.csv("data/wind-power-dk-prognosis_2018_hourly.csv", header = TRUE, sep =",", skip = 2)
WW1_pp = 0.25 # WestWind1 quantity based on predicted prod
WW2_pp = 0.75 # WestWind2 quantity based on predicted prod
EW1_pp = 1/3 # EastWind1 quantity based on predicted prod
EW2_pp = 2/3 # EastWind2 quantity based on predicted prod

## 
N_simulations = 31*24 # January hourly
transmission_cap = 600 # MWh

## Save the results 
dispatch_hour = matrix(0,nrow = N_simulations, ncol = 20)
price_hour_new = matrix(0,nrow = N_simulations, ncol = 43)
demand_hour = matrix(0,nrow = N_simulations, ncol = 2)

## DK1 = WEST / DK2 = EAST
for (i in 1:N_simulations){
 
  lps.model <- make.lp(22,20)
  set.column(lps.model, 1, c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0))
  set.column(lps.model, 2, c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0))
  set.column(lps.model, 3, c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1))
  set.column(lps.model, 4, c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1))
  set.column(lps.model, 5, c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0))
  set.column(lps.model, 6, c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0))
  set.column(lps.model, 7, c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0))
  set.column(lps.model, 8, c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0))
  set.column(lps.model, 9, c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0))
  set.column(lps.model, 10, c(0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0))
  set.column(lps.model, 11, c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1))
  set.column(lps.model, 12, c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1))
  set.column(lps.model, 13, c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1))
  set.column(lps.model, 14, c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1))
  set.column(lps.model, 15, c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1))
  set.column(lps.model, 16, c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1))
  set.column(lps.model, 17, c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1))
  set.column(lps.model, 18, c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,-1))
  set.column(lps.model, 19, c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0))
  set.column(lps.model, 20, c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1))
  set.objfn(lps.model, c(0, -17, 0, -500, 70, 64, 153, 82, 89, Nuke22$G6_price[i], Nuke22$G7_price[i], 43, 39, 36, 31, 5, 10, 0, 10000, 10000))
  set.constr.type(lps.model, c(rep("<=", 20),"=", "=") )
  
  b = c(WW1_pp*Wind$DK1[i], WW2_pp*Wind$DK1[i],EW1_pp*Wind$DK2[i], EW2_pp*Wind$DK2[i],
        400, 330, 345, 390, 510,Nuke22$G6_quantity[i], Nuke22$G7_quantity[i], 320, 360, 400, 350, 730, 630, transmission_cap, lsDK1= 10000, lsDK2=10000)
  beq_DK1 = Consumption$DK1[i] + GermanyExport$Germany_quantity[i] - NorwayImport$Norway_Quantity[i]
  beq_DK2 = Consumption$DK2[i] + SwedenExport$Sweden_quantity[i]
  beq = c(beq_DK1, beq_DK2)
 
  
  set.rhs(lps.model, c(b,beq))
  set.bounds(lps.model, lower = -600, upper = 600, columns = 18)

  solve(lps.model)
  dispatch_hour[i,] <- get.variables(lps.model)
  demand_hour[i,1] = beq_DK1
  demand_hour[i,2] = beq_DK2
  price_hour_new[i,] <- -get.dual.solution(lps.model)
  
}

# Post-traitments 
dispatch_daily  = matrix(0,nrow = 31, ncol = 20)
day = seq(1,N_simulations,by = 24)
for (i in 1:length(day)){
  dispatch_daily[i,] = colSums(dispatch_hour[day[i]:(day[i]+23),])
}

# General wind penetration 
wp = ((dispatch_hour[,1]+dispatch_hour[,2]+dispatch_hour[,3]+dispatch_hour[,4])/(demand_hour[,1]+demand_hour[,2]))*100
mean_wp = mean(wp)
plot(1:N_simulations, wp, 
     type = 'l', ylim = c(-5,105), xlab = "Time [h]", ylab = "[%]", lwd = 2)
abline(h = mean_wp,
      col = "blue")
legend("bottomright", legend = paste0("Mean penetration over January : ", round(mean_wp) ))
title(main = "Wind penetration in DK")


# DK1 
# General Demand vs wind offers DK1
plot(1:N_simulations, demand_hour[,1], type = "l", ylim = c(0,4000), lwd = 2,
     xlab = "Time [h]", ylab = "[MWh]")
lines(1:N_simulations, Wind$DK1, col = "blue", lwd = 2)
legend("topleft", legend = c("Electricity Demand DK1", "Wind offers DK1"), col=c("black","blue"),lty = 1, lwd = 2, cex = 0.75)

# Wind production
plot(1:N_simulations, demand_hour[,1], type = 'l', ylim = c(0,4000), xlab = "Time [h]", ylab = "[MW]", lwd = 2)
lines(1:N_simulations,dispatch_hour[,1]+dispatch_hour[,2], col = "blue", lwd = 2)
legend("topleft", legend = c("Electricity demand in DK1", "Wind production in DK1"), col = c("black", "blue"), lty = c(1,1), cex = 0.75, lwd = 2 )
title(main= "Wind production in the electricity market DK1")

# let's look at the wind penetration in this market 
## Wind energy penetration [%] is calculated as the wind energy produced in a time period [hour; day; year] 
## divided by the total electricity consumption in that period
count = 0
for (i in 1:N_simulations){
  if ((dispatch_hour[i,1]+dispatch_hour[i,2])/demand_hour[i,1]*100 >= 100) count = count + 1
}
cat(paste0("100% wind penetration in DK1 : ", round(count/N_simulations*100), " % of the time"))

par(mar = c(4.5, 4.5,2, 2))
plot(1:N_simulations, (dispatch_hour[,1]+dispatch_hour[,2])/demand_hour[,1]*100, type = 'l', ylim = c(-5,135),
     xlab = "Time [h]", ylab = "[%]", lwd = 2)
legend("bottomright", legend = paste0("100% wind penetration in DK1 : ", round(count/N_simulations*100), " % of the time"), cex = 0.75)
title(main = "Wind penetration in DK1")

## DK2
# General Demand vs wind offers DK2
plot(1:N_simulations, demand_hour[,2], type = "l", ylim = c(0,3000), lwd = 2,
     xlab = "Time [h]", ylab = "[MWh]")
lines(1:N_simulations, Wind$DK2, col = "blue", lwd = 2)
legend("topleft", legend = c("Electricity Demand DK2", "Wind offers DK2"), col=c("black","blue"),lty = 1, lwd = 2, cex = 0.75)

# Wind production
par(mar = c(4.5, 4.5,2, 2))
plot(1:N_simulations, demand_hour[,2], type = 'l', ylim = c(0,3000),xlab = "Time [h]", ylab = "[MW]", lwd = 2)
lines(1:N_simulations,dispatch_hour[,3]+dispatch_hour[,4], col = "blue", lwd = 2)
legend("topleft", legend = c("Electricity demand in DK2", "Wind production in DK2"), col = c("black", "blue"), lty = c(1,1), cex = 0.75, lwd = 2 )
title(main= "Wind production in the electricity market DK2")

# Let's look at the wind penetration in this market
## Wind energy penetration [%] is calculated as the wind energy produced in a time period [hour; day; year] 
## divided by the total electricity consumption in that period
count = 0
for (i in 1:N_simulations){
  if ((dispatch_hour[i,3]+dispatch_hour[i,4])/demand_hour[i,2]*100 == 100) count = count + 1
}
cat(paste0("100% wind penetration in DK2 : ", round(count/N_simulations*100), " % of the time"))

par(mar = c(4.5, 4.5,2, 2))
plot(1:N_simulations, (dispatch_hour[,3]+dispatch_hour[,4])/demand_hour[,2]*100, type = 'l', ylim = c(-5,105),
     xlab = "Time [h]", ylab = "[%]", col ="black", lwd = 2)
legend("bottomright", legend = paste0("100% wind penetration in DK2 : ", round(count/N_simulations*100), " % of the time"), cex = 0.75)
title(main = "Wind penetration in DK2")

# Influence of the transmission
plot(190:220, demand_hour[190:220,1], type = 'h', ylim = c(-2000,3500), xlab = "Time [h]", ylab = "[MW]", lwd = 10)
lines(190:220,dispatch_hour[190:220,1]+dispatch_hour[190:220,2]+dispatch_hour[190:220,5]+dispatch_hour[190:220,6]+dispatch_hour[190:220,7]+dispatch_hour[190:220,8]+dispatch_hour[190:220,9]+dispatch_hour[190:220,10], col = "grey", lwd = 4, type = "h")
lines(190:220,dispatch_hour[190:220,1]+dispatch_hour[190:220,2], col = "darkorange", lwd = 4, type = "h")
lines(190:220, dispatch_hour[190:220,18], type = "h", xlab = "Time [h]", ylab = "[MWh]", col = "green", lwd = 4)
legend("bottomleft", legend = c("Electricity demand in DK1", "Electricity prod. in DK1", "Electricity prod. from wind", "Transmission to DK2 (negative = export to DK2)"), col = c("black", "grey", "orange","green"), lty = c(1,1,1,1), cex = 0.75, lwd = 4 )
