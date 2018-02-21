## Assignment 1 ## 
## Renewables in electricity market ## 

## Workspace directory
setwd("~/Documents/DTU/B_Semester-2/31761_Renew_ElectricityMarkets/Assignments/Assignment1/New_try")

## Install packages 
# install.packages("ggplot2")
# install.packages("lpSolve")
# install.packages("linprog")
# install.packages("circlize")

## Read packages if already installed 
library(ggplot2)
library(lpSolve)
library(linprog)
library(tidyverse)
library(circlize)


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
price_hour = demand_hour = matrix(0,nrow = N_simulations, ncol = 2) # 2 zones
welfare_hour = vector()

## DK1 = WEST / DK2 = EAST
for (i in 1:N_simulations){
  # Objective function
  f.obj = c(0, -17, 0, -500, 70, 
            64, 153, 82, 89, 25, 19, 43, 39, 36, 31, 5, 10, 0, 1000, 1000)
  
  # define load shedding
  lsDK1 = 1000000
  lsDK2 = 1000000
  
  b = c(WW1_pp*Wind$DK1[i], WW2_pp*Wind$DK1[i],EW1_pp*Wind$DK2[i], EW2_pp*Wind$DK2[i],
        400, 330, 345, 390, 510, 1000, 1200, 320, 360, 400, 350, 730, 630, transmission_cap, lsDK1, lsDK2)
  
  beq_DK1 = Consumption$DK1[i] + GermanyExport$Germany_quantity[i] - NorwayImport$Norway_Quantity[i]
  beq_DK2 = Consumption$DK2[i] + SwedenExport$Sweden_quantity[i]
  
  beq = c(beq_DK1, beq_DK2)
  
  f.rhs <- c(b,beq)
  
  eq.c <- rep(1,length(f.obj))
  
  # Construct the A eq vector for the equality constraint
  Aeq_DK1 = c(1,1,0,0,
              1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,0)
  
  Aeq_DK2 = c(0,0,1,1,
              0,0,0,0,0,0,1,1,1,1,1,1,1,-1,0,1)
  
  f.con = diag(1,nrow=length(eq.c)+2, ncol = length(eq.c))
  f.con[length(eq.c)+1,] = Aeq_DK1
  f.con[length(eq.c)+2,] = Aeq_DK2
  
  
  # Declare the all types of constraints to be represented (i.e., 1 equality # constraint , and a suite of inequality constraints)
  f.dir <- c(rep("<=",length(b)),"==","==")
  
  # solving 
  lp("min", f.obj, f.con, f.dir, f.rhs)
  v.sol <- lp("min", f.obj, f.con, f.dir, f.rhs)$solution
  supply_dispatch = v.sol[1:length(b)]
  
  lp_sol_dual <- -lp("min", f.obj, f.con, f.dir, f.rhs,compute.sens=TRUE)$duals[1:length(beq)] 
  eq.welfare <- lp("min", f.obj, f.con, f.dir, f.rhs,compute.sens=TRUE)$objval 
  
  # Keeps track of the results
  dispatch_hour[i,] = supply_dispatch
  price_hour[i,] = lp_sol_dual
  demand_hour[i,] = beq
  welfare_hour[i] = abs(eq.welfare)
}

# Plots 
## DK1
# Wind production
plot(1:N_simulations, demand_hour[,1], type = 'l', ylim = c(0,max(demand_hour[,1])+1000), xlab = "Time [h]", ylab = "[MW]")
lines(1:N_simulations,dispatch_hour[,1]+dispatch_hour[,2], col = "blue")
legend("topleft", legend = c("Electricity demand in DK1", "Wind production in DK1"), col = c("black", "blue"), lty = c(1,1), cex = 0.75 )
title(main= "Wind production in the electricity market DK1")

# let's look at the price evolution 
plot(1:N_simulations, price_hour[,1], xlab = "Time [h]", ylab = "Electricity price [Eur/MWh]", type = 'l', ylim = c(-5, max(price_hour[,1])+50))
abline( h = max(price_hour[,1]), col = "red")
abline( h = mean(price_hour[,1]), col = "orange", lty = 2, lwd = 2)
legend("topleft", legend= c("Max price", "Mean price over the month"), col = c("red","orange"), lty = c(1,2), lwd = c(1,2))
title(main= "January electricity price evolution in DK1")

# let's look at the wind penetration in this market 
## Wind energy penetration [%] is calculated as the wind energy produced in a time period [hour; day; year] 
## divided by the total electricity consumption in that period
plot(1:N_simulations, (dispatch_hour[,1]+dispatch_hour[,2])/demand_hour[,1]*100, type = 'l', ylim = c(-5,105),
     xlab = "Time [h]", ylab = "[%]")
title(main = "Wind penetration in DK1")

## DK2
# Wind production
plot(1:N_simulations, demand_hour[,2], type = 'l', ylim = c(0,max(demand_hour[,2])+1000),xlab = "Time [h]", ylab = "[MW]")
lines(1:N_simulations,dispatch_hour[,3]+dispatch_hour[,4], col = "blue")
legend("topleft", legend = c("Electricity demand in DK2", "Wind production in DK2"), col = c("black", "blue"), lty = c(1,1), cex = 0.75 )
title(main= "Wind production in the electricity market DK2")

# Let's look at the wind penetration in this market
## Wind energy penetration [%] is calculated as the wind energy produced in a time period [hour; day; year] 
## divided by the total electricity consumption in that period
plot(1:N_simulations, (dispatch_hour[,3]+dispatch_hour[,4])/demand_hour[,2]*100, type = 'l', ylim = c(-5,105),
     xlab = "Time [h]", ylab = "[%]")
title(main = "Wind penetration in DK2")

# let's look at the price evolution 
plot(1:N_simulations, price_hour[,2], xlab = "Time [h]", ylab = "Electricity price [Eur/MWh]",ylim = c(-5, max(price_hour[,2])+50), type = "l", col = "black")
abline( h = max(price_hour[,2]), col = "red")
abline( h = mean(price_hour[,2]), col = "orange", lty = 2, lwd = 2)
legend("topleft", legend= c("Max price", "Mean price over the month"), col = c("red","orange"), lty = c(1,2), lwd = c(1,2))
title(main= "January electricity price evolution in DK2")

## Global
# here we take a look at the welfare evolution
plot(1:N_simulations, welfare_hour, type = "l", xlab = "Time [h]", ylab = "Welfare [Eur/MW]")
title(main= "Welfare evolution during January for the global market")

# here we look at the usage of the transmission
count = 0
for (i in 1:N_simulations) if (dispatch_hour[i,18]==600) count = count + 1
cat(count/N_simulations*100) # retunr the % of time when transmission is fully used

plot(1:N_simulations, dispatch_hour[,18], type = 'l', xlab = "Time [h]", ylab = "[MW]")
abline(h = transmission_cap, col ="red")
title(main = "Transmission from DK2 to DK1", sub = "Full transmission 62.3% of the time", col.sub = "red")

# here we look the difference in prices [%] from the 2 markets
plot(1:N_simulations, (price_hour[,1]/price_hour[,2])*100, type = 'l', xlab = "Time [h]", ylab = "[%]")
abline( h = mean((price_hour[,1]/price_hour[,2])*100, na.rm = TRUE), col = "blue")
title(main = "Ratio of PriceDK1/PriceDK2 and mean value",
      sub = "Null DK2 prices ignored", cex.sub = 0.75)

plot(1:N_simulations, price_hour[,2], xlab = "Time [h]", ylab = "Electricity price [Eur/MWh]",ylim = c(-5, max(price_hour[,2])+50), type = "l", col = "black")
lines(1:N_simulations, price_hour[,1], col = "blue")
legend("topleft", legend= c("DK2", "DK1"), col = c("black","blue"), lty = 1)
title(main = "Electricity price evolution for both markets")


# histogram
