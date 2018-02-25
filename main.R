## Assignment 1 ## 
## Renewables in electricity market ## 

## Workspace directory
setwd("~/Documents/DTU/B_Semester-2/31761_Renew_ElectricityMarkets/Assignments/Assignment1/New_try")

## Install packages 
# install.packages("ggplot2")
# install.packages("lpSolve")
# install.packages("lpSolveAPI")
# install.packages("linprog")

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
participants = c("WW1_DK1","WW2_DK1","EW1_DK2","EW2_DK2",
                 "FlexiGas_DK1", "FlexiGas_DK1","FlexiGas_DK1",
                 "Peako_DK1", "Peako_DK1",
                 "Nuke22_DK1", "Nuke22_DK2",
                 "RoskildeCHP_DK2", "RoskildeCHP_DK2",
                 "Avedovre_DK2", "Avedovre_DK2",
                 "BlueWater_DK2", "BlueWater_DK2",
                 "Transmission_line",
                 "Shedding1", "Shedding2")
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
  f.obj = c(0, -17, 0, -500, 
            70, 64, 153, 82, 89, Nuke22$G6_price[i], Nuke22$G7_price[i], 43, 39, 36, 31, 5, 10, 0, 1000, 1000)
  
  # define load shedding 
  # first try
  lsDK1 = 1000000
  lsDK2 = 1000000
  
  b = c(WW1_pp*Wind$DK1[i], WW2_pp*Wind$DK1[i],EW1_pp*Wind$DK2[i], EW2_pp*Wind$DK2[i],
        400, 330, 345, 390, 510, Nuke22$G6_quantity[i], Nuke22$G7_quantity[i], 320, 360, 400, 350, 730, 630, transmission_cap, lsDK1, lsDK2)
  
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

stop("stop heere")
########################################################
########################################################
# Post-traitments 
dispatch_daily  = matrix(0,nrow = 31, ncol = 20)
demand_daily = matrix(0,nrow = 31, ncol = 2)
day = seq(1,N_simulations,by = 24)
for (i in 1:length(day)){
  dispatch_daily[i,] = colSums(dispatch_hour[day[i]:(day[i]+23),])
  demand_daily[i,] = colSums(demand_hour[day[i]:(day[i]+23),])
}

########################################################
########################################################
# run with no transmission / write csv.
# write_csv(data.frame(dispatch_hour), path = "dispatch_hour_no_transmission.csv")
# write_csv(data.frame(dispatch_daily), path = "dispatch_daily_no_transmission.csv")

########################################################
########################################################
# run with transmission and read csv with no transmission
dispatch_hour_no_transmission = read_csv("dispatch_hour_no_transmission.csv")

########################################################
########################################################

# Determines who benefit from the transmission by comparaing market clearing with / without transmission
for (i in 1:N_simulations){
  count = 0
  for (j in 1:17){
    if (dispatch_hour_no_transmission[[i,j]] != dispatch_hour[i,j]){
      if (dispatch_hour[i,j] < dispatch_hour_no_transmission[[i,j]]){
        dispatch_hour[i,j] = dispatch_hour_no_transmission[[i,j]]
        count = count + dispatch_hour_no_transmission[[i,j]]
      }
    }
  }
  # check if it corresponds to the transmission
  if (count == dispatch_hour[i,18]) cat("TRUE")
}

########################################################
########################################################
# Plots 
# General demand
par(mar = c(4.5, 4.5,2, 2))
png('images/daily_demand_DK1_DK2.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:31, demand_daily[,1]/1000, type = 'h', lwd = 10, ylab = "[GW]", xlab = "Days", ylim = c(0,75))
lines(1:31, demand_daily[,2]/1000, type = 'h', lwd = 5, col = "grey")
legend("topleft", legend = c("DK1", "DK2"), col = c("black", "grey"), lty = 1, lwd = 5, cex = 0.75)
dev.off()

########################################################
########################################################
## DK1
# General Demand vs wind offers DK1
png('images/DK1_demand_vs_wind_offers.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:N_simulations, demand_hour[,1], type = "l", ylim = c(0,4000), lwd = 2,
     xlab = "Time [h]", ylab = "[MWh]")
lines(1:N_simulations, Wind$DK1, col = "blue", lwd = 2)
legend("topleft", legend = c("Electricity Demand DK1", "Wind offers DK1"), col=c("black","blue"),lty = 1, lwd = 2, cex = 0.75)
dev.off()

# Wind production
par(mar = c(4.5, 4.5,2, 2))
png('images/DK1_demand_vs_wind_prod.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:N_simulations, demand_hour[,1], type = 'l', ylim = c(0,max(demand_hour[,1])+1000), xlab = "Time [h]", ylab = "[MW]", lwd = 2)
lines(1:N_simulations,dispatch_hour[,1]+dispatch_hour[,2], col = "blue", lwd = 2)
legend("topleft", legend = c("Electricity demand in DK1", "Wind production in DK1"), col = c("black", "blue"), lty = c(1,1), cex = 0.75, lwd = 2 )
title(main= "Wind production in the electricity market DK1")
dev.off()

# let's look at the price evolution 
png('images/Price_DK1.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:N_simulations, price_hour[,1], xlab = "Time [h]", ylab = "Electricity price [Eur/MWh]", type = 'l', ylim = c(-5, max(price_hour[,1])+50))
abline( h = max(price_hour[,1]), col = "red")
abline( h = mean(price_hour[,1]), col = "orange", lty = 2, lwd = 2)
legend("topleft", legend= c(paste0("Max price : ", max(price_hour[,1]), " €/MWh"), paste0("Mean price over the month: ", round(mean(price_hour[,1])), " €/MWh") ), col = c("red","orange"), lty = c(1,2), lwd = c(1,2))
title(main= "January electricity price evolution in DK1")
dev.off()

# let's look at the wind penetration in this market 
## Wind energy penetration [%] is calculated as the wind energy produced in a time period [hour; day; year] 
## divided by the total electricity consumption in that period
count = 0
for (i in 1:N_simulations){
  if ((dispatch_hour[i,1]+dispatch_hour[i,2])/demand_hour[i,1]*100 == 100) count = count + 1
}
cat(paste0("100% wind penetration in DK1 : ", round(count/N_simulations*100), " % of the time"))

par(mar = c(4.5, 4.5,2, 2))
png('images/Wind_penetrationDK1.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:N_simulations, (dispatch_hour[,1]+dispatch_hour[,2])/demand_hour[,1]*100, type = 'l', ylim = c(-5,105),
     xlab = "Time [h]", ylab = "[%]", lwd = 2)
legend("bottomright", legend = paste0("100% wind penetration in DK1 : ", round(count/N_simulations*100), " % of the time"), cex = 0.75)
title(main = "Wind penetration in DK1")
dev.off()

########################################################
## DK2
par(mar = c(4.5, 4.5,2, 2))

# General Demand vs wind offers DK2
png('images/DK2_demand_vs_wind_offers.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:N_simulations, demand_hour[,2], type = "l", ylim = c(0,3000), lwd = 2,
     xlab = "Time [h]", ylab = "[MWh]")
lines(1:N_simulations, Wind$DK2, col = "blue", lwd = 2)
legend("topleft", legend = c("Electricity Demand DK2", "Wind offers DK2"), col=c("black","blue"),lty = 1, lwd = 2, cex = 0.75)
dev.off()

# Wind production
png('images/DK2_demand_vs_wind_prod.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar = c(4.5, 4.5,2, 2))
plot(1:N_simulations, demand_hour[,2], type = 'l', ylim = c(0,max(demand_hour[,2])+1000),xlab = "Time [h]", ylab = "[MW]", lwd = 2)
lines(1:N_simulations,dispatch_hour[,3]+dispatch_hour[,4], col = "blue", lwd = 2)
legend("topleft", legend = c("Electricity demand in DK2", "Wind production in DK2"), col = c("black", "blue"), lty = c(1,1), cex = 0.75, lwd = 2 )
title(main= "Wind production in the electricity market DK2")
dev.off()

# Let's look at the wind penetration in this market
## Wind energy penetration [%] is calculated as the wind energy produced in a time period [hour; day; year] 
## divided by the total electricity consumption in that period
count = 0
for (i in 1:N_simulations){
  if ((dispatch_hour[i,3]+dispatch_hour[i,4])/demand_hour[i,2]*100 == 100) count = count + 1
}
cat(paste0("100% wind penetration in DK2 : ", round(count/N_simulations*100), " % of the time"))

par(mar = c(4.5, 4.5,2, 2))
# png('images/Wind_penetrationDK2.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:N_simulations, (dispatch_hour[,3]+dispatch_hour[,4])/demand_hour[,2]*100, type = 'l', ylim = c(-5,105),
     xlab = "Time [h]", ylab = "[%]", col ="black", lwd = 2)
legend("bottomright", legend = paste0("100% wind penetration in DK2 : ", round(count/N_simulations*100), " % of the time"), cex = 0.75)
title(main = "Wind penetration in DK2")
# dev.off()

# let's look at the price evolution 
png('images/Price_DK2.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:N_simulations, price_hour[,2], xlab = "Time [h]", ylab = "Electricity price [Eur/MWh]",ylim = c(-5, max(price_hour[,2])+50), type = "l", col = "black")
abline( h = max(price_hour[,2]), col = "red")
abline( h = mean(price_hour[,2]), col = "orange", lty = 2, lwd = 2)
legend("topleft", legend= c(paste0("Max price : ", max(price_hour[,2]), " €/MWh"), paste0("Mean price over the month: ", round(mean(price_hour[,2])), " €/MWh") ), col = c("red","orange"), lty = c(1,2), lwd = c(1,2))
title(main= "January electricity price evolution in DK2")
dev.off()

########################################################
########################################################
## Global
# here we take a look at the welfare evolution
plot(1:N_simulations, welfare_hour, type = "l", xlab = "Time [h]", ylab = "Welfare [Eur/MW]")
title(main= "Welfare evolution during January for the global market")

# here we look at the usage of the transmission
count = 0
for (i in 1:N_simulations) if (dispatch_hour[i,18]==600) count = count + 1
cat(count/N_simulations*100) # return the % of time when transmission is fully used

par(mar = c(6, 4.5,2, 2))
png('images/transmission_DK12.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:N_simulations, dispatch_hour[,18], type = 'l', xlab = "Time [h]", ylab = "[MW]")
abline(h = transmission_cap, col ="red")
title(main = "Transmission from DK2 to DK1", sub = "Congestion 62.63% of the time", col.sub = "red")
dev.off()

# here we look the difference in prices [%] from the 2 markets
par(mar = c(6, 4.5,2, 2))
png('images/Ratio_DK1DK2.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:N_simulations, (price_hour[,1]/price_hour[,2])*100, type = 'l', xlab = "Time [h]", ylab = "[%]")
abline( h = mean((price_hour[,1]/price_hour[,2])*100, na.rm = TRUE), col = "blue")
legend("bottomright", legend = paste0("mean ratio : ", round(mean((price_hour[,1]/price_hour[,2])*100, na.rm = TRUE)), " %"))
title(main = "Ratio of PriceDK1/PriceDK2 and mean value",
      sub = "Null DK2 prices ignored", cex.sub = 0.75)
dev.off()

par(mar = c(4.5, 4.5,2, 2))
png('images/Electricity_Price_DK12.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:N_simulations, price_hour[,2], xlab = "Time [h]", ylab = "Electricity price [Eur/MWh]",ylim = c(-5, max(price_hour[,2])+50), col = "black", type = 'h', lwd = 5)
lines(1:N_simulations, price_hour[,1], col = "grey", type = 'h', lwd = 2)
legend("topleft", legend= c("DK2", "DK1"), col = c("black","grey"), lwd = 3)
title(main = "Electricity price evolution for both markets")
dev.off()

## Plots per day 
par(mar = c(4.5, 4.5,2, 2))
png('images/daily_prod_wind_DK12.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:31, (dispatch_daily[,1]+dispatch_daily[,2])/1000, type = "h", lwd = 12, col = "black", 
     xlab = "Time [day]", ylab = "[GW]", ylim = c(0,90))
lines(1:31, (dispatch_daily[,3]+dispatch_daily[,4])/1000, type = "h", col = "grey", lwd = 10)
legend("topleft", legend = c("DK1", "DK2"), col = c("black", "grey"), lwd = c(12,10), lty = 1)
title(main = "[January] Daily electricity production from wind")
dev.off()

## wind penetration per day 
par(mar = c(4.5, 4.5,2, 2))
png('images/daily_penetration_wind_DK12.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:31, (dispatch_daily[,1]+dispatch_daily[,2])/demand_daily[,1]*100, type = 'h', lwd = 12, col = "black",
     xlab = "Time [day]", ylab = "[%]", ylim = c(0,130))
lines(1:31, (dispatch_daily[,3]+dispatch_daily[,4])/demand_daily[,2]*100, type = 'h', lwd = 10, col = "grey")
legend("topleft", legend = c("DK1", "DK2"), col = c("black", "grey"), lwd = c(12,10), lty = 1)
title(main = "[January] Wind penetration")
dev.off()

## Comparison between high wind penetration and low wind penetration
wp_DK1_h650 = (dispatch_hour[650,1]+dispatch_hour[650,2])/demand_hour[650,1]*100
wp_DK2_h650 = (dispatch_hour[650,3]+dispatch_hour[650,4])/demand_hour[650,2]*100

wp_DK1_h120 = (dispatch_hour[120,1]+dispatch_hour[120,2])/demand_hour[120,1]*100
wp_DK2_h120 = (dispatch_hour[120,3]+dispatch_hour[120,4])/demand_hour[120,2]*100

# market prices
mp_high_wp = price_hour[650,]
mp_low_wp = price_hour[120,]

# scheduled power producers
png('images/dispatch_high_WP.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar=c(8, 4, 2, 2) + 0.1)
plot(1:18, dispatch_hour[650,1:18], type = "h", xlab = "", ylab = "[W]", lwd = 5, xaxt = "n")
lines(3:4, dispatch_hour[650,3:4], type = "h", lwd = 5, col = "red")
lines(18,dispatch_hour[650,18], type = "h", lwd = 5, col= "blue")
legend("topright",legend = c(paste0("generator producing at full capacity"), paste0("0 transmission"),
                             paste0("Wind penetration DK1 = ", wp_DK1_h650, "%"), paste0("Wind penetration DK2 = ", round(wp_DK2_h650), "%" )), 
       col = c("red","blue"), lwd = 5, lty = c(1,1,0,0), cex = 0.75)
axis(1, at=1:18, labels = participants[1:18], las = 2, cex.axis = 0.8)
title(main = "Production dispatch at high wind penetration (DK1 & DK2)")
dev.off()

png('images/dispatch_low_WP.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar=c(8, 4, 2, 2) + 0.1)
plot(1:18, dispatch_hour[120,1:18], type = "h", xaxt ="n", xlab = "", ylab = "[W]", lwd = 5, ylim = c(0,1200))
lines(5:6, dispatch_hour[120,5:6], type = "h", lwd = 5, col = "red")
lines(8:9, dispatch_hour[120,8:9], type = "h", lwd = 5, col = "red")
lines(15:17, dispatch_hour[120,15:17], type = "h", lwd = 5, col = "red")
lines(18, dispatch_hour[120,18], type = "h", lwd = 5, col = "blue")
legend("topleft",legend = c(paste0("generator producing at full capacity"), paste0("Transmission congested"),
                            paste0("Wind penetration DK1 = ", round(wp_DK1_h120), "%"), paste0("Wind penetration DK2 = ", round(wp_DK2_h120), "%" )), 
       col = c("red","blue"), lwd = 5, lty = c(1,1,0,0), cex = 0.75)
axis(1, at=1:18, labels = participants[1:18], las = 2, cex.axis = 0.8)
title(main = "Production dispatch at low wind penetration (DK1 & DK2)")
dev.off()

########################################################
########################################################
### merit order plot
source("functions/m_order.R")
source("functions/simple_merit_order_plot.R")

## high wind penetration
i = 4
b = c(WW1_pp*Wind$DK1[i], WW2_pp*Wind$DK1[i],EW1_pp*Wind$DK2[i], EW2_pp*Wind$DK2[i],
      400, 330, 345, 390, 510, Nuke22$G6_quantity[i], Nuke22$G7_quantity[i], 320, 360, 400, 350, 730, 630, transmission_cap, lsDK1, lsDK2)

# DK1
quantity = c(b[1:2],b[5:10])
price = c(f.obj[1:2],f.obj[5:10])
demand_dk1 = Consumption$DK1[i] + GermanyExport$Germany_quantity[i] - NorwayImport$Norway_Quantity[i]
data = data.frame(quantity = quantity, price = price)
data = data[order(data$price),]
new_quantity = m_order(data$quantity)
data[,1] = new_quantity
simple_merit_order_plot(data, paste0("Market clearing DK1 : i =  ", i ), TRUE, demand_dk1)

# png('images/MC_DK1_hwp.png', width = 580, height = 400, units = "px", pointsize = 12)
# simple_merit_order_plot(data, "Market clearing DK1 - high wind penetration", TRUE, demand_dk1)
# dev.off()

# DK2
quantity = c(b[3:4],b[11:17])
price = c(f.obj[3:4],f.obj[11:17])
demand_dk2 = Consumption$DK2[i] + SwedenExport$Sweden_quantity[i]
data = data.frame(quantity = quantity, price = price)
data = data[order(data$price),]
new_quantity = m_order(data$quantity)
data[,1] = new_quantity
simple_merit_order_plot(data, paste0("Market clearing DK2 : i =  ", i ), TRUE, demand_dk2)

# png('images/MC_DK2_hwp.png', width = 580, height = 400, units = "px", pointsize = 12)
# simple_merit_order_plot(data, "Market clearing DK2 - high wind penetration", TRUE, demand_dk2)
# dev.off()

## low wind penetration

i = 120 # low penetration
b = c(WW1_pp*Wind$DK1[i], WW2_pp*Wind$DK1[i],EW1_pp*Wind$DK2[i], EW2_pp*Wind$DK2[i],
      400, 330, 345, 390, 510, Nuke22$G6_quantity[i], Nuke22$G7_quantity[i], 320, 360, 400, 350, 730, 630, transmission_cap, lsDK1, lsDK2)

# DK1
quantity = c(b[1:2],b[5:10])
price = c(f.obj[1:2],f.obj[5:10])
demand_dk1 = Consumption$DK1[i] + GermanyExport$Germany_quantity[i] - NorwayImport$Norway_Quantity[i]
data = data.frame(quantity = quantity, price = price)
data = data[order(data$price),]
new_quantity = m_order(data$quantity)
data[,1] = new_quantity
# png('images/MC_DK1_lwp.png', width = 580, height = 400, units = "px", pointsize = 12)
simple_merit_order_plot(data, "Market clearing DK1 - low wind penetration", TRUE, demand_dk1)
# dev.off()

# DK2
quantity = c(b[3:4],b[11:17])
price = c(f.obj[3:4],f.obj[11:17])
demand_dk2 = Consumption$DK2[i] + SwedenExport$Sweden_quantity[i]
data = data.frame(quantity = quantity, price = price)
data = data[order(data$price),]
new_quantity = m_order(data$quantity)
data[,1] = new_quantity
# png('images/MC_DK2_lwp.png', width = 580, height = 400, units = "px", pointsize = 12)
simple_merit_order_plot(data, "Market clearing DK2 - low wind penetration", TRUE, demand_dk2)
# dev.off()

########################################################
########################################################
# Revenues calculation 
# We order Generators for DK1 and for DK2 
new_dispatch_hour = matrix(0,nrow = N_simulations, ncol = 17)
new_dispatch_hour[,1:2] = dispatch_hour[,1:2]
new_dispatch_hour[,3:8] = dispatch_hour[,5:10]
new_dispatch_hour[,9:10] = dispatch_hour[,3:4]
new_dispatch_hour[,11:17] = dispatch_hour[,11:17]

# Therefore the participants are : 

new_participants = c("WW1_DK1","WW2_DK1",
                 "Peako_DK1", "Peako_DK1",
                 "Nuke22_DK1", "FlexiGas_DK1", "FlexiGas_DK1", "FlexiGas_DK1",
                 "EW1_DK2","EW2_DK2", "Nuke22_DK2",
                 "RoskildeCHP_DK2", "RoskildeCHP_DK2",
                 "Avedovre_DK2", "Avedovre_DK2",
                 "BlueWater_DK2", "BlueWater_DK2") # "Transmission_line", "Shedding1", "Shedding2")

eq.price_hour = matrix(0,nrow = N_simulations, ncol = 17) 
eq.price_hour[,1:8] = price_hour[,1] # DK1
eq.price_hour[,9:17] = price_hour[,2] # DK2

# particular case of support 
eq.price_hour[,2] = eq.price_hour[,2] + 17 # prenium
eq.price_hour[,10] = 25 # feed in tariff

revenues_hour = matrix(0,nrow = N_simulations, ncol = 17)
for (i in 1:N_simulations){
  revenues_hour[i,] = eq.price_hour[i,]*new_dispatch_hour[i,]
}

# reset graphical parameters
png('images/hourly_revenues_from_wind_DK12.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar = c(5, 5,4, 2))
plot(1:N_simulations,(revenues_hour[,1]+revenues_hour[,2])/1000, type = 'l', xlab = "Time [h]", ylab = "Revenues [k€]")
lines(1:N_simulations,(revenues_hour[,9]+revenues_hour[,10])/1000, type = 'l', xlab = "Time [h]", ylab = "Revenues [k€]", col = "blue", lwd = 1.5)
legend("topleft", legend= c("DK1","DK2"), col = c("black", "blue"), lty = 1, cex = 0.75, lwd = 2)
title(main = "Hourly revenues from wind production")
dev.off()

##  
# Generators revenues when low wind penetration 
png('images/generator_dispatch_lwp.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar=c(8, 4, 2, 2) + 0.1)
plot(1:17, new_dispatch_hour[120,], type = "h", lwd = 5, xlab = "", xaxt ="n", ylab = "[MWh]" )
axis(1, at=1:20, labels = new_participants[1:20], las = 2, cex.axis = 0.8)
title(main="Generators dispatch when low wind penetration")
dev.off()

png('images/revenues_dispatch_lwp.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar=c(8, 4, 2, 2) + 0.1)
plot(1:17, revenues_hour[120,1:17]/1000, type = "h", lwd = 5, ylab = "[k€]", xlab = "", xaxt ="n")
axis(1, at=1:17, labels = new_participants[1:17], las = 2, cex.axis = 0.8)
title(main="Generators revenues when low wind penetration")
mtext(paste0("Eq. price (€) in DK1 - DK2 resp. : ", price_hour[120, 1], " - ", price_hour[120,2]), 1, line=7, col = "blue")
dev.off()

### Generators revenues when high wind penetration 
## Notes : BlueWater_DK2 produces 258 MWh but makes no revenues
## Notes : same for EW1_DK2 who produces 325 MWh but doesn't have any support therefore makes no revenues
png('images/generator_dispatch_hwp.png', width = 580, height = 400, units = "px", pointsize = 12)
plot(1:17, new_dispatch_hour[650,], type = "h", lwd = 5, xlab = "", xaxt ="n", ylab = "[MWh]" )
axis(1, at=1:17, labels = new_participants[1:17], las = 2, cex.axis = 0.8)
title(main="Generators dispatch when high wind penetration")
dev.off()

png('images/revenues_dispatch_hwp.png', width = 580, height = 400, units = "px", pointsize = 12)
par(mar=c(8, 4, 2, 2) + 0.1)
plot(1:17, revenues_hour[650,]/1000, type = "h", lwd = 5, ylab = "[k€]", xlab = "", xaxt ="n")
lines(9,revenues_hour[650,9]/1000, type = "h", col = "red", lwd = 5)
lines(16,revenues_hour[650,16]/1000, type = "h", col = "red", lwd = 5)
axis(1, at=1:17, labels = new_participants[1:17], las = 2, cex.axis = 0.8)
title(main="Generators revenues when high wind penetration")
mtext(paste0("Eq. price (€) in DK1 - DK2 resp. : ", price_hour[650, 1], " - ", price_hour[650,2]), 1, line=7, col = "blue")
dev.off()

# plot average daily revenue for each produceur 
# compute overall revenue for each participant in each market
