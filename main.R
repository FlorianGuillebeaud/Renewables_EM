## Assignment 1 ## 
## Renewables in electricity market ## 
## Author : Florian Guillebeaud ## 

## Workspace directory
setwd("~/Documents/DTU/B_Semester-2/31761_Renew_ElectricityMarkets/Assignments/Assignment1/New_try")

## Install packages if not installed
# install.packages("ggplot2")
# install.packages("lpSolveAPI")

## Read packages if already installed 
library(ggplot2)
library(linprog)
library(lpSolveAPI)

# read function
source("functions/pie2.R")
source("functions/m_order.R") # orders the bid to enter in market clearing
source("functions/simple_merit_order_plot.R") # plot the market clearing

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

## January - hourly
N_simulations = 31*24

## Transmission capacity MWh
transmission_cap = 600 

## Save the results 
dispatch_hour = matrix(0,nrow = N_simulations, ncol = 20)
price_hour_new = matrix(0,nrow = N_simulations, ncol = 2)
demand_hour = matrix(0,nrow = N_simulations, ncol = 2)

## DK1 = WEST / DK2 = EAST
for (i in 1:N_simulations){
  
  # Build the lp mode : # 17 generators + 1 transmission + shedding capacity in both area
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
  
  # Define the objective function (bid prices / trnasmission / shedding)
  set.objfn(lps.model, c(0, -17, 0, -25, 70, 64, 153, 82, 89, Nuke22$G6_price[i], Nuke22$G7_price[i], 43, 39, 36, 31, 5, 10, 0, 10000, 10000))
  
  # Define the contraint vector of 20 inequalities and 2 equalities (2 demand) 
  set.constr.type(lps.model, c(rep("<=", 20),"=", "=") )
  
  # Define the capacity of each participant for the inequalities
  b = c(WW1_pp*Wind$DK1[i], WW2_pp*Wind$DK1[i],EW1_pp*Wind$DK2[i], EW2_pp*Wind$DK2[i],
        400, 330, 345, 390, 510,Nuke22$G6_quantity[i], Nuke22$G7_quantity[i], 320, 360, 400, 350, 730, 630, transmission_cap, lsDK1= 10000, lsDK2=10000)
  
  # Define the 2 demand for the equalities 
  beq_DK1 = Consumption$DK1[i] + GermanyExport$Germany_quantity[i] - NorwayImport$Norway_Quantity[i]
  beq_DK2 = Consumption$DK2[i] + SwedenExport$Sweden_quantity[i]
  beq = c(beq_DK1, beq_DK2)
  
  # Right hand side constraint merge the previous two vectors b and beq
  set.rhs(lps.model, c(b,beq))
  
  # Define the -transmission_cap boundary limit 
  set.bounds(lps.model, lower = -transmission_cap, columns = 18)
  
  # solve !
  solve(lps.model)
  
  # Who participates
  dispatch_hour[i,] <- get.variables(lps.model)
  
  # facing what demand
  demand_hour[i,1] = beq_DK1
  demand_hour[i,2] = beq_DK2
  
  # at what price
  price_hour_new[i,1] <- get.dual.solution(lps.model)[22] # happens to be the 22nd and 23rd column, don't know why...
  price_hour_new[i,2] <- get.dual.solution(lps.model)[23]
  
}

########################################################
########################################################

# here we look at the usage of the transmission
count = 0
for (i in 1:N_simulations) if (abs(dispatch_hour[i,18])==600) count = count + 1
cat(count/N_simulations*100) # return the % of time when transmission is fully used

par(mar = c(6, 4.5,2, 2)) 
plot(1:N_simulations, dispatch_hour[,18], type = 'h', xlab = "Time [h]", ylab = "[MW]", col = "black", ylim = c(-800,600), cex.lab = 1.5)
abline(h = transmission_cap, col ="black", lty = 2, lwd = 2)
abline(h = -transmission_cap, col ="black", lty = 2, lwd = 2)
legend("bottom", legend = c("(+) : DK2 => DK1", "(-) : DK2 <= DK1"), cex = 1, lty = 1, col = "darkgrey", lwd = 2)
title(main = "Transmission", sub = paste0("Congestion occurs : ", round(count/N_simulations*100)," % of the time"), col.sub = "black")
dev.off()

DK12_transmission = DK21_transmission = matrix(0, nrow = 24, ncol = 31)
k = 1
for (i in 1:(N_simulations-1)){
  j = i%%24 
  
  if (j != 0){
    k = k
    if (dispatch_hour[i,18] >= 0){
      DK21_transmission[j,k] = dispatch_hour[i,18]/transmission_cap*100 
    } else{
      DK12_transmission[j,k] = -dispatch_hour[i,18]/transmission_cap*100
    }
  }else{
    k=k+1
    if (dispatch_hour[i,18] >= 0){
      DK21_transmission[24,k-1] = dispatch_hour[i,18]/transmission_cap*100 
    } else{
      DK12_transmission[24,k-1] = -dispatch_hour[i,18]/transmission_cap*100
    }
  }
}

# How often the transmission is congested 
par(mar = c(4.5, 4.5,2, 2))
plot(1:24, rowSums(DK21_transmission[1:24,]==100)/31*100, type = 'h', lwd = 15, ylim = c(0,100), xlab = "Time slot", ylab = "[%]", cex.axis = 1.75, cex.lab = 1.5)
lines(1:24, rowSums(DK12_transmission[1:24,]==100)/31*100, type = 'h', lwd = 12, ylim = c(0,100), col = "grey")
legend("topleft", legend = c("DK2 => DK1", "DK1 => DK2"), col = c("black", "grey"), lwd = 5, lty = 1, cex = 1.25)
title(main = "Congestion of the transmission cable")

# Pie representation of the congestion 
global_congestion = round(sum(dispatch_hour[,18]==abs(600))/length(dispatch_hour[,18])*100, digits = 1)
slices = c(global_congestion, 100-global_congestion)
lbls = c(paste0("Congestion : ", global_congestion, " %"), paste0("No Congestion : ", 100-global_congestion, " %"))
pie(slices,lbls, col=c("black", "grey"))
dev.off()

########################################################
########################################################
# We look at the dispatch here
# We order Generators for DK1 and for DK2 
new_dispatch_hour = matrix(0,nrow = N_simulations, ncol = 17)
new_dispatch_hour[,1:2] = dispatch_hour[,1:2]
new_dispatch_hour[,3:8] = dispatch_hour[,5:10]
new_dispatch_hour[,9:10] = dispatch_hour[,3:4]
new_dispatch_hour[,11:17] = dispatch_hour[,11:17]

# Therefore the participants are : 
new_participants = c("WW1","WW2",
                     "FlexiGas", "FlexiGas", "FlexiGas",
                     "Peako", "Peako","Nuke22",
                     "EW1","EW2", "Nuke22",
                     "RoskildeCHP", "RoskildeCHP",
                     "Avedovre", "Avedovre",
                     "BlueWater", "BlueWater") # "Transmission_line", "Shedding1", "Shedding2")


# schedule of conventional generators  
par(mar=c(8, 4, 2, 2) + 0.1)
generator_schedule = (colSums(new_dispatch_hour[1:N_simulations,]>0)[1:17])/N_simulations*100
plot(1:17, generator_schedule, type = "h", lend="square", lwd = 10, xlab = "", xaxt = "n", ylab = "[%]", yaxt ="n", ylim = c(0,140))
ylabel <- seq(0, 100, by = 25)
axis(2, at = ylabel)
lines(1:2, generator_schedule[1:2], type = "h", lend="square",col ="blue", lwd =10)
lines(3:4, generator_schedule[3:4], type = "h", lend="square",col ="black", lwd =10)
lines(5, generator_schedule[5], type = "h",lend="square", col ="red", lwd =10)
lines(6:8, generator_schedule[6:8], type = "h", lend="square",col ="black", lwd =10)
lines(9:10, generator_schedule[9:10], type = "h", lend="square",col ="blue", lwd =10)
lines(11, generator_schedule[11], type = "h", lend="square",col ="black", lwd =10)
lines(12, generator_schedule[12], type = "h", lend="square",col ="red", lwd =10)
lines(13:15, generator_schedule[13:15], type = "h", lend="square",col ="black", lwd =10)
lines(16:17, generator_schedule[16:17], type = "h", lend="square",col ="blue", lwd =10)
axis(1, at=1:17, labels = new_participants[1:17], las = 2, cex.axis = 1.2, lwd = 2)
abline(v = 8.5, type = 'l' )
legend("topleft", legend = "DK1")
legend("topright", legend = "DK2" )
legend("top", legend = c("Renewable generators", "Conventional generators","Conventional generators never scheduled"),
       col = c("blue", "black", "red"), lty = 1, lwd = 3, cex = 0.75)
title(main="Generation scheduled over January")

# Pie representation with legend coming after the pie plot
color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
sample_color = sample(color,8)
par(mar = c(4.5, 4.5,2, 2))
slices = (colSums(new_dispatch_hour[,1:8])/sum(colSums(new_dispatch_hour[,1:8])))*100
lbls = paste0(round(slices, digits = 2), " %")
labls = new_participants[1:8]
pie2(slices, labels = "", col = sample_color, radius = 0.8)
title(main ="DK1")
plot(1, type="n", axes=FALSE, xlab="", ylab="") 
legend(0.75, 1.41, paste0(new_participants[1:8], ": ", round(slices, digits = 1), " %"), cex = 1.75, fill = sample_color)

color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
sample_color = sample(color,9)
par(mar = c(4.5, 4.5,2, 2))
slices = (colSums(new_dispatch_hour[,9:17])/sum(colSums(new_dispatch_hour[,9:17])))*100
lbls = paste0(round(slices, digits = 3), " %")
labls = new_participants[9:17]
pie2(slices, labels = "",col = sample_color, radius = 0.8)
title(main = "DK2")
plot(1, type="n", axes=FALSE, xlab="", ylab="") 
legend(0.75, 1.42, paste0(new_participants[9:17], ": ", round(slices, digits = 3), " %"), cex = 1.75, fill = sample_color)

########################################################
########################################################
# We focus on the wind curtailement in DK1 
par(mar = c(5, 5,4, 5))
plot(1:N_simulations, Wind$DK1, type = 'h', lwd = 5, xlab = "Time [h]", ylab = "[MWh]")
lines(1:N_simulations, dispatch_hour[,1]+dispatch_hour[,2], col = "grey", type = 'h', lwd = 5)
legend("bottomright", legend = c("Wind offers", "Wind production"), col = c("black", "grey"), lwd = 2, lty = 1, cex = 0.75)
title(main = "Wind Curtailment in DK1")

curtail_DK1 = round(Wind$DK1 - dispatch_hour[,1] - dispatch_hour[,2])
curtail_on = sum(curtail_DK1 !=0)/length(curtail_DK1)
curtail_off = 1 - curtail_on
slices = c(curtail_on, curtail_off)
lbls = c(paste0("wind curtailment : ", round(curtail_on*100), " %"), paste0("full production : ",round(curtail_off*100), " %"))

# pie representation 
par(mar = c(5, 5,4, 5))
pie(slices, lbls, col=c("blue", "grey"), main = "Wind curtailment DK1")

# We focus on the wind curtailement in DK2
par(mar = c(5, 5,4, 5))
plot(1:N_simulations, Wind$DK2, type = 'h', lwd = 5, xlab = "Time [h]", ylab = "[MWh]")
lines(1:N_simulations, dispatch_hour[,3]+dispatch_hour[,4], col = "grey", type = 'h', lwd = 5)
legend("bottomright", legend = c("Wind offers", "Wind production"), col = c("black", "grey"), lwd = 2, lty = 1, cex = 0.75)
title(main = "Wind Curtailment in DK2")

curtail_DK2 = round(Wind$DK2 - dispatch_hour[,3] - dispatch_hour[,4])
curtail_on = sum(curtail_DK2 !=0)/length(curtail_DK2)
curtail_off = 1 - curtail_on
slices = c(curtail_on, curtail_off)
lbls = c(paste0("wind curtailment : ", round(curtail_on*100), " %"), paste0("full production : ", round(curtail_off*100), " %"))

# pie representation 
par(mar = c(5, 5,4, 5))
pie(slices, lbls, col=c("blue", "grey"), main = "Wind curtailment DK2")

########################################################
########################################################
# The influence of the transmission is studied here

# For DK1 between the hour 620 and 660
par(mar = c(4.5, 4.5,2, 2))
plot(620:660, demand_hour[620:660,1], type = 'h', ylim = c(-2500,3500), xlab = "Time [h]", ylab = "[MW]", lwd = 10, cex.lab = 1.5, cex.axis = 1.2)
lines(620:660,dispatch_hour[620:660,1]+dispatch_hour[620:660,2]+dispatch_hour[620:660,5]+dispatch_hour[620:660,6]+dispatch_hour[620:660,7]+dispatch_hour[620:660,8]+dispatch_hour[620:660,9]+dispatch_hour[620:660,10], col = "grey", lwd = 4, type = "h")
lines(620:660, Wind$DK1[620:660],col = "blue", lwd = 4, type = "h" )
lines(620:660,dispatch_hour[620:660,1]+dispatch_hour[620:660,2], col = "darkorange", lwd = 4, type = "h")
lines(620:660, dispatch_hour[620:660,18], type = "h", xlab = "Time [h]", ylab = "[MWh]", col = "green", lwd = 4)
abline(h=-600, col ="red", lty = 2, lwd = 2)
abline(h=600, col ="red", lty = 2, lwd = 2)
legend("bottomleft", legend = c("Electricity demand in DK1", "Electricity prod. in DK1",
                                "Electricity prod. from wind", "Transmission to DK2 (negative = export to DK2)",
                                "Wind availabilities", "Transmission cap"), 
       col = c("black", "grey", "orange","green", "blue","red"), lty = c(1,1,1,1,1,2), cex = 0.75, lwd = 4 )
title(main = "Study case when high wind penetration")

########################################################
########################################################
### Market clearing for two specific hours

# high wind penetration h = 650
i = 650
b = c(WW1_pp*Wind$DK1[i], WW2_pp*Wind$DK1[i],EW1_pp*Wind$DK2[i], EW2_pp*Wind$DK2[i],
      400, 330, 345, 390, 510, Nuke22$G6_quantity[i], Nuke22$G7_quantity[i], 320, 360, 400, 350, 730, 630, transmission_cap, 10000, 10000)
f.obj = c(0, -17, 0, -25, 70, 64, 153, 82, 89, Nuke22$G6_price[i], Nuke22$G7_price[i], 43, 39, 36, 31, 5, 10, 0, 10000, 10000)

# DK1
quantity = c(b[1:2],b[5:10])
price = c(f.obj[1:2],f.obj[5:10])
demand_dk1 = Consumption$DK1[i] + GermanyExport$Germany_quantity[i] - NorwayImport$Norway_Quantity[i]
data_dk1 = data.frame(quantity = quantity, price = price)
data_dk1 = data_dk1[order(data_dk1$price),]
new_quantity = m_order(data_dk1$quantity)
data_dk1[,1] = new_quantity
simple_merit_order_plot(data_dk1, paste0("Market clearing DK1 : hour =  ", i ), TRUE, demand_dk1)

# DK2
quantity = c(b[3:4],b[11:17])
price = c(f.obj[3:4],f.obj[11:17])
demand_dk2 = Consumption$DK2[i] + SwedenExport$Sweden_quantity[i]
data_dk2 = data.frame(quantity = quantity, price = price)
data_dk2 = data_dk2[order(data_dk2$price),]
new_quantity = m_order(data_dk2$quantity)
data_dk2[,1] = new_quantity
simple_merit_order_plot(data_dk2, paste0("Market clearing DK2 : high wind penetration hour =  ", i ), TRUE, demand_dk2)

## low wind penetration at h=431
i = 431 # low penetration
b = c(WW1_pp*Wind$DK1[i], WW2_pp*Wind$DK1[i],EW1_pp*Wind$DK2[i], EW2_pp*Wind$DK2[i],
      400, 330, 345, 390, 510, Nuke22$G6_quantity[i], Nuke22$G7_quantity[i], 320, 360, 400, 350, 730, 630, transmission_cap, 10000, 10000)
f.obj = c(0, -17, 0, -25, 70, 64, 153, 82, 89, Nuke22$G6_price[i], Nuke22$G7_price[i], 43, 39, 36, 31, 5, 10, 0, 10000, 10000)

# DK1
quantity = c(b[1:2],b[5:10])
price = c(f.obj[1:2],f.obj[5:10])
demand_dk1 = Consumption$DK1[i] + GermanyExport$Germany_quantity[i] - NorwayImport$Norway_Quantity[i]
data_dk1 = data.frame(quantity = quantity, price = price)
data_dk1 = data_dk1[order(data_dk1$price),]
new_quantity = m_order(data_dk1$quantity)
data_dk1[,1] = new_quantity
simple_merit_order_plot(data_dk1, paste0("Market clearing DK1 : low wind penetration hour =  ", i ), TRUE, demand_dk1)

# DK2
quantity = c(b[3:4],b[11:17])
price = c(f.obj[3:4],f.obj[11:17])
demand_dk2 = Consumption$DK2[i] + SwedenExport$Sweden_quantity[i]
data_dk2 = data.frame(quantity = quantity, price = price)
data_dk2 = data_dk2[order(data_dk2$price),]
new_quantity = m_order(data_dk2$quantity)
data_dk2[,1] = new_quantity
simple_merit_order_plot(data_dk2, paste0("Market clearing DK2 : low wind penetration hour =  ", i ), TRUE, demand_dk2)
