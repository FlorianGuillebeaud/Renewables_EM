## Assignment 1 ## 
## Renewables in electricity market ## 
## Author : Florian Guillebeaud ## 

########################################################
########################################################

## Workspace directory : put the 31761_Assignment1_Guillebeaud_Dziedzioch folder on the Desktop of your computer
setwd("~/Desktop/31761-Assignment1-Guillebeaud-Dziedzioch/R_code")

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

## 
N_simulations = 31*24 # January hourly
transmission_cap = 650 # MWh
transmission_step = 50 
shedding = 0 # initialisation

while( shedding == 0)
{
  transmission_cap = transmission_cap - transmission_step
  
  ## Save the results 
  dispatch_hour = matrix(0,nrow = N_simulations, ncol = 20)
  price_hour_new = matrix(0,nrow = N_simulations, ncol = 2)
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
    
    set.objfn(lps.model, c(0, -17, 0, -25, 70, 64, 153, 82, 89, Nuke22$G6_price[i], Nuke22$G7_price[i], 43, 39, 36, 31, 5, 10, 0, 10000, 10000))
    set.constr.type(lps.model, c(rep("<=", 20),"=", "=") )
    # 
    b = c(WW1_pp*Wind$DK1[i], WW2_pp*Wind$DK1[i],EW1_pp*Wind$DK2[i], EW2_pp*Wind$DK2[i],
          400, 330, 345, 390, 510,Nuke22$G6_quantity[i], Nuke22$G7_quantity[i], 320, 360, 400, 350, 730, 630, transmission_cap, lsDK1= 10000, lsDK2=10000)
    beq_DK1 = Consumption$DK1[i] + GermanyExport$Germany_quantity[i] - NorwayImport$Norway_Quantity[i]
    beq_DK2 = Consumption$DK2[i] + SwedenExport$Sweden_quantity[i]
    beq = c(beq_DK1, beq_DK2)
    # 
    # 
    set.rhs(lps.model, c(b,beq))
    set.bounds(lps.model, lower = -transmission_cap, columns = 18)
    
    solve(lps.model)
    dispatch_hour[i,] <- get.variables(lps.model)
    demand_hour[i,1] = beq_DK1
    demand_hour[i,2] = beq_DK2
    price_hour_new[i,1] <- get.dual.solution(lps.model)[22]
    price_hour_new[i,2] <- get.dual.solution(lps.model)[23]
    
  }
  
  shedding = sum(colSums(dispatch_hour[,19:20]))
  # Influence of the transmission
  # DK1
  DK1_dispatch = rowSums(dispatch_hour[425:435,1:2])+rowSums(dispatch_hour[425:435,5:10])
  # make labels and margins smaller
  par(cex=1, mai=c(0.05,0.01,0.2,0.6))
  # define area for the histogram
  par(fig=c(0.1,0.8,0.2,0.9))
  plot(425:435, demand_hour[425:435,1], type = 'h',lend="square", ylim = c(-transmission_cap-1700,3500),
       xlab = "Time [h]", ylab = "[MW]", lwd = 15, cex.lab = 1.5, cex.axis = 1.2, col = "black")
  lines(425:435,DK1_dispatch,
        col = "grey", lwd = 15, type = "h",lend="square")
  lines(424.8:434.8, Wind$DK1[425:435],col = "blue", lwd = 15, type = "h",lend="square" )
  lines(425.2:435.2,dispatch_hour[425:435,1]+dispatch_hour[425:435,2], col = "darkorange", 
        lwd = 15, type = "h", lend = "square")
  abline(h=-transmission_cap, col ="red", lty = 2, lwd = 2)
  abline(h=transmission_cap, col ="red", lty = 2, lwd = 2)
  legend("bottomleft", legend = c("Electricity demand in DK1", "Electricity prod. in DK1",
                                  "Electricity prod. from wind","Wind offers"),
         col = c("black", "grey", "orange", "blue"), lty = c(1,1,1,1), cex = 1, lwd = 4 )
  legend("topright", legend = paste0("Transmission cap : +/- ", transmission_cap, "MWh"), col = "red", lty = 1)
  # title(main = paste0("Study case when low wind penetration using cable of : ", transmission_cap))
  
  # define area for the boxplot
  par(fig=c(0.85,1,0.1,1), new=TRUE)
  plot(431, demand_hour[431,1], type = 'h',lend="square", ylim = c(2000,2200), xaxt ="n",
       ylab = "[MW]", lwd = 15, cex.lab = 1.5, cex.axis = 1.2, col = "black")
  lines(431,DK1_dispatch[7],
        col = "grey", lwd = 15, type = "h",lend="square")
  segments(431,DK1_dispatch[7],
           431,DK1_dispatch[7]+dispatch_hour[431,18],  col = "red",lend="square", lwd = 15)
}
