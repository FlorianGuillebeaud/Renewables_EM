## Assignment 1 ## 
## Renewables in electricity market ## 
## Author : Florian Guillebeaud ## 

########################################################
########################################################
# By sourcing the main, you will be able to source this code 
# to have the study on the prices

# if not already sourced, de-comment the next line
# source("main.R")
########################################################
########################################################


# DK1 
# General Demand vs wind offers DK1
par(mar = c(4.5, 4.5,2, 2))
plot(1:N_simulations, demand_hour[,1], type = "l", ylim = c(0,4000), lwd = 2,
     xlab = "Time [h]", ylab = "[MWh]")
lines(1:N_simulations, Wind$DK1, col = "blue", lwd = 2)
legend("topleft", legend = c("Electricity Demand DK1", "Wind offers DK1"), col=c("black","blue"),lty = 1, lwd = 2, cex = 0.75)
title(main = "Demand vs offers DK1")

# DK2
# General Demand vs wind offers DK2
plot(1:N_simulations, demand_hour[,2], type = "l", ylim = c(0,3000), lwd = 2,
     xlab = "Time [h]", ylab = "[MWh]")
lines(1:N_simulations, Wind$DK2, col = "blue", lwd = 2)
legend("topleft", legend = c("Electricity Demand DK2", "Wind offers DK2"), col=c("black","blue"),lty = 1, lwd = 2, cex = 0.75)
title(main = "Demand vs offers DK2")

########################################################
# Global
demand_day_dk1 = matrix(0, nrow = 24, ncol = 31)
demand_day_dk2 = matrix(0, nrow = 24, ncol = 31)

k = 1
for (i in 1:(N_simulations-1)){
  j = i%%24 
  if (j != 0){
    k = k
    demand_day_dk1[j,k] = demand_hour[i,1]
    demand_day_dk2[j,k] = demand_hour[i,2]
  }else{
    k=k+1
    demand_day_dk1[24,k-1] = demand_hour[i,1]
    demand_day_dk2[24,k-1] = demand_hour[i,2]
    
  }
}

plot(1:24, (rowSums(demand_day_dk1[1:24,])/31), type = 'h', lwd = 15, ylim = c(0,4000), xlab = "Time slot", ylab = "[MWh]", cex.axis = 1.75, cex.lab = 1.5)
lines(1:24, rowSums(demand_day_dk2[1:24,])/31, type = 'h', lwd = 12, ylim = c(0,100), col = "grey")
legend("topleft", legend = c("DK1", "DK2"), col = c("black", "grey"), lwd = 5, lty = 1, cex = 1.25)
title(main = "Average demand DK12")