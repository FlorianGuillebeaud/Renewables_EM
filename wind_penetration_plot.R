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

# Penetration plots study
########################################################
########################################################

# General wind penetration 
par(mar = c(4.5, 4.5,2, 2))
wp = ((dispatch_hour[,1]+dispatch_hour[,2]+dispatch_hour[,3]+dispatch_hour[,4])/(demand_hour[,1]+demand_hour[,2]))*100
mean_wp = mean(wp)

plot(1:N_simulations, wp, 
     type = 'l', ylim = c(-5,105), xlab = "Time [h]", ylab = "[%]", lwd = 2)
abline(h = mean_wp,
       col = "blue")
legend("bottomright", legend = paste0("Mean penetration over January : ", round(mean_wp), " %" ), lty = 1, col = "blue")
title(main = "Wind penetration in DK")

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

par(mar = c(4.5, 4.5,2, 2))
plot(1:N_simulations, (dispatch_hour[,1]+dispatch_hour[,2])/demand_hour[,1]*100, type = 'l', ylim = c(-5,135),
     xlab = "Time [h]", ylab = "[%]", lwd = 2)
title(main = "Wind penetration in DK1")

# confronting wind penetration and prices in DK1
par(mar = c(4.5, 4.5,2,4.5))
plot(150:400, (dispatch_hour[150:400,1]+dispatch_hour[150:400,2]), type = 'l',
     xlab = "Time [h]", ylab = "[MWh]", lwd = 2, cex.lab = 2)
par(new = TRUE)
plot(150:400, price_hour_new[150:400,1], col = "blue", lwd = 2, type = "l", xlab = "", ylab ="", yaxt = "n")
axis(4, col = "blue")
mtext("[€/MWh]", col = "blue", side=4, line=2.5, cex = 2)
title(main = "Wind production vs prices DK1")


## DK2
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

par(mar = c(4.5, 4.5,2, 2))
plot(1:N_simulations, (dispatch_hour[,3]+dispatch_hour[,4])/demand_hour[,2]*100, type = 'l', ylim = c(-5,105),
     xlab = "Time [h]", ylab = "[%]", col ="black", lwd = 2)
title(main = "Wind penetration in DK2")