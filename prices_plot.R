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


# DK1 let's look at the price evolution and statistics
plot(1:N_simulations, price_hour_new[,1], xlab = "Time [h]", ylab = "Electricity price [Eur/MWh]",ylim = c(-20, max(price_hour_new[,1])+50), type = "l", col = "black", lwd = 1.5)
abline( h = max(price_hour_new[,1]), col = "red", lwd = 2)
abline( h = mean(price_hour_new[,1]), col = "red", lty = 3, lwd = 3)
abline(h=0)
legend("topleft", legend= c("DK1", paste0("Max price : ", max(price_hour_new[,1]), " €/MWh"), paste0("Mean price over the month: ", round(mean(price_hour_new[,1])), " €/MWh")),
       col = c("black", "red","red"), lty = c(1,1,3), lwd = 2, cex = 0.75)
title(main = "DK1 : electricity prices evolution")

# DK1 negative prices statistics
negative_on_DK1 = round(sum(price_hour_new[,1]<0)/length(price_hour_new[,1])*100, digits = 1)
negative_off_DK1 = (1 - negative_on_DK1/100)*100
slices_DK1 = c(negative_on_DK1, negative_off_DK1)
lbls_DK1 = c(paste0("Neg. prices : ", negative_on_DK1, " %"), paste0(negative_off_DK1, " %"))
par(mar = c(5, 5,4, 5))
pie(slices_DK1, lbls_DK1, main =" DK1 January prices", col=c("blue", "grey"))

# same for DK2 
plot(1:N_simulations, price_hour_new[,2], xlab = "Time [h]", ylab = "Electricity price [Eur/MWh]",ylim = c(-20, max(price_hour_new[,2])+20), type = "l", col = "black", lwd = 1.5)
abline( h = max(price_hour_new[,2]), col = "red", lwd = 2)
abline( h = mean(price_hour_new[,2]), col = "red", lty = 3, lwd = 3)
abline(h=0)
legend("topleft", legend= c("DK2", paste0("Max price : ", max(price_hour_new[,1]), " €/MWh"), paste0("Mean price over the month: ", round(mean(price_hour_new[,1])), " €/MWh")),
       col = c("black", "red","red"), lty = c(1,1,3), lwd = 2, cex = 0.75)
title(main = "DK2 : electricity prices evolution")

# DK2 negative prices statistics
negative_on_DK2 = round(sum(price_hour_new[,2]<0)/length(price_hour_new[,2])*100, digits = 1)
negative_off_DK2 = (1 - negative_on_DK2/100)*100
slices_DK2 = c(negative_on_DK2, negative_off_DK2)
lbls_DK2 = c(paste0("Neg. prices : ", negative_on_DK2, " %"), paste0(negative_off_DK2, " %"))
par(mar = c(5, 5,4, 5))
pie(slices_DK2, lbls_DK2, main =" DK2 January prices", col=c("blue", "grey"))

# Summary plot
par(mfrow=c(2,2))
par(mar = c(2,2,2,2))
pie(slices_DK1, lbls_DK1, main =" DK1 January prices", col=c("blue", "grey"), cex = 1, cex.main = 2)
par(mar = c(2,2,2,4))
pie(slices_DK2, lbls_DK2, main =" DK2 January prices", col=c("blue", "grey"), cex = 1, cex.main = 2)
par(mar=c(2, 4.5, 0, 0.5))
plot(1:N_simulations, price_hour_new[,1], xlab = "Time [h]", ylab = "Electricity price [Eur/MWh]",ylim = c(-20, max(price_hour_new[,1])+50), type = "l", col = "black", lwd = 1.5)
abline( h = max(price_hour_new[,1]), col = "red", lwd = 2)
abline( h = mean(price_hour_new[,1]), col = "red", lty = 3, lwd = 3)
abline(h=0)
legend("topleft", legend= c("DK1", paste0("Max price : ", max(price_hour_new[,1]), " €/MWh"), paste0("Mean price over the month: ", round(mean(price_hour_new[,1])), " €/MWh")),
       col = c("black", "red","red"), lty = c(1,1,3), lwd = 2, cex =1.5)
par(mar=c(2, 4.5, 0, 0.5))
plot(1:N_simulations, price_hour_new[,2], xlab = "Time [h]", ylab = "",ylim = c(-20, max(price_hour_new[,2])+30), type = "l", col = "black", lwd = 1.5)
abline( h = max(price_hour_new[,2]), col = "red", lwd = 2)
abline( h = mean(price_hour_new[,2]), col = "red", lty = 3, lwd = 3)
abline(h=0)
legend("topleft", legend= c("DK2", paste0("Max price : ", max(price_hour_new[,2]), " €/MWh"), paste0("Mean price over the month: ", round(mean(price_hour_new[,2])), " €/MWh")),
       col = c("black", "red","red"), lty = c(1,1,3), lwd = 2, cex = 1.5)
title(main="Summary prices for DK1 ad DK2")

########################################################
## Average price per time slot 
prices_day_dk1 = matrix(0, nrow = 24, ncol = 31)
prices_day_dk2 = matrix(0, nrow = 24, ncol = 31)

k = 1
for (i in 1:(N_simulations-1)){
  j = i%%24 
  
  if (j != 0){
    k = k
    prices_day_dk1[j,k] = price_hour_new[i,1]
    prices_day_dk2[j,k] = price_hour_new[i,2]
  }else{
    k=k+1
    prices_day_dk1[24,k-1] = price_hour_new[i,1]
    prices_day_dk2[24,k-1] = price_hour_new[i,2]
    
  }
}

par(mar = c(4.5, 4.5,2, 2))
plot(1:24, apply(prices_day_dk1, 1,mean), type = 'l', lwd = 3, ylim = c(-25,120), 
     xlab = "Time slot", ylab = "[€/MWh]", cex.axis = 2, cex.lab = 2, col = "purple")
abline(h=seq(-20,120,10), lty = 2, col = "grey")
abline(v = seq(0,24,1), lty = 2, col = "grey")
lines(1:24, apply(prices_day_dk2, 1,mean), type = 'l', lwd = 3, ylim = c(0,100), col = "green")
lines(1:24,apply(prices_day_dk1, 1,max), col = "purple", lwd = 2, type = "o")
lines(1:24,apply(prices_day_dk2, 1,max), col = "green", lwd = 2, type = "o" )
lines(1:24,apply(prices_day_dk1, 1,min), col = "purple", lwd = 2, lty = 2)
lines(1:24,apply(prices_day_dk2, 1,min), col = "green", lwd = 2, lty = 2)

legend("topleft", legend = c("DK1", "DK2"), col = c("purple","green"), lwd = 3, lty = 1, cex = 1)
legend("topright", legend = c("Max", "Min"), col = c("black"), lwd = 2, pch = c("o",""), lty = c(1,2), cex = 1.)
title(main ="Prices evolution in January")
