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

# Revenues plots
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


#
eq.price_hour = matrix(0,nrow = N_simulations, ncol = 17) 
eq.price_hour[,1:8] = price_hour_new[,1] # DK1
eq.price_hour[,9:17] = price_hour_new[,2] # DK2

# particular case of support 
eq.price_hour[,2] = eq.price_hour[,2] + 17 # premium
eq.price_hour[,10] = 25 # feed in tariff

revenues_hour = matrix(0,nrow = N_simulations, ncol = 17)
for (i in 1:N_simulations){
  revenues_hour[i,] = eq.price_hour[i,]*new_dispatch_hour[i,]
}

par(mar = c(5, 5,4, 2))
plot(1:N_simulations,(revenues_hour[,1]+revenues_hour[,2])/1000, type = 'h', xlab = "Time [h]", ylab = "Revenues [k€]", lwd = 5)
lines(1:N_simulations,(revenues_hour[,9]+revenues_hour[,10])/1000, type = 'h', xlab = "Time [h]", ylab = "Revenues [k€]", col = "grey", lwd = 3)
legend("topleft", legend= c("DK1","DK2"), col = c("black", "grey"), lty = 1, cex = 0.75, lwd = 2)
title(main = "Hourly revenues from wind production")

# high wind penetration i = 650
par(mar=c(8, 4, 2, 2) + 0.1)
plot(1:17, new_dispatch_hour[650,], type = "h", lend="square", lwd = 10, xlab = "", xaxt ="n", ylab = "[MWh]" ,ylim = c(0,2800))
axis(1, at=1:17, labels = new_participants[1:17], las = 2, cex.axis = 1.2, lwd=2)
abline(v = 8.5, type = 'l' )
legend("topleft", legend = "DK1 ")
legend("topright", legend = "DK2")
title(main="Generators dispatch when high wind penetration")

par(mar=c(8, 4, 2, 2) + 0.1)
plot(1:17, revenues_hour[650,]/1000, type = "h",lend="square", lwd = 10, ylab = "[k€]", xlab = "", xaxt ="n")
lines(2,revenues_hour[650,2]/1000, type = "h", col = "red", lwd = 10, lend="square")
axis(1, at=1:17, labels = new_participants[1:17], las = 2, cex.axis = 1.2, lwd= 2)
title(main="Generators revenues when high wind penetration")
abline(v = 8.5, type = 'l' )
legend("topleft", legend = "DK1 ")
legend("topright", legend = "DK2")
mtext(paste0("Eq. price (€) in DK1 - DK2 resp. : ", price_hour_new[650,1], " / ", price_hour_new[650,2]), 1, line=7, col = "blue")

# low wind penetration i = 431
# Generators revenues when low wind penetration 
par(mar=c(8, 4, 2, 2) + 0.1)
plot(1:17, new_dispatch_hour[431,], type = "h",lend="square", lwd = 10, xlab = "", xaxt ="n", ylab = "[MWh]", ylim = c(0,1000) )
axis(1, at=1:20, labels = new_participants[1:20], las = 2, cex.axis = 1.2, lwd = 2)
abline(v = 8.5, type = 'l' )
legend("topleft", legend = "DK1 ")
legend("topright", legend = "DK2")
title(main="Generators dispatch when low wind penetration")

par(mar=c(8, 4, 2, 2) + 0.1)
plot(1:17, revenues_hour[431,1:17]/1000, type = "h", lend="square", lwd = 10, ylab = "[k€]", xlab = "", xaxt ="n", ylim = c(0,45))
axis(1, at=1:17, labels = new_participants[1:17], las = 2, cex.axis = 1.2, cex.lab=2)
abline(v = 8.5, type = 'l' )
legend("topleft", legend = "DK1 ")
legend("topright", legend = "DK2")
title(main="Generators revenues when low wind penetration")
mtext(paste0("Eq. price (€) in DK1 - DK2 resp. : ", price_hour_new[431, 1], " / ", price_hour_new[431,2]), 1, line=7, col = "blue")

########################################################
########################################################
# Overall revenues
overall_revenues = matrix(0,nrow = 1, ncol = 17)
for (i in 1:17){
  overall_revenues[,i] = sum(revenues_hour[1:N_simulations,i])
}

par(mar=c(8, 4, 2, 2) + 0.1)
plot(1:17, overall_revenues[1,1:17]/1000, type = "h", lend="square", lwd = 10, ylab = "[k€]", xlab = "", xaxt ="n", ylim = c(0,35000))
abline(v = 8.5, type = 'l' )
legend("topleft", legend = "DK1 ")
legend("topright", legend = "DK2")
axis(1, at=1:17, labels = new_participants[1:17], las = 2, cex.axis = 1.2, lwd = 2)
title(main = "[January] Overall revenues for each market participant")

# DK1
color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
sample_color = sample(color,8)
par(mar = c(4.5, 4.5,2, 2))
slices = (colSums(new_dispatch_hour[,1:8])/sum(colSums(new_dispatch_hour[,1:8])))*100
lbls = paste0(round(slices, digits = 2), " %")
labls = new_participants[1:8]
pie2(slices, labels = "", col = sample_color, radius = 0.8)
title(main ="DK1 Generation")
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend(0.75, 1.42, paste0(new_participants[1:8], ": ", round(slices, digits = 1), " %"), cex = 1.75, fill = sample_color)

par(mar = c(4.5, 4.5,2, 2))
slices = overall_revenues[1,1:8]/1000
lbls = paste0(round(slices, digits = 2), " k€")
labls = new_participants[1:8]
pie2(slices, labels = "", col = sample_color, radius = 0.8)
title(main ="DK1 Revenue")
plot(1, type="n", axes=FALSE, xlab="", ylab="") 
legend(0.75, 1.42, paste0(new_participants[1:8], ": ", round(slices, digits = 1), " k€"), cex = 1.75, fill = sample_color)

## DK2
color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
sample_color = sample(color,9)
par(mar = c(4.5, 4.5,2, 2))
slices = (colSums(new_dispatch_hour[,9:17])/sum(colSums(new_dispatch_hour[,9:17])))*100
lbls = paste0(round(slices, digits = 2), " %")
labls = new_participants[9:17]
pie2(slices, labels = "", col = sample_color, radius = 0.8)
title(main ="DK2 Generation")

plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend(0.75, 1.42, paste0(new_participants[9:17], ": ", round(slices, digits = 1), " %"), cex = 1.75, fill = sample_color)

par(mar = c(4.5, 4.5,2, 2))
slices = overall_revenues[1,9:17]/1000
lbls = paste0(round(slices, digits = 2), " k€")
labls = new_participants[9:17]
pie2(slices, labels = "", col = sample_color, radius = 0.8)
title(main ="DK2 Revenues")
plot(1, type="n", axes=FALSE, xlab="", ylab="") 
legend(0.75, 1.42, paste0(new_participants[9:17], ": ", round(slices, digits = 1), " k€"), cex = 1.75, fill = sample_color)


