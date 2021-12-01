library(rvest)
library(plyr)
library(dplyr)
library(data.table)
library(dtplyr)
library(randomForest)
library(rpart)
library(mice)
library(lubridate)
library(parallel)

a15 <- load("mc.res_15sim-2020-09-14:00:00:00.Rdata") #15 list full
mc.res15 <- mc.res
a45 <- load("mc.res_60sim-2020-09-13:00:00:00.Rdata") # 30 list 6,16,26 error list
mc.res_temp <-mc.res
mc.res45 <- mc.res_temp[-c(6,16,26)]
a75 <- load("mc.res_120sim-2020-09-11:00:00:00.Rdata") # 30 list 5,15,25 are error list
mc.res_temp <-mc.res
mc.res75 <- mc.res_temp[-c(5,15,25)]
a90 <- load("mc.res_30sim-2020-09-11:00:00:00.Rdata") # 15 list full
mc.res90 <-mc.res
a120 <- load("mc.res_150sim-2020-09-10:00:00:00.Rdata") # 30 list 4,14,24 are error list
mc.res_temp <-mc.res
mc.res120 <- mc.res_temp[-c(4,14,24)]
a150 <- load("mc.res_180sim-2020-09-11:00:00:00.Rdata") # 30 list 6,16,26 error list
mc.res_temp <-mc.res
mc.res150 <- mc.res_temp[-c(6,16,26)]
a165 <- load("mc.res_45sim-2020-09-14:00:00:00.Rdata") # 15 list full
mc.res165 <-mc.res
a195 <- load("mc.res_60sim-2020-09-10:00:00:00.Rdata") # 30 list full
mc.res195 <-mc.res
a225 <- load("mc.res_210sim-2020-09-10:00:00:00.Rdata") # 30 list 2,9,12,19,22,29 sre error list
mc.res_temp <-mc.res
mc.res225 <- mc.res_temp[-c(2,9,12,19,22,29)]
a255 <- load("mc.res_240sim-2020-09-12:00:00:00.Rdata") # 30 list full
mc.res255 <-mc.res

dat <- c(mc.res15, mc.res45, mc.res75, mc.res90, mc.res120, mc.res150, mc.res165, mc.res195, mc.res225, mc.res255)

cor_1per <- vector(mode = "list", length = 60)
for (i in 1:60) {
  cor_1per[[i]] <- dat[[i]]$corsim_1
}
cor_5per <- vector(mode = "list", length = 60)
for (i in 1:60) {
  cor_5per[[i]] <- dat[[i]]$corsim_5
}
cor_10per <- vector(mode = "list", length = 60)
for (i in 1:60) {
  cor_10per[[i]] <- dat[[i]]$corsim_10
}
cor_20per <- vector(mode = "list", length = 60)
for (i in 1:60) {
  cor_20per[[i]] <- dat[[i]]$corsim_20
}

estim_full <- matrix(0,237,20)
estim_sur <- matrix(0,237,20)
estim_mi.split <- matrix(0,237,20)
estim_mi.NA <- matrix(0,237,20)
estim_mi.white <- matrix(0,237,20)
estim_mi <- matrix(0,237,20)

x <- 1
for (k in 1:237) {
  
  ##est[i,] <- dat[[60]][[j]][,"survey.x3"]
  estim_full[x,] <- dat[[k]][[1]][,1]
  estim_sur[x,] <- dat[[k]][[1]][,2]
  estim_mi.split[x,] <- dat[[k]][[1]][,3]
  estim_mi.NA[x,] <- dat[[k]][[1]][,4]
  estim_mi.white[x,] <- dat[[k]][[1]][,5]
  estim_mi[x,] <- dat[[k]][[1]][,6]
  x = x+1
  ##estim[k,] <- rbind(est)
}
se_full <- matrix(0,237,20)
se_sur <- matrix(0,237,20)
se_mi.split <- matrix(0,237,20)
se_mi.NA <- matrix(0,237,20)
se_mi.white <- matrix(0,237,20)
se_mi <- matrix(0,237,20)
x <- 1
for (k in 1:237) {
  
  ##est[i,] <- dat[[60]][[j]][,"survey.x3"]
  se_full[x,] <- dat[[k]][[2]][,1]
  se_sur[x,] <- dat[[k]][[2]][,2]
  se_mi.split[x,] <- dat[[k]][[2]][,3]
  se_mi.NA[x,] <- dat[[k]][[2]][,4]
  se_mi.white[x,] <- dat[[k]][[2]][,5]
  se_mi[x,] <- dat[[k]][[2]][,6]
  x = x+1
  ##estim[k,] <- rbind(est)
}

est.full <- cbind(estim_full ,se_full)
colnames(est.full) <- c("x3_1", "x3_5", "x3_10", "x3_20", "x4_1", "x4_5", "x4_10","x4_20", "x5.2_1","x5.2_5",
                          "x5.2_10","x5.2_20","x5.3_1","x5.3_5","x5.3_10","x5.3_20","x5.4_1","x5.4_5","x5.4_10",
                          "x5.4_20","x3.se_1", "x3.se_5", "x3.se_10", "x3.se_20", "x4.se_1", "x4.se_5", "x4.se_10",
                          "x4.se_20", "x5.2.se_1", "x5.2.se_5", "x5.2.se_10", "x5.2.se_20", "x5.3.se_1", "x5.3.se_5", 
                          "x5.3.se_10", "x5.3.se_20", "x5.4.se_1", "x5.4.se_5", "x5.4.se_10", "x5.4.se_20")

est.sur <- cbind(estim_sur,se_sur)
colnames(est.sur) <- c("x3_1", "x3_5", "x3_10", "x3_20", "x4_1", "x4_5", "x4_10","x4_20", "x5.2_1","x5.2_5",
                        "x5.2_10","x5.2_20","x5.3_1","x5.3_5","x5.3_10","x5.3_20","x5.4_1","x5.4_5","x5.4_10",
                        "x5.4_20","x3.se_1", "x3.se_5", "x3.se_10", "x3.se_20", "x4.se_1", "x4.se_5", "x4.se_10",
                        "x4.se_20", "x5.2.se_1", "x5.2.se_5", "x5.2.se_10", "x5.2.se_20", "x5.3.se_1", "x5.3.se_5", 
                        "x5.3.se_10", "x5.3.se_20", "x5.4.se_1", "x5.4.se_5", "x5.4.se_10", "x5.4.se_20")

est.mi.split <- cbind(estim_mi.split, se_mi.split)
colnames(est.mi.split) <- c("x3_1", "x3_5", "x3_10", "x3_20", "x4_1", "x4_5", "x4_10","x4_20", "x5.2_1","x5.2_5",
                        "x5.2_10","x5.2_20","x5.3_1","x5.3_5","x5.3_10","x5.3_20","x5.4_1","x5.4_5","x5.4_10",
                        "x5.4_20","x3.se_1", "x3.se_5", "x3.se_10", "x3.se_20", "x4.se_1", "x4.se_5", "x4.se_10",
                        "x4.se_20", "x5.2.se_1", "x5.2.se_5", "x5.2.se_10", "x5.2.se_20", "x5.3.se_1", "x5.3.se_5", 
                        "x5.3.se_10", "x5.3.se_20", "x5.4.se_1", "x5.4.se_5", "x5.4.se_10", "x5.4.se_20")

est.mi.NA <- cbind(estim_mi.NA, se_mi.NA)
colnames(est.mi.NA) <- c("x3_1", "x3_5", "x3_10", "x3_20", "x4_1", "x4_5", "x4_10","x4_20", "x5.2_1","x5.2_5",
                            "x5.2_10","x5.2_20","x5.3_1","x5.3_5","x5.3_10","x5.3_20","x5.4_1","x5.4_5","x5.4_10",
                            "x5.4_20","x3.se_1", "x3.se_5", "x3.se_10", "x3.se_20", "x4.se_1", "x4.se_5", "x4.se_10",
                            "x4.se_20", "x5.2.se_1", "x5.2.se_5", "x5.2.se_10", "x5.2.se_20", "x5.3.se_1", "x5.3.se_5", 
                            "x5.3.se_10", "x5.3.se_20", "x5.4.se_1", "x5.4.se_5", "x5.4.se_10", "x5.4.se_20")
est.mi.white <- cbind(estim_mi.white, se_mi.white)
colnames(est.mi.white) <- c("x3_1", "x3_5", "x3_10", "x3_20", "x4_1", "x4_5", "x4_10","x4_20", "x5.2_1","x5.2_5",
                            "x5.2_10","x5.2_20","x5.3_1","x5.3_5","x5.3_10","x5.3_20","x5.4_1","x5.4_5","x5.4_10",
                            "x5.4_20","x3.se_1", "x3.se_5", "x3.se_10", "x3.se_20", "x4.se_1", "x4.se_5", "x4.se_10",
                            "x4.se_20", "x5.2.se_1", "x5.2.se_5", "x5.2.se_10", "x5.2.se_20", "x5.3.se_1", "x5.3.se_5", 
                            "x5.3.se_10", "x5.3.se_20", "x5.4.se_1", "x5.4.se_5", "x5.4.se_10", "x5.4.se_20")
est.mi <- cbind(estim_mi, se_mi)
colnames(est.mi) <- c("x3_1", "x3_5", "x3_10", "x3_20", "x4_1", "x4_5", "x4_10","x4_20", "x5.2_1","x5.2_5",
                            "x5.2_10","x5.2_20","x5.3_1","x5.3_5","x5.3_10","x5.3_20","x5.4_1","x5.4_5","x5.4_10",
                            "x5.4_20","x3.se_1", "x3.se_5", "x3.se_10", "x3.se_20", "x4.se_1", "x4.se_5", "x4.se_10",
                            "x4.se_20", "x5.2.se_1", "x5.2.se_5", "x5.2.se_10", "x5.2.se_20", "x5.3.se_1", "x5.3.se_5", 
                            "x5.3.se_10", "x5.3.se_20", "x5.4.se_1", "x5.4.se_5", "x5.4.se_10", "x5.4.se_20")
est_list <- lapply(list(est.full = est.full, est.sur = est.sur,est.mi.split =est.mi.split, 
                   est.mi.NA= est.mi.NA, est.mi.white=est.mi.white, est.mi = est.mi), function(x){x})
est <- array(as.numeric(unlist(est_list)), dim=c(237, 40, 6))

x3 <- matrix(0,4,6)
x4 <- matrix(0,4,6)
x5.2 <- matrix(0,4,6)
x5.3 <- matrix(0,4,6)
x5.4 <- matrix(0,4,6)

for (l in 1:4) {
  ci.cover_x3 <- apply(est,c(1,3), function(x) abs(0.1 - x[l]) < 1.96 * x[20+l])
  x3[l,] <- apply(ci.cover_x3, 2, mean)
  ci.cover_x4 <- apply(est, c(1,3), function(x) abs(0.02 - x[l+4]) < 1.96 * x[24+l])
  x4[l,] <- apply(ci.cover_x4, 2, mean)
  ci.cover_x5.1 <- apply(est, c(1,3), function(x) abs(log(5) - x[l+8]) < 1.96 * x[28+l])
  x5.2[l,] <- apply(ci.cover_x5.1, 2, mean)
  ci.cover_x5.2 <- apply(est, c(1,3), function(x) abs(log(2) - x[l+12]) < 1.96 * x[32+l])
  x5.3[l,] <- apply(ci.cover_x5.2, 2, mean)
  ci.cover_x5.3 <- apply(est, c(1,3), function(x) abs(log(1.5) - x[l+16]) < 1.96 * x[36+l])
  x5.4[l,] <- apply(ci.cover_x5.3, 2, mean)
  
}

beta_x3 <- matrix(0,4,6)
beta_x4 <- matrix(0,4,6)
beta_x5.2 <- matrix(0,4,6)
beta_x5.3 <- matrix(0,4,6)
beta_x5.4 <- matrix(0,4,6)

for (l in 1:4) {
  beta_x3[l,] <- apply(est[,l,], 2, mean)
  beta_x4[l,] <- apply(est[,l+4,], 2, mean)
  beta_x5.2[l,] <- apply(est[,l+8,], 2, mean)
  beta_x5.3[l,] <- apply(est[,l+12,], 2, mean)
  beta_x5.4[l,] <- apply(est[,l+16,], 2, mean)
}

rmse_x3 <- matrix(0,4,6)
rmse_x4 <- matrix(0,4,6)
rmse_x5.2 <- matrix(0,4,6)
rmse_x5.3 <- matrix(0,4,6)
rmse_x5.4 <- matrix(0,4,6)

for (l in 1:4) {
  rmse_x3[l,] <- round(sqrt(apply(est[,l,], 2, function(x) mean((x - 0.1)^2))), 3)
  rmse_x4[l,] <- round(sqrt(apply(est[,l+4,], 2, function(x) mean((x - 0.02)^2))), 3)
  rmse_x5.2[l,] <- round(sqrt(apply(est[,l+8,], 2, function(x) mean((x - log(5))^2))), 3)
  rmse_x5.3[l,] <- round(sqrt(apply(est[,l+12,], 2, function(x) mean((x - log(2))^2))), 3)
  rmse_x5.4[l,] <- round(sqrt(apply(est[,l+16,], 2, function(x) mean((x - log(1.5))^2))), 3)
  
}

mae_x3 <- matrix(0,4,6)
mae_x4 <- matrix(0,4,6)
mae_x5.2 <- matrix(0,4,6)
mae_x5.3 <- matrix(0,4,6)
mae_x5.4 <- matrix(0,4,6)

for (l in 1:4) {
  mae_x3[l,] <- round(apply(est[,l,], 2, function(x) mean(abs(x - 0.1))), 3)
  mae_x4[l,] <- round(apply(est[,l+4,], 2, function(x) mean(abs(x - 0.02))), 3)
  mae_x5.2[l,] <- round(apply(est[,l+8,], 2, function(x) mean(abs(x - log(5)))), 3)
  mae_x5.3[l,] <- round(apply(est[,l+12,], 2, function(x) mean(abs(x - log(2)))), 3)
  mae_x5.4[l,] <- round(apply(est[,l+16,], 2, function(x) mean(abs(x - log(1.5)))), 3)
}

bias_x3 <- apply(beta_x3, 2, function(x) 0.1-x)  
bias_x4 <- apply(beta_x4, 2, function(x) 0.02-x)
bias_x5.2 <- apply(beta_x5.2, 2, function(x) log(5)-x)  
bias_x5.3 <- apply(beta_x5.3, 2, function(x) log(2)-x)  
bias_x5.4 <- apply(beta_x5.4, 2, function(x) log(1.5)-x)

d_split <- matrix(0,4,5)
d_NA <- matrix(0,4,5)
d_white <- matrix(0,4,5)
d_mi <- matrix(0,4,5)
rownames(d_split) <- c(10,20,30,40)
colnames(d_split) <- c("x3","x4","x5-2","x5-3","x5-4")
rownames(d_NA) <- c(10,20,30,40)
colnames(d_NA) <- c("x3","x4","x5-2","x5-3","x5-4")
rownames(d_white) <- c(10,20,30,40)
colnames(d_white) <- c("x3","x4","x5-2","x5-3","x5-4")
rownames(d_mi) <- c(10,20,30,40)
colnames(d_mi) <- c("x3","x4","x5-2","x5-3","x5-4")
for (i in 1:4) {
  d_split[i,1] <- (1/237)* sum(as.numeric(abs(estim_mi.split[,i]-0.1)< abs(estim_sur[,i] -0.1))) 
  d_split[i,2] <- (1/237)* sum(abs(estim_mi.split[,i+4]-0.02)< abs(estim_sur[,i+4] -0.02))
  d_split[i,3] <- (1/237)* sum(abs(estim_mi.split[,i+8]-log(5))< abs(estim_sur[,i+8] -log(5)))
  d_split[i,4] <- (1/237)* sum(abs(estim_mi.split[,i+12]-log(2))< abs(estim_sur[,i+12] -log(2)))
  d_split[i,5] <- (1/237)* sum(abs(estim_mi.split[,i+16]-log(1.5))< abs(estim_sur[,i+16] -log(1.5)))
  
  
  d_NA[i,1] <- (1/237)* sum(abs(estim_mi.NA[,i]-0.1)< abs(estim_sur[,i] -0.1)) 
  d_NA[i,2] <- (1/237)* sum(abs(estim_mi.NA[,i+4]-0.02)< abs(estim_sur[,i+4] -0.02))
  d_NA[i,3] <- (1/237)* sum(abs(estim_mi.NA[,i+8]-log(5))< abs(estim_sur[,i+8] -log(5)))
  d_NA[i,4] <- (1/237)* sum(abs(estim_mi.NA[,i+12]-log(2))< abs(estim_sur[,i+12] -log(2)))
  d_NA[i,5] <- (1/237)* sum(abs(estim_mi.NA[,i+16]-log(1.5))< abs(estim_sur[,i+16] -log(1.5)))
  
  d_white[i,1] <- (1/237)* sum(abs(estim_mi.white[,i]-0.1)< abs(estim_sur[,i] -0.1)) 
  d_white[i,2] <- (1/237)* sum(abs(estim_mi.white[,i+4]-0.02)< abs(estim_sur[,i+4] -0.02))
  d_white[i,3] <- (1/237)* sum(abs(estim_mi.white[,i+8]-log(5))< abs(estim_sur[,i+8] -log(5)))
  d_white[i,4] <- (1/237)* sum(abs(estim_mi.white[,i+12]-log(2))< abs(estim_sur[,i+12] -log(2)))
  d_white[i,5] <- (1/237)* sum(abs(estim_mi.white[,i+16]-log(1.5))< abs(estim_sur[,i+16] -log(1.5)))
  
  d_mi[i,1] <- (1/237)* sum(abs(estim_mi[,i]-0.1)< abs(estim_sur[,i] -0.1)) 
  d_mi[i,2] <- (1/237)* sum(abs(estim_mi[,i+4]-0.02)< abs(estim_sur[,i+4] -0.02))
  d_mi[i,3] <- (1/237)* sum(abs(estim_mi[,i+8]-log(5))< abs(estim_sur[,i+8] -log(5)))
  d_mi[i,4] <- (1/237)* sum(abs(estim_mi[,i+12]-log(2))< abs(estim_sur[,i+12] -log(2)))
  d_mi[i,5] <- (1/237)* sum(abs(estim_mi[,i+16]-log(1.5))< abs(estim_sur[,i+16] -log(1.5)))
}
split_d <- apply(d_split, c(1,2), function(x) round(x/(1-x),3))
NA_d <- apply(d_NA, c(1,2), function(x) round(x/(1-x),3))
white_d <- apply(d_white, c(1,2), function(x) round(x/(1-x),3))
mi_d <- apply(d_mi, c(1,2), function(x) round(x/(1-x),3))
######################################################################################################
#                                        PLOTS                                                       #
######################################################################################################
png("beta1.png", width=12, height=7, units = 'in', res=500)
par(mfrow = c(1,2))
t <- c(1,5,10,20)
plot(t,beta_x3[,2],main="Coefficient of x3",ylab="Error",xlab = "percentage",type="l", ylim = c(0,0.12))
lines(t,beta_x3[,3], col="darkgreen")
lines(t,beta_x3[,4], col="deeppink")
lines(t,beta_x3[,5], col="red")
lines(t,beta_x3[,6], col="blue")
lines(t,c(0.1,0.1,0.1,0.1), lty = 2 )
legend("bottomright", c("survey","imputed-split", "imputed-NA","imputed-white", "imputed-trans"), col=c("black", "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1,1), lwd=1, bty='n')

plot(t,beta_x4[,2],main="Coefficient of x4",ylab="Error",xlab = "percentage",type="l", ylim = c(0,.03))
lines(t,beta_x4[,3], col="darkgreen")
lines(t,beta_x4[,4], col="deeppink")
lines(t,beta_x4[,5], col="red")
lines(t,beta_x4[,6], col="blue")
lines(t,c(0.02,0.02,0.02,0.02), lty = 2 )
legend("topright", c("survey","imputed-split", "imputed-NA","imputed-white", "imputed-trans"), col=c("black", "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1,1), lwd=1, bty='n')
dev.off()

png("beta2.png", width=15, height=7, units = 'in', res=500)
par(mfrow = c(1,3))
t <- c(1,5,10,20)
plot(t,beta_x5.2[,2],main="Coefficient of x5- cat2",ylab="bias",xlab = "percentage",type="l", ylim = c(0.15,1.7), cex.lab = 1.5, cex = 1.5)
lines(t,beta_x5.2[,3], col="darkgreen")
lines(t,beta_x5.2[,4], col="deeppink")
lines(t,beta_x5.2[,5], col="red")
lines(t,beta_x5.2[,6], col="blue")
lines(t,c(log(5),log(5),log(5),log(5)), lty = 2 )
legend("bottomright", c("survey","imputed-split", "imputed-NA","imputed-white", "imputed-trans"), col=c("black", "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1,1), lwd=1, bty='n',cex = 1.5)

plot(t,beta_x5.3[,2],main="Coefficient of x5- cat3",ylab="bias",xlab = "percentage",type="l", ylim = c(0.15,1.7), cex.lab = 1.5, cex = 1.5)
lines(t,beta_x5.3[,3], col="darkgreen")
lines(t,beta_x5.3[,4], col="deeppink")
lines(t,beta_x5.3[,5], col="red")
lines(t,beta_x5.3[,6], col="blue")
lines(t,c(log(2),log(2),log(2),log(2)), lty = 2 )
legend("topright", c("survey","imputed-split", "imputed-NA","imputed-white", "imputed-trans"), col=c("black", "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1,1), lwd=1, bty='n',cex = 1.5)

plot(t,beta_x5.4[,2],main="Coefficient of x5- cat4",ylab="bias",xlab = "percentage",type="l", ylim =  c(0.15,1.7), cex.lab = 1.5, cex = 1.5)
lines(t,beta_x5.4[,3], col="darkgreen")
lines(t,beta_x5.4[,4], col="deeppink")
lines(t,beta_x5.4[,5], col="red")
lines(t,beta_x5.4[,6], col="blue")
lines(t,c(log(1.5),log(1.5),log(1.5),log(1.5)), lty = 2 )
legend("topright", c("survey","imputed-split", "imputed-NA","imputed-white", "imputed-trans"), col=c("black", "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1,1), lwd=1, bty='n',cex = 1.5)
dev.off()

png("bias1.png", width=12, height=7, units = 'in', res=500)
par(mfrow = c(1,2))
t <- c(1,5,10,20)
plot(t,bias_x3[,2],main="Bias in x3",ylab="bias",xlab = "percentage",type="l", ylim = c(-.0009,.08))
lines(t,bias_x3[,3], col="darkgreen")
lines(t,bias_x3[,4], col="deeppink")
lines(t,bias_x3[,5], col="red")
lines(t,bias_x3[,6], col="blue")
legend("topright", c("survey","imputed-split", "imputed-NA","imputed-white", "imputed-trans"), col=c("black", "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1,1), lwd=1, bty='n')

plot(t, bias_x3[,3],main="Bias in mputed x3",ylab="bias",xlab = "percentage",type="l",col="darkgreen", ylim = c(0,0.09))
lines(t,bias_x3[,4], col="deeppink")
lines(t,bias_x3[,5], col="red")
lines(t,bias_x3[,6], col="blue")
legend("topright", c("imputed-split", "imputed-NA","imputed-white", "imputed-trans"), col=c( "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1), lwd=1, bty='n')

dev.off()

png("bias2.png", width=12, height=7, units = 'in', res=500)
par(mfrow = c(1,2))
t <- c(1,5,10,20)
plot(t,bias_x4[,2],main="Bias in x4",ylab="bias",xlab = "percentage",type="l", ylim = c(-.007,.02))
lines(t,bias_x4[,3], col="darkgreen")
lines(t,bias_x4[,4], col="deeppink")
lines(t,bias_x4[,5], col="red")
lines(t,bias_x4[,6], col="blue")
lines(t,c(0,0,0,0), lty = 2 )

legend("topright", c("survey","imputed-split", "imputed-NA","imputed-white", "imputed-trans"), col=c("black", "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1,1), lwd=1, bty='n')
plot(t, bias_x4[,3],main="Bias in mputed x4",ylab="bias",xlab = "percentage",type="l",col="darkgreen", ylim =c(-.007,.02))
lines(t,bias_x4[,4], col="deeppink")
lines(t,bias_x4[,5], col="red")
lines(t,bias_x4[,6], col="blue")
lines(t,c(0,0,0,0), lty = 2 )
legend("topright", c("imputed-split", "imputed-NA","imputed-white", "imputed-trans"), col=c( "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1), lwd=1, bty='n')
dev.off()

png("bias3.png", width=15, height=7, units = 'in', res=500)
par(mfrow = c(1,3))
t <- c(1,5,10,20)
plot(t,bias_x5.2[,2],main="Bias in x5- class2",ylab="bias",xlab = "percentage",type="l", ylim = c(-0.02,1.5), cex.lab = 1.5)
lines(t,bias_x5.2[,3], col="darkgreen")
lines(t,bias_x5.2[,4], col="deeppink")
lines(t,bias_x5.2[,5], col="red")
lines(t,bias_x5.2[,6], col="blue")
legend("topright", c("survey","imputed-split", "imputed-NA","imputed-white", "imputed-trans"), col=c("black", "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1,1), lwd=1, bty='n',cex = 1.5)

plot(t,bias_x5.3[,2],main="Bias in x5- class3",ylab="bias",xlab = "percentage",type="l", ylim =c(-0.02,1.5), cex.lab = 1.5)
lines(t,bias_x5.3[,3], col="darkgreen")
lines(t,bias_x5.3[,4], col="deeppink")
lines(t,bias_x5.3[,5], col="red")
lines(t,bias_x5.3[,6], col="blue")
legend("topright", c("survey","imputed-split", "imputed-NA","imputed-white", "imputed-trans"), col=c("black", "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1,1), lwd=1, bty='n',cex = 1.5)

plot(t,bias_x5.4[,2],main="Bias in x5- class4",ylab="bias",xlab = "percentage",type="l", ylim = c(-0.02,1.5), cex.lab = 1.5)
lines(t,bias_x5.4[,3], col="darkgreen")
lines(t,bias_x5.4[,4], col="deeppink")
lines(t,bias_x5.4[,5], col="red")
lines(t,bias_x5.4[,6], col="blue")
legend("topright", c("survey","imputed-split", "imputed-NA","imputed-white", "imputed-trans"), col=c("black", "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1,1), lwd=1, bty='n',cex = 1.5)
dev.off()

png("bias3.1.png", width=15, height=7, units = 'in', res=500)
par(mfrow = c(1,3))
t <- c(1,5,10,20)
plot(t, bias_x5.2[,3],main="Bias in mputed x5- class2",ylab="bias",xlab = "percentage",type="l",col="darkgreen", ylim = c(-0.02,1.5), cex.lab = 1.5)
lines(t,bias_x5.2[,4], col="deeppink")
lines(t,bias_x5.2[,5], col="red")
lines(t,bias_x5.2[,6], col="blue")
legend("topright", c("imputed-split", "imputed-NA","imputed-white", "imputed-trans"), col=c( "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1), lwd=1, bty='n',cex = 1.5)

plot(t, bias_x5.3[,3],main="Bias in mputed x5- class3",ylab="bias",xlab = "percentage",type="l",col="darkgreen", ylim = c(-0.02,1.5), cex.lab = 1.5)
lines(t,bias_x5.3[,4], col="deeppink")
lines(t,bias_x5.3[,5], col="red")
lines(t,bias_x5.3[,6], col="blue")
legend("topright", c("imputed-split", "imputed-NA","imputed-white", "imputed-trans"), col=c( "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1), lwd=1, bty='n',cex = 1.5)

plot(t, bias_x5.4[,3],main="Bias in mputed x5- class4",ylab="bias",xlab = "percentage",type="l",col="darkgreen", ylim = c(-0.02,1.5), cex.lab = 1.5)
lines(t,bias_x5.4[,4], col="deeppink")
lines(t,bias_x5.4[,5], col="red")
lines(t,bias_x5.4[,6], col="blue")
legend("topright", c("imputed-split", "imputed-NA","imputed-white", "imputed-trans"), col=c( "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1), lwd=1, bty='n',cex = 1.5)
dev.off()

png("rmse.png", width=12, height=7, units = 'in', res=500)
par(mfrow = c(1,2))
t <- c(1,5,10,20)
plot(t,rmse_x3[,2],main="RMSE in x3",ylab="Error",xlab = "percentage",type="l", ylim = c(0,.08))
lines(t,rmse_x3[,3], col="darkgreen")
lines(t,rmse_x3[,4], col="deeppink")
lines(t,rmse_x3[,5], col="red")
lines(t,rmse_x3[,6], col="blue")
legend("topright", c("CCA","MI-split", "MI-NA","MI-white", "MI-transformation"), col=c("black", "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1,1), lwd=1, bty='n')

plot(t,rmse_x4[,2],main="RMSE in x4",ylab="Error",xlab = "percentage",type="l", ylim = c(0,.03))
lines(t,rmse_x4[,3], col="darkgreen")
lines(t,rmse_x4[,4], col="deeppink")
lines(t,rmse_x4[,5], col="red")
lines(t,rmse_x4[,6], col="blue")
legend("topright", c("CCA","MI-split", "MI-NA","MI-white", "MI-transformation"), col=c("black", "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1,1), lwd=1, bty='n')
dev.off()

png("rmse1.png", width=15, height=7, units = 'in', res=500)

t <- c(1,5,10,20)
par(mfrow = c(1,3))
plot(t,rmse_x5.2[,2],main="RMSE in x5-Class2",ylab="Error",xlab = "percentage",type="l", ylim = c(0,1.3), cex.lab = 1.5)
lines(t,rmse_x5.2[,3], col="darkgreen")
lines(t,rmse_x5.2[,4], col="deeppink")
lines(t,rmse_x5.2[,5], col="red")
lines(t,rmse_x5.2[,6], col="blue")
legend("topright", c("CCA","MI-split", "MI-NA","MI-white", "MI-transformation"), col=c("black", "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1,1), lwd=1, bty='n')

plot(t,rmse_x5.3[,2],main="RMSE in x5-Class3",ylab="Error",xlab = "percentage",type="l", ylim = c(0,1.3), cex.lab = 1.5)
lines(t,rmse_x5.3[,3], col="darkgreen")
lines(t,rmse_x5.3[,4], col="deeppink")
lines(t,rmse_x5.3[,5], col="red")
lines(t,rmse_x5.3[,6], col="blue")
legend("topright", c("CCA","MI-split", "MI-NA","MI-white", "MI-transformation"), col=c("black", "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1,1), lwd=1, bty='n')

plot(t,rmse_x5.4[,2],main="RMSE in x5-Class4",ylab="Error",xlab = "percentage",type="l", ylim = c(0,1.3), cex.lab = 1.5)
lines(t,rmse_x5.4[,3], col="darkgreen")
lines(t,rmse_x5.4[,4], col="deeppink")
lines(t,rmse_x5.4[,5], col="red")
lines(t,rmse_x5.4[,6], col="blue")
legend("topright", c("CCA","MI-split", "MI-NA","MI-white", "MI-transformation"), col=c("black", "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1,1), lwd=1, bty='n')
dev.off()

##MAE plot

png("mae.png", width=12, height=7, units = 'in', res=500)
par(mfrow = c(1,2))
t <- c(1,5,10,20)
plot(t,mae_x3[,2],main="MAE in x3",ylab="Error",xlab = "percentage",type="l", ylim = c(0,.08))
lines(t,mae_x3[,3], col="darkgreen")
lines(t,mae_x3[,4], col="deeppink")
lines(t,mae_x3[,5], col="red")
lines(t,mae_x3[,6], col="blue")
legend("topright", c("CCA","MI-split", "MI-NA","MI-white", "MI-transformation"), col=c("black", "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1,1), lwd=1, bty='n')

plot(t,mae_x4[,2],main="MAE in x4",ylab="Error",xlab = "percentage",type="l", ylim = c(0,.02))
lines(t,mae_x4[,3], col="darkgreen")
lines(t,mae_x4[,4], col="deeppink")
lines(t,mae_x4[,5], col="red")
lines(t,mae_x4[,6], col="blue")
legend("topright", c("CCA","MI-split", "MI-NA","MI-white", "MI-transformation"), col=c("black", "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1,1), lwd=1, bty='n')
dev.off()

png("mae1.png", width=15, height=7, units = 'in', res=500)
t <- c(1,5,10,20)
par(mfrow = c(1,3))
plot(t,mae_x5.2[,2],main="MAE in x5-Class2",ylab="Error",xlab = "percentage",type="l", ylim = c(0,1.3), cex.lab = 1.5)
lines(t,mae_x5.2[,3], col="darkgreen")
lines(t,mae_x5.2[,4], col="deeppink")
lines(t,mae_x5.2[,5], col="red")
lines(t,mae_x5.2[,6], col="blue")
legend("topright", c("CCA","MI-split", "MI-NA","MI-white", "MI-transformation"), col=c("black", "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1,1), lwd=1, bty='n')

plot(t,mae_x5.3[,2],main="MAE in x5-Class3",ylab="Error",xlab = "percentage",type="l", ylim = c(0,1.3), cex.lab = 1.5)
lines(t,mae_x5.3[,3], col="darkgreen")
lines(t,mae_x5.3[,4], col="deeppink")
lines(t,mae_x5.3[,5], col="red")
lines(t,mae_x5.3[,6], col="blue")
legend("topright", c("CCA","MI-split", "MI-NA","MI-white", "MI-transformation"), col=c("black", "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1,1), lwd=1, bty='n')

plot(t,mae_x5.4[,2],main="MAE in x5-Class4",ylab="Error",xlab = "percentage",type="l", ylim =c(0,1.3), cex.lab = 1.5)
lines(t,mae_x5.4[,3], col="darkgreen")
lines(t,mae_x5.4[,4], col="deeppink")
lines(t,mae_x5.4[,5], col="red")
lines(t,mae_x5.4[,6], col="blue")
legend("topright", c("CCA","MI-split", "MI-NA","MI-white", "MI-transformation"), col=c("black", "darkgreen","deeppink","red","blue"), lty=c(1,1,1,1,1), lwd=1, bty='n')
dev.off()

############################################################################################################
#                                      Extra plots                                                         #    
############################################################################################################

