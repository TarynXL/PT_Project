library("readxl")
library(ggplot2)
library(reshape2)
library(tidyr)
library(dplyr)
library(magrittr)
library(circlize)

#Read in data
IDinfo <- read_excel("Data/IDinfo.xls")


#Attempt to recreate the graphs shown for the normalized ML AP and V
ML_GRF_stance_N <- read.csv('Data/ML_GRF_stance_N.csv', header  =FALSE)

ML_GRF_stance_N_1 <- cbind(IDinfo[c('ID','KNEE','TRIAL')],ML_GRF_stance_N )
ML_GRF_stance_N_1$ID2 <- paste0(ML_GRF_stance_N_1$ID,ML_GRF_stance_N_1$KNEE, ML_GRF_stance_N_1$TRIAL)
ML_GRF_stance_N_1<- select(ML_GRF_stance_N_1,-c(ID,KNEE, TRIAL))
ML_GRF_stance_N_2 <- data.frame(gather(ML_GRF_stance_N_1, key = "ML_GRF_stance_N", value = "Value", V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, 
                                       V11, V12, V13, V14, V15, V16, V17, V18, 
                                       V19, V20, V21, V22, V23, V24, V25, V26, V27,
                                       V28, V29, V30, V31, V32, V33, V34, V35, V36,
                                       V37, V38, V39, V40, V41, V42, V43, V44, V45,
                                       V46, V47, V48, V49, V50, V51, V52, V53, V54,
                                       V55,V56, V57, V58, V59, V60, V61, V62, V63,
                                       V64, V65, V66, V67, V68, V69, V70, V71, V72, 
                                       V73, V74, V75, V76, V77, V78, V79, V80, V81,
                                       V82,  V83, V84, V85, V86, V87, V88, V89, V90,
                                       V91, V92, V93, V94, V95, V96, V97, V98, V99,V100))
ML_GRF_stance_N_2$time <- as.numeric(substr(ML_GRF_stance_N_2$ML_GRF_stance_N,2,length(ML_GRF_stance_N)))
ML_GRF_stance_N_3 <-arrange(ML_GRF_stance_N_2,ID2,time)

mycolors <- rand_color(15696)
ML_GRF_stance_N_plot <- ggplot(ML_GRF_stance_N_3) + geom_line(aes(x = time, y = Value, group = ID2, color = ID2)) +
  scale_color_manual(values = mycolors) +
  theme(legend.position="none") + ggtitle("ML_GRF_stance_N")


###########################FUNCTIONAL PRINCIPAL COMPONENT ANALYSIS###########################
library(fda)
library(tidyverse)
library(plotly)


knots    = c(seq(0,100,5)) #Location of knots
n_knots   = length(knots) #Number of knots
n_order   = 4 # order of basis functions: for cubic b-splines: order = 3 + 1
n_basis   = length(knots) + n_order - 2;
basis = create.bspline.basis(rangeval = c(0,100), n_basis)
plot(basis)


argvals <- matrix(ML_GRF_stance_N_3$time, nrow = 100, ncol = 15696)
y_mat <- matrix(ML_GRF_stance_N_3$Value, nrow = 100, ncol = 15696)

W.obj <- Data2fd(argvals = argvals, y = y_mat, basisobj = basis, lambda = 0.5)

W_mean <- mean.fd(W.obj)
W_sd <- std.fd(W.obj)
# Create objects for the standard upper and lower standard deviation
SE_u <- fd(basisobj = basis)
SE_l <- fd(basisobj = basis)
# Fill in the sd values
SE_u$coefs <- W_mean$coefs +  1.96 * W_sd$coefs/sqrt(15696) 
SE_l$coefs <- W_mean$coefs -  1.96 * W_sd$coefs/sqrt(15696)

plot(W.obj, xlab="Time", ylab="", lty = 1)
title(main = "Smoothed Curves")
lines(SE_u, lwd = 3, lty = 3)
lines(SE_l, lwd = 3, lty = 3)
lines(W_mean,  lwd = 3)


days <- seq(0,100, by=2)
cov_W <- var.fd(W.obj)
var_mat <-  eval.bifd(days,days,cov_W)
fig <- plot_ly(x = days, y = days, z = ~var_mat) %>% 
  add_surface(contours = list(
    z = list(show=TRUE,usecolormap=TRUE, highlightcolor="#ff0000", project=list(z=TRUE))))

fig <- fig %>% 
  layout(scene = list(camera=list(eye = list(x=1.87, y=0.88, z=-0.64))))

fig

fun_pca <- pca.fd(W.obj, nharm = 5)
plot(fun_pca$harmonics, lwd = 3)
