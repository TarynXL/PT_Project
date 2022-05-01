library(readxl)
library(ggplot2)
library(reshape2)
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(dplyr))
library(magrittr)
library(circlize)
suppressPackageStartupMessages(library(gridExtra))

# Read in data
# discrete <- read_excel("Data/discrete.xls")
IDinfo <- read_excel("Data/IDinfo.xls")


# Investigate data -- should only be 4 trials per knee
IDinfo$ID_Check <- paste0(IDinfo$ID,IDinfo$KNEE)
count_of_trials <- data.frame(table(IDinfo$ID_Check))
names(count_of_trials) <- c("ID","trial_count")
miscount <- subset(count_of_trials,count_of_trials$trial_count != 4)
table(miscount$trial_count)

summary(IDinfo)

###### EDA COPx_stance, and COPy_stance #####

COPx_stance <- read.csv('Data/COPx_stance.csv', header  =FALSE)
COPy_stance <- read.csv('Data/COPy_stance.csv', header  =FALSE)

COPx_stance_1 <- cbind(IDinfo[c('ID','KNEE','TRIAL')],COPx_stance )
COPx_stance_1$ID2 <- paste0(COPx_stance_1$ID,COPx_stance_1$KNEE, COPx_stance_1$TRIAL)
COPx_stance_1<- select(COPx_stance_1,-c(ID,KNEE, TRIAL))
COPx_stance_2 <- data.frame(gather(COPx_stance_1, key = "COPx_stance", value = "Value", V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, 
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
COPx_stance_2$time <- as.numeric(substr(COPx_stance_2$COPx_stance,2,length(COPx_stance)))
COPx_stance_3 <-arrange(COPx_stance_2,ID2,time)

mycolors <- rand_color(15696)
COPx_stance_plot <- ggplot(COPx_stance_3) + geom_line(aes(x = time, y = Value, group = ID2, color = ID2)) +
  scale_color_manual(values = mycolors) +
  theme(legend.position="none") + ggtitle("COPx_stance")


COPy_stance_1 <- cbind(IDinfo[c('ID','KNEE','TRIAL')],COPy_stance )
COPy_stance_1$ID2 <- paste0(COPy_stance_1$ID,COPy_stance_1$KNEE, COPy_stance_1$TRIAL)
COPy_stance_1<- select(COPy_stance_1,-c(ID,KNEE, TRIAL))
COPy_stance_2 <- data.frame(gather(COPy_stance_1, key = "COPy_stance", value = "Value", V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, 
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
COPy_stance_2$time <- as.numeric(substr(COPy_stance_2$COPy_stance,2,length(COPy_stance)))
COPy_stance_3 <-arrange(COPy_stance_2,ID2,time)

COPy_stance_plot <- ggplot(COPy_stance_3) + geom_line(aes(x = time, y = Value, group = ID2, color = ID2)) +
  scale_color_manual(values = mycolors) +
  theme(legend.position="none") + ggtitle("COPy_stance")

##########PCA_COPy########

#Reference 
#https://rviews.rstudio.com/2021/05/14/basic-fda-descriptive-statistics-with-r/
#https://rviews.rstudio.com/2021/06/10/functional-pca-with-r/

library(fda)
library(tidyverse)
suppressPackageStartupMessages(library(plotly))


knots    = c(seq(0,100,5)) #Location of knots
n_knots   = length(knots) #Number of knots
n_order   = 4 # order of basis functions: for cubic b-splines: order = 3 + 1
n_basis   = length(knots) + n_order - 2;
basis = create.bspline.basis(rangeval = c(0,100), n_basis)
plot(basis)


argvals <- matrix(COPy_stance_3$time, nrow = 100, ncol = 15696)
y_mat <- matrix(COPy_stance_3$Value, nrow = 100, ncol = 15696)

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
#summary(fun_pca)
plot(fun_pca$harmonics, lwd = 3)
title(main = "COPy_PCA",
      cex.main = 1.5,   font.main= 3, col.main= "darkblue",
      col.lab ="darkblue"
)

fun_pca$values
fun_pca$varprop
##############PCA_COPx######################


argvals_COPx <- matrix(COPx_stance_3$time, nrow = 100, ncol = 15696)
y_mat_COPx <- matrix(COPx_stance_3$Value, nrow = 100, ncol = 15696)

W.obj_COPx <- Data2fd(argvals = argvals_COPx, y = y_mat_COPx, basisobj = basis, lambda = 0.5)

W_mean_COPx <- mean.fd(W.obj_COPx)
W_sd_COPx <- std.fd(W.obj_COPx)
# Create objects for the standard upper and lower standard deviation
SE_u <- fd(basisobj = basis)
SE_l <- fd(basisobj = basis)
# Fill in the sd values
SE_u$coefs <- W_mean$coefs +  1.96 * W_sd$coefs/sqrt(15696) 
SE_l$coefs <- W_mean$coefs -  1.96 * W_sd$coefs/sqrt(15696)

plot(W.obj_COPx, xlab="Time", ylab="", lty = 1)
title(main = "Smoothed Curves")
lines(SE_u, lwd = 3, lty = 3)
lines(SE_l, lwd = 3, lty = 3)
lines(W_mean,  lwd = 3)


days <- seq(0,100, by=2)
cov_W_COPx <- var.fd(W.obj_COPx)
var_mat_COPx <-  eval.bifd(days,days,cov_W_COPx)
fig_COPx <- plot_ly(x = days, y = days, z = ~var_mat_COPx) %>% 
  add_surface(contours = list(
    z = list(show=TRUE,usecolormap=TRUE, highlightcolor="#ff0000", project=list(z=TRUE))))

fig_COPx <- fig_COPx %>% 
  layout(scene = list(camera=list(eye = list(x=1.87, y=0.88, z=-0.64))))

fig_COPx

fun_pca_COPx <- pca.fd(W.obj_COPx, nharm = 5)
#summary(fun_pca)
plot(fun_pca_COPx$harmonics, lwd = 3)
title(main = "COPx_PCA",
      cex.main = 1.5,   font.main= 3, col.main= "darkblue",
      col.lab ="darkblue"
)
#title(main = "COPx PCA", sub = "Sub-title",
#      xlab = "X axis", ylab = "Y axis",
 #     cex.main = 2,   font.main= 4, col.main= "red",
 #     cex.sub = 0.75, font.sub = 3, col.sub = "green",
#      col.lab ="darkblue")

#It is also to obtain the eigenvalues λj

fun_pca_COPx$values

#the proportion of the variance explained by each eigenvalue.
fun_pca_COPx$varprop

#############################EDA ML, AP, V GRF#########
ML_GRF_stance_N <- read.csv('Data/ML_GRF_stance_N.csv', header  =FALSE)
AP_GRF_stance_N <- read.csv('Data/AP_GRF_stance_N.csv', header  =FALSE)
V_GRF_stance_N <- read.csv('Data/V_GRF_stance_N.csv', header  =FALSE)

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


AP_GRF_stance_N_1 <- cbind(IDinfo[c('ID','KNEE','TRIAL')],AP_GRF_stance_N )
AP_GRF_stance_N_1$ID2 <- paste0(AP_GRF_stance_N_1$ID,AP_GRF_stance_N_1$KNEE, AP_GRF_stance_N_1$TRIAL)
AP_GRF_stance_N_1<- select(AP_GRF_stance_N_1,-c(ID,KNEE, TRIAL))
AP_GRF_stance_N_2 <- data.frame(gather(AP_GRF_stance_N_1, key = "AP_GRF_stance_N", value = "Value", V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, 
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
AP_GRF_stance_N_2$time <- as.numeric(substr(AP_GRF_stance_N_2$AP_GRF_stance_N,2,length(AP_GRF_stance_N)))
AP_GRF_stance_N_3 <-arrange(AP_GRF_stance_N_2,ID2,time)

AP_GRF_stance_N_plot <- ggplot(AP_GRF_stance_N_3) + geom_line(aes(x = time, y = Value, group = ID2, color = ID2)) +
  scale_color_manual(values = mycolors) +
  theme(legend.position="none") + ggtitle("AP_GRF_stance_N")

V_GRF_stance_N_1 <- cbind(IDinfo[c('ID','KNEE','TRIAL')],V_GRF_stance_N )
V_GRF_stance_N_1$ID2 <- paste0(V_GRF_stance_N_1$ID,V_GRF_stance_N_1$KNEE, V_GRF_stance_N_1$TRIAL)
V_GRF_stance_N_1<- select(V_GRF_stance_N_1,-c(ID,KNEE, TRIAL))
V_GRF_stance_N_2 <- data.frame(gather(V_GRF_stance_N_1, key = "V_GRF_stance_N", value = "Value", V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, 
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
V_GRF_stance_N_2$time <- as.numeric(substr(V_GRF_stance_N_2$V_GRF_stance_N,2,length(V_GRF_stance_N)))
V_GRF_stance_N_3 <-arrange(V_GRF_stance_N_2,ID2,time)

V_GRF_stance_N_plot <- ggplot(V_GRF_stance_N_3) + geom_line(aes(x = time, y = Value, group = ID2, color = ID2)) +
  scale_color_manual(values = mycolors) +
  theme(legend.position="none") + ggtitle("V_GRF_stance_N")


grid.arrange(ML_GRF_stance_N_plot, AP_GRF_stance_N_plot,V_GRF_stance_N_plot, ncol = 1 )


#######ML_GRF PCA#######
argvals_ML_GRF <- matrix(ML_GRF_stance_N_3$time, nrow = 100, ncol = 15696)
y_mat_ML_GRF <- matrix(ML_GRF_stance_N_3$Value, nrow = 100, ncol = 15696)

W.obj_ML_GRF <- Data2fd(argvals = argvals_ML_GRF, y = y_mat_ML_GRF, basisobj = basis, lambda = 0.5)

W_mean_ML_GRF <- mean.fd(W.obj_ML_GRF)
W_sd_ML_GRF <- std.fd(W.obj_ML_GRF)
# Create objects for the standard upper and lower standard deviation
SE_u <- fd(basisobj = basis)
SE_l <- fd(basisobj = basis)
# Fill in the sd values
SE_u$coefs <- W_mean$coefs +  1.96 * W_sd$coefs/sqrt(15696) 
SE_l$coefs <- W_mean$coefs -  1.96 * W_sd$coefs/sqrt(15696)

plot(W.obj_ML_GRF, xlab="Time", ylab="", lty = 1)
title(main = "Smoothed Curves")
lines(SE_u, lwd = 3, lty = 3)
lines(SE_l, lwd = 3, lty = 3)
lines(W_mean,  lwd = 3)


days <- seq(0,100, by=2)
cov_W_ML_GRF <- var.fd(W.obj_ML_GRF)
var_mat_ML_GRF <-  eval.bifd(days,days,cov_W_ML_GRF)
fig_ML_GRF <- plot_ly(x = days, y = days, z = ~var_mat_ML_GRF) %>% 
  add_surface(contours = list(
    z = list(show=TRUE,usecolormap=TRUE, highlightcolor="#ff0000", project=list(z=TRUE))))

fig_ML_GRF <- fig_ML_GRF %>% 
  layout(scene = list(camera=list(eye = list(x=1.87, y=0.88, z=-0.64))))

fig_ML_GRF

fun_pca_ML_GRF <- pca.fd(W.obj_ML_GRF, nharm = 5)
#summary(fun_pca)
plot(fun_pca_ML_GRF$harmonics, lwd = 3)
title(main = "ML_GRF_PCA",
      cex.main = 1.5,   font.main= 3, col.main= "darkblue",
      col.lab ="darkblue"
)
#title(main = "ML_GRF PCA", sub = "Sub-title",
#      xlab = "X axis", ylab = "Y axis",
#     cex.main = 2,   font.main= 4, col.main= "red",
#     cex.sub = 0.75, font.sub = 3, col.sub = "green",
#      col.lab ="darkblue")

#It is also to obtain the eigenvalues λj

fun_pca_ML_GRF$values

#the proportion of the variance explained by each eigenvalue.
fun_pca_ML_GRF$varprop


######AP_GRF PCA#########
argvals_AP_GRF <- matrix(AP_GRF_stance_N_3$time, nrow = 100, ncol = 15696)
y_mat_AP_GRF <- matrix(AP_GRF_stance_N_3$Value, nrow = 100, ncol = 15696)

W.obj_AP_GRF <- Data2fd(argvals = argvals_AP_GRF, y = y_mat_AP_GRF, basisobj = basis, lambda = 0.5)

W_mean_AP_GRF <- mean.fd(W.obj_AP_GRF)
W_sd_AP_GRF <- std.fd(W.obj_AP_GRF)
# Create objects for the standard upper and lower standard deviation
SE_u <- fd(basisobj = basis)
SE_l <- fd(basisobj = basis)
# Fill in the sd values
SE_u$coefs <- W_mean$coefs +  1.96 * W_sd$coefs/sqrt(15696) 
SE_l$coefs <- W_mean$coefs -  1.96 * W_sd$coefs/sqrt(15696)

plot(W.obj_AP_GRF, xlab="Time", ylab="", lty = 1)
title(main = "Smoothed Curves")
lines(SE_u, lwd = 3, lty = 3)
lines(SE_l, lwd = 3, lty = 3)
lines(W_mean,  lwd = 3)


days <- seq(0,100, by=2)
cov_W_AP_GRF <- var.fd(W.obj_AP_GRF)
var_mat_AP_GRF <-  eval.bifd(days,days,cov_W_AP_GRF)
fig_AP_GRF <- plot_ly(x = days, y = days, z = ~var_mat_AP_GRF) %>% 
  add_surface(contours = list(
    z = list(show=TRUE,usecolormap=TRUE, highlightcolor="#ff0000", project=list(z=TRUE))))

fig_AP_GRF <- fig_AP_GRF %>% 
  layout(scene = list(camera=list(eye = list(x=1.87, y=0.88, z=-0.64))))

fig_AP_GRF

fun_pca_AP_GRF <- pca.fd(W.obj_AP_GRF, nharm = 5)
#summary(fun_pca)
plot(fun_pca_AP_GRF$harmonics, lwd = 3)
title(main = "AP_GRF_PCA",
      cex.main = 1.5,   font.main= 3, col.main= "darkblue",
      col.lab ="darkblue"
)
#title(main = "AP_GRF PCA", sub = "Sub-title",
#      xlab = "X axis", ylab = "Y axis",
#     cex.main = 2,   font.main= 4, col.main= "red",
#     cex.sub = 0.75, font.sub = 3, col.sub = "green",
#      col.lab ="darkblue")

#It is also to obtain the eigenvalues λj

fun_pca_AP_GRF$values

#the proportion of the variance explained by each eigenvalue.
fun_pca_AP_GRF$varprop

#######V_GRF PCA#########
argvals_V_GRF <- matrix(V_GRF_stance_N_3$time, nrow = 100, ncol = 15696)
y_mat_V_GRF <- matrix(V_GRF_stance_N_3$Value, nrow = 100, ncol = 15696)

W.obj_V_GRF <- Data2fd(argvals = argvals_V_GRF, y = y_mat_V_GRF, basisobj = basis, lambda = 0.5)

W_mean_V_GRF <- mean.fd(W.obj_V_GRF)
W_sd_V_GRF <- std.fd(W.obj_V_GRF)
# Create objects for the standard upper and lower standard deviation
SE_u <- fd(basisobj = basis)
SE_l <- fd(basisobj = basis)
# Fill in the sd values
SE_u$coefs <- W_mean$coefs +  1.96 * W_sd$coefs/sqrt(15696) 
SE_l$coefs <- W_mean$coefs -  1.96 * W_sd$coefs/sqrt(15696)

plot(W.obj_V_GRF, xlab="Time", ylab="", lty = 1)
title(main = "Smoothed Curves")
lines(SE_u, lwd = 3, lty = 3)
lines(SE_l, lwd = 3, lty = 3)
lines(W_mean,  lwd = 3)


days <- seq(0,100, by=2)
cov_W_V_GRF <- var.fd(W.obj_V_GRF)
var_mat_V_GRF <-  eval.bifd(days,days,cov_W_V_GRF)
fig_V_GRF <- plot_ly(x = days, y = days, z = ~var_mat_V_GRF) %>% 
  add_surface(contours = list(
    z = list(show=TRUE,usecolormap=TRUE, highlightcolor="#ff0000", project=list(z=TRUE))))

fig_V_GRF <- fig_V_GRF %>% 
  layout(scene = list(camera=list(eye = list(x=1.87, y=0.88, z=-0.64))))

fig_V_GRF

fun_pca_V_GRF <- pca.fd(W.obj_V_GRF, nharm = 5)
#summary(fun_pca)
plot(fun_pca_V_GRF$harmonics, lwd = 3)
title(main = "V_GRF_PCA",
      cex.main = 1.5,   font.main= 3, col.main= "darkblue",
      col.lab ="darkblue"
)
#title(main = "V_GRF PCA", sub = "Sub-title",
#      xlab = "X axis", ylab = "Y axis",
#     cex.main = 2,   font.main= 4, col.main= "red",
#     cex.sub = 0.75, font.sub = 3, col.sub = "green",
#      col.lab ="darkblue")

#It is also to obtain the eigenvalues λj

fun_pca_V_GRF$values

#the proportion of the variance explained by each eigenvalue.
fun_pca_V_GRF$varprop