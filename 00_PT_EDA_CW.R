library("readxl")
library(ggplot2)
library(reshape2)
library(tidyr)
library(dplyr)
library(magrittr)
library(circlize)

#Read in data
#discrete <- read_excel("Data/discrete.xls")
IDinfo <- read_excel("Data/IDinfo.xls")


#Investigate data -- should only be 4 trials per knee
IDinfo$ID_Check <- paste0(IDinfo$ID,IDinfo$KNEE)
count_of_trials <- data.frame(table(IDinfo$ID_Check))
names(count_of_trials) <- c("ID","trial_count")

miscount <- subset(count_of_trials,count_of_trials$trial_count != 4)
table(miscount$trial_count)

summary(IDinfo)

#Attempt to recreate the graphs shown for the normalized ML AP and V
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

require(gridExtra)
grid.arrange(ML_GRF_stance_N_plot, AP_GRF_stance_N_plot,V_GRF_stance_N_plot, ncol = 1 )

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


##Investigate between trial variability
ML_GRF_stance_N_var <- cbind(IDinfo[c('ID','KNEE','TRIAL')],ML_GRF_stance_N )
ML_GRF_stance_N_var$ID2 <- paste0(ML_GRF_stance_N_var$ID,ML_GRF_stance_N_var$KNEE)
ML_GRF_stance_N_var1 <- ML_GRF_stance_N_var %>% group_by(ID2) %>% summarise(ML_GRF_stance_N_var1   = var(V1  ),
    ML_GRF_stance_N_var2   = var(V2  ),
    ML_GRF_stance_N_var3   = var(V3  ),
    ML_GRF_stance_N_var4   = var(V4  ),
    ML_GRF_stance_N_var5   = var(V5  ),
    ML_GRF_stance_N_var6   = var(V6  ),
    ML_GRF_stance_N_var7   = var(V7  ),
    ML_GRF_stance_N_var8   = var(V8  ),
    ML_GRF_stance_N_var9   = var(V9  ),
    ML_GRF_stance_N_var10  = var(V10 ),
    ML_GRF_stance_N_var11  = var(V11 ),
    ML_GRF_stance_N_var12  = var(V12 ),
    ML_GRF_stance_N_var13  = var(V13 ),
    ML_GRF_stance_N_var14  = var(V14 ),
    ML_GRF_stance_N_var15  = var(V15 ),
    ML_GRF_stance_N_var16  = var(V16 ),
    ML_GRF_stance_N_var17  = var(V17 ),
    ML_GRF_stance_N_var18  = var(V18 ),
    ML_GRF_stance_N_var19  = var(V19 ),
    ML_GRF_stance_N_var20  = var(V20 ),
    ML_GRF_stance_N_var21  = var(V21 ),
    ML_GRF_stance_N_var22  = var(V22 ),
    ML_GRF_stance_N_var23  = var(V23 ),
    ML_GRF_stance_N_var24  = var(V24 ),
    ML_GRF_stance_N_var25  = var(V25 ),
    ML_GRF_stance_N_var26  = var(V26 ),
    ML_GRF_stance_N_var27  = var(V27 ),
    ML_GRF_stance_N_var28  = var(V28 ),
    ML_GRF_stance_N_var29  = var(V29 ),
    ML_GRF_stance_N_var30  = var(V30 ),
    ML_GRF_stance_N_var31  = var(V31 ),
    ML_GRF_stance_N_var32  = var(V32 ),
    ML_GRF_stance_N_var33  = var(V33 ),
    ML_GRF_stance_N_var34  = var(V34 ),
    ML_GRF_stance_N_var35  = var(V35 ),
    ML_GRF_stance_N_var36  = var(V36 ),
    ML_GRF_stance_N_var37  = var(V37 ),
    ML_GRF_stance_N_var38  = var(V38 ),
    ML_GRF_stance_N_var39  = var(V39 ),
    ML_GRF_stance_N_var40  = var(V40 ),
    ML_GRF_stance_N_var41  = var(V41 ),
    ML_GRF_stance_N_var42  = var(V42 ),
    ML_GRF_stance_N_var43  = var(V43 ),
    ML_GRF_stance_N_var44  = var(V44 ),
    ML_GRF_stance_N_var45  = var(V45 ),
    ML_GRF_stance_N_var46  = var(V46 ),
    ML_GRF_stance_N_var47  = var(V47 ),
    ML_GRF_stance_N_var48  = var(V48 ),
    ML_GRF_stance_N_var49  = var(V49 ),
    ML_GRF_stance_N_var50  = var(V50 ),
    ML_GRF_stance_N_var51  = var(V51 ),
    ML_GRF_stance_N_var52  = var(V52 ),
    ML_GRF_stance_N_var53  = var(V53 ),
    ML_GRF_stance_N_var54  = var(V54 ),
    ML_GRF_stance_N_var55  = var(V55 ),
    ML_GRF_stance_N_var56  = var(V56 ),
    ML_GRF_stance_N_var57  = var(V57 ),
    ML_GRF_stance_N_var58  = var(V58 ),
    ML_GRF_stance_N_var59  = var(V59 ),
    ML_GRF_stance_N_var60  = var(V60 ),
    ML_GRF_stance_N_var61  = var(V61 ),
    ML_GRF_stance_N_var62  = var(V62 ),
    ML_GRF_stance_N_var63  = var(V63 ),
    ML_GRF_stance_N_var64  = var(V64 ),
    ML_GRF_stance_N_var65  = var(V65 ),
    ML_GRF_stance_N_var66  = var(V66 ),
    ML_GRF_stance_N_var67  = var(V67 ),
    ML_GRF_stance_N_var68  = var(V68 ),
    ML_GRF_stance_N_var69  = var(V69 ),
    ML_GRF_stance_N_var70  = var(V70 ),
    ML_GRF_stance_N_var71  = var(V71 ),
    ML_GRF_stance_N_var72  = var(V72 ),
    ML_GRF_stance_N_var73  = var(V73 ),
    ML_GRF_stance_N_var74  = var(V74 ),
    ML_GRF_stance_N_var75  = var(V75 ),
    ML_GRF_stance_N_var76  = var(V76 ),
    ML_GRF_stance_N_var77  = var(V77 ),
    ML_GRF_stance_N_var78  = var(V78 ),
    ML_GRF_stance_N_var79  = var(V79 ),
    ML_GRF_stance_N_var80  = var(V80 ),
    ML_GRF_stance_N_var81  = var(V81 ),
    ML_GRF_stance_N_var82  = var(V82 ),
    ML_GRF_stance_N_var83  = var(V83 ),
    ML_GRF_stance_N_var84  = var(V84 ),
    ML_GRF_stance_N_var85  = var(V85 ),
    ML_GRF_stance_N_var86  = var(V86 ),
    ML_GRF_stance_N_var87  = var(V87 ),
    ML_GRF_stance_N_var88  = var(V88 ),
    ML_GRF_stance_N_var89  = var(V89 ),
    ML_GRF_stance_N_var90  = var(V90 ),
    ML_GRF_stance_N_var91  = var(V91 ),
    ML_GRF_stance_N_var92  = var(V92 ),
    ML_GRF_stance_N_var93  = var(V93 ),
    ML_GRF_stance_N_var94  = var(V94 ),
    ML_GRF_stance_N_var95  = var(V95 ),
    ML_GRF_stance_N_var96  = var(V96 ),
    ML_GRF_stance_N_var97  = var(V97 ),
    ML_GRF_stance_N_var98  = var(V98 ),
    ML_GRF_stance_N_var99  = var(V99 ),
    ML_GRF_stance_N_var100 = var(V100))



ML_GRF_stance_N_var2 <- data.frame(gather(ML_GRF_stance_N_var1, key = "ML_GRF_stance_N_Var", value = "Value", ML_GRF_stance_N_var1, ML_GRF_stance_N_var2, ML_GRF_stance_N_var3, ML_GRF_stance_N_var4, ML_GRF_stance_N_var5, ML_GRF_stance_N_var6, ML_GRF_stance_N_var7, ML_GRF_stance_N_var8, ML_GRF_stance_N_var9, ML_GRF_stance_N_var10, 
                                          ML_GRF_stance_N_var11, ML_GRF_stance_N_var12, ML_GRF_stance_N_var13, ML_GRF_stance_N_var14, ML_GRF_stance_N_var15, ML_GRF_stance_N_var16, ML_GRF_stance_N_var17, ML_GRF_stance_N_var18, 
                                          ML_GRF_stance_N_var19, ML_GRF_stance_N_var20, ML_GRF_stance_N_var21, ML_GRF_stance_N_var22, ML_GRF_stance_N_var23, ML_GRF_stance_N_var24, ML_GRF_stance_N_var25, ML_GRF_stance_N_var26, ML_GRF_stance_N_var27,
                                          ML_GRF_stance_N_var28, ML_GRF_stance_N_var29, ML_GRF_stance_N_var30, ML_GRF_stance_N_var31, ML_GRF_stance_N_var32, ML_GRF_stance_N_var33, ML_GRF_stance_N_var34, ML_GRF_stance_N_var35, ML_GRF_stance_N_var36,
                                          ML_GRF_stance_N_var37, ML_GRF_stance_N_var38, ML_GRF_stance_N_var39, ML_GRF_stance_N_var40, ML_GRF_stance_N_var41, ML_GRF_stance_N_var42, ML_GRF_stance_N_var43, ML_GRF_stance_N_var44, ML_GRF_stance_N_var45,
                                          ML_GRF_stance_N_var46, ML_GRF_stance_N_var47, ML_GRF_stance_N_var48, ML_GRF_stance_N_var49, ML_GRF_stance_N_var50, ML_GRF_stance_N_var51, ML_GRF_stance_N_var52, ML_GRF_stance_N_var53, ML_GRF_stance_N_var54,
                                          ML_GRF_stance_N_var55,ML_GRF_stance_N_var56, ML_GRF_stance_N_var57, ML_GRF_stance_N_var58, ML_GRF_stance_N_var59, ML_GRF_stance_N_var60, ML_GRF_stance_N_var61, ML_GRF_stance_N_var62, ML_GRF_stance_N_var63,
                                          ML_GRF_stance_N_var64, ML_GRF_stance_N_var65, ML_GRF_stance_N_var66, ML_GRF_stance_N_var67, ML_GRF_stance_N_var68, ML_GRF_stance_N_var69, ML_GRF_stance_N_var70, ML_GRF_stance_N_var71, ML_GRF_stance_N_var72, 
                                          ML_GRF_stance_N_var73, ML_GRF_stance_N_var74, ML_GRF_stance_N_var75, ML_GRF_stance_N_var76, ML_GRF_stance_N_var77, ML_GRF_stance_N_var78, ML_GRF_stance_N_var79, ML_GRF_stance_N_var80, ML_GRF_stance_N_var81,
                                          ML_GRF_stance_N_var82,  ML_GRF_stance_N_var83, ML_GRF_stance_N_var84, ML_GRF_stance_N_var85, ML_GRF_stance_N_var86, ML_GRF_stance_N_var87, ML_GRF_stance_N_var88, ML_GRF_stance_N_var89, ML_GRF_stance_N_var90,
                                          ML_GRF_stance_N_var91, ML_GRF_stance_N_var92, ML_GRF_stance_N_var93, ML_GRF_stance_N_var94, ML_GRF_stance_N_var95, ML_GRF_stance_N_var96, ML_GRF_stance_N_var97, ML_GRF_stance_N_var98, ML_GRF_stance_N_var99,
                                          ML_GRF_stance_N_var100))
ML_GRF_stance_N_var2$time <- as.numeric(substr(ML_GRF_stance_N_var2$ML_GRF_stance_N_Var,20,length(ML_GRF_stance_N_var2$ML_GRF_stance_N_Var)))
ML_GRF_stance_N_var3 <-arrange(ML_GRF_stance_N_var2,ID2,time)

mycolors <- rand_color(4545)
ML_GRF_stance_N_VAR_plot <- ggplot(ML_GRF_stance_N_var3) + geom_line(aes(x = time, y = Value, group = ID2, color = ID2)) +
  scale_color_manual(values = mycolors) +
  theme(legend.position="none") + ggtitle("ML_GRF_stance_N Between Trial Variability") + ylab("Variance")

ML_GRF_stance_N_VAR_plot




##Subset down to people who have similar readings -- closer to the mean;

ML_GRF_stance_N_mean <- ML_GRF_stance_N_2 %>% group_by(time) %>% summarise(mean_value = mean(Value)) %>% arrange(time)

ML_GRF_stance_N_2_Diff <- inner_join(ML_GRF_stance_N_2,ML_GRF_stance_N_mean,by = "time") %>% mutate(abs_diff = abs(Value - mean_value))

ML_GRF_stance_N_2_Overall_Diff <- ML_GRF_stance_N_2_Diff %>% group_by(ID2) %>% summarise(Overall_diff = sum(abs_diff))

lower_bound = mean(ML_GRF_stance_N_2_Overall_Diff$Overall_diff) - sd(ML_GRF_stance_N_2_Overall_Diff$Overall_diff)
upper_bound = mean(ML_GRF_stance_N_2_Overall_Diff$Overall_diff) + sd(ML_GRF_stance_N_2_Overall_Diff$Overall_diff)

similar_group_IDs <- ML_GRF_stance_N_2_Overall_Diff %>% filter(Overall_diff>lower_bound & Overall_diff<upper_bound)


ML_GRF_stance_N_similar <- inner_join(ML_GRF_stance_N_3,similar_group_IDs, by  = "ID2" )

mycolors <- rand_color(15696)
ML_GRF_stance_N_plot1 <- ggplot(ML_GRF_stance_N_3) + geom_line(aes(x = time, y = Value, group = ID2, color = ID2)) +
  scale_color_manual(values = mycolors) +
  theme(legend.position="none") + ggtitle("ML_GRF_stance_N All Particiapnts")

mycolors <- rand_color(12752)
ML_GRF_stance_N_plot2 <- ggplot(ML_GRF_stance_N_similar) + geom_line(aes(x = time, y = Value, group = ID2, color = ID2)) +
  scale_color_manual(values = mycolors) +
  theme(legend.position="none") + ggtitle("ML_GRF_stance_N Similar Participants")


require(gridExtra)
grid.arrange(ML_GRF_stance_N_plot1, ML_GRF_stance_N_plot2, ncol = 1 )


ML_GRF_stance_N_subset <- inner_join(ML_GRF_stance_N_1,similar_group_IDs["ID2"], by = "ID2" )

write.csv(ML_GRF_stance_N_subset,"ML_GRF_stance_N_subset.csv", row.names = FALSE)


ML_GRF_stance_N_outlier <- anti_join(ML_GRF_stance_N_1,similar_group_IDs["ID2"], by = "ID2" )

write.csv(ML_GRF_stance_N_outlier,"ML_GRF_stance_N_outlier.csv", row.names = FALSE)



