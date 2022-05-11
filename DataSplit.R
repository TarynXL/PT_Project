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




##Subset down to people who have similar readings -- closer to the mean;

#Take mean value at each point in time
ML_GRF_stance_N_mean <- ML_GRF_stance_N_2 %>% group_by(time) %>% 
  summarise(mean_value = mean(Value)) %>% 
  arrange(time)

#Calculate the difference between each obs at each point of time
ML_GRF_stance_N_2_Diff <- inner_join(ML_GRF_stance_N_2,ML_GRF_stance_N_mean,by = "time") %>% 
  mutate(abs_diff = abs(Value - mean_value))

#Calculate the overall difference for each trial
ML_GRF_stance_N_2_Overall_Diff <- ML_GRF_stance_N_2_Diff %>% 
  group_by(ID2) %>% 
  summarise(Overall_diff = sum(abs_diff))

#Calculate 1 sd above and below the mean as upper and lower thresholds
lower_bound = mean(ML_GRF_stance_N_2_Overall_Diff$Overall_diff) - sd(ML_GRF_stance_N_2_Overall_Diff$Overall_diff)
upper_bound = mean(ML_GRF_stance_N_2_Overall_Diff$Overall_diff) + sd(ML_GRF_stance_N_2_Overall_Diff$Overall_diff)

#Subset to "typical" trials
similar_group_IDs <- ML_GRF_stance_N_2_Overall_Diff %>% 
  filter(Overall_diff>lower_bound & Overall_diff<upper_bound)

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


#output split dataframes
ML_GRF_stance_N_subset <- inner_join(ML_GRF_stance_N_1,similar_group_IDs["ID2"], by = "ID2" )

write.csv(ML_GRF_stance_N_subset,"ML_GRF_stance_N_subset.csv", row.names = FALSE)


ML_GRF_stance_N_outlier <- anti_join(ML_GRF_stance_N_1,similar_group_IDs["ID2"], by = "ID2" )

write.csv(ML_GRF_stance_N_outlier,"ML_GRF_stance_N_outlier.csv", row.names = FALSE)



