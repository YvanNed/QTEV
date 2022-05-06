library(RColorBrewer)
library("ggplot2")
library(readxl)
library(tidyverse)
library(plyr); library(dplyr)
library(tidyr)
library(DBI)
library("ggplot2")
library(ggpubr)
library("qqplotr")
library("DescTools")  # can't manage to install it, hope it is not useful
library("PMCMRplus")  # can't manage to install it, hope it is not useful
library(ggsignif)
library("readr")
library("chron")
library(lattice)
library(car)
library(gdata)
library(lsr)
library(prettyR)
library(plyr)
library(dplyr)
library(lmerTest)
library(ggplot2)
library(contrast)
#library(multcomp) make my variable column name as function soif I don't need it, I will not add it for now

rm(list=ls())     # to clear current workspace

setwd("D:\\Yvan_PhD\\PROJECTS\\QTEV\\Analysis_YN")

df_survey <- read.csv2("Formulaires_csv.csv")
# df_survey[is.na(df_survey)] = 0 # turn NA into 0 but I'm not sure it is useful for now so I'm removing it (+ it was not used already since the name of the df wa uncorrect (data_survey instead of df_survey))
df_survey["Departure"] <- df_survey$Departure_hour+(df_survey$Departure_min/60)

#####################################################################################################
####################################### Analyse knowledge ###########################################
#####################################################################################################

# select data of int.
df_knowledge <- select(df_survey, Knowledge_distance, Distance, Knowledge_time, Time_hour, Time_min, Max_speed)
df_knowledge["TimeRef"] <- (df_knowledge$Time_hour*60) + df_knowledge$Time_min # convert knowledge duration in min
df_knowledge <- subset(df_knowledge, select = -c(Time_hour, Time_min)) # remove unnecessary column
#df_knowledge <- df_knowledge[!is.na(df_knowledge)]

# descriptive stat.
describe(df_knowledge$TimeRef[df_knowledge$Knowledge_time == 0], num.desc = c("mean","sd","median","valid.n"))
describe(df_knowledge$TimeRef[df_knowledge$Knowledge_time == 1], num.desc = c("mean","sd","median","valid.n"))

describe(df_knowledge$Distance[df_knowledge$Knowledge_distance == 0], num.desc = c("mean","sd","median","valid.n"))
describe(df_knowledge$Distance[df_knowledge$Knowledge_distance == 1], num.desc = c("mean","sd","median","valid.n"))

describe(df_knowledge$Max_speed, num.desc = c("mean", "sd", "median", "valid.n"))

# plot histogram for duration
# X-axis grid
p1 <- hist(df_knowledge$TimeRef[df_knowledge$Knowledge_time == 0], breaks = 50, main = "Journey's duration *without* knowledge", xlab = "Duration (min)" )
p2 <- hist(df_knowledge$TimeRef[df_knowledge$Knowledge_time == 1], breaks = 50, main = "Journey's duration *with* knowledge", xlab = "Duration (min)" )
plot( p1, col = rgb(233/255,163/255,201/255, 1/2))
plot( p2, col = rgb(161/255,215/255,106/255, 1/2), add = T) 
title("Journey's duration with and without knowledge")

# plot histogram for speed
hist(df_knowledge$Max_speed)

# plot histogram for distance
p3 <- hist(df_knowledge$Distance[df_knowledge$Knowledge_distance == 0], breaks = 20, main = "Journey's distance *without* knowledge", xlab = "Distance (Km)" )
p4 <- hist(df_knowledge$Distance[df_knowledge$Knowledge_distance == 1], breaks = 20, main = "Journey's distance *with* knowledge", xlab = "Distance (Km)" )
plot( p3, col = rgb(233/255,163/255,201/255, 1/2))
plot( p4, col = rgb(161/255,215/255,106/255, 1/2), add = T)


#####################################################################################################
#excluding outliers
df_survey_filt <- filter(df_survey, !Age %in% c(1:17))                                                        # remove participants under 18 years old
df_survey_filt$Age <- replace_na(df_survey_filt$Age, mean(df_survey_filt$Age[!is.na(df_survey_filt$Age)]))    # replace missing age by the mean age
# df_survey_filt <- filter(df_survey_filt, !Age > 70)                                                     # if you want to remove particpant older than 70 years old


#####################################################################################################


#####################################################################################################
########################### Extract Duration data for quick analysis with jsp #######################
#####################################################################################################

# 1- extract the duration data and put them in the right format (min) 
# extract data
df_dur <- select(df_survey_filt, 
                 Time_hour, 		# knowledge or best guess of the total dur. of the travel (hour)
                 Time_min, 		  # knowledge or best guess of the total dur. of the travel (min)
                 Time_hour_num, # numerical estimation of elapsed dur. since departure (hour)
                 Time_min_num, 	# numerical estimation of elapsed dur. since departure (min)
                 Time_graphic, 	# graphical estimation of elapsed dur. (degree from 0 to 360)
                 Hour_3, 		    # objective timing when the estimation was done (hour)
                 min_3)			    # objective timing when the estimation was done (min)

# convert data
df_dur["Rep_dur"] <-  df_dur$Hour_3            + (df_dur$min_3/60)    # convert response time in decimal hour to match the train data
df_dur["Dur_tot"] <- (df_dur$Time_hour*60) 	   + df_dur$Time_min 		  # convert total duration estimation (ref) in min
df_dur["Dur_num"] <- (df_dur$Time_hour_num*60) + df_dur$Time_min_num 	# convert numerical duration estimation in min
df_dur["Dur_grh"] <- (df_dur$Time_graphic/360)*df_dur$Dur_tot			    # convert graphical duration estimation in min - (X°/360)* total duration estimated (in min) 
df_dur["Obj_dur"] <- NA                                               # create an empty column for the objective duration
df_dur["Obj_dur_tot"] <- NA                                           # create an empty column for the total objective duration

#df_dur <- subset(df_dur, select = -c(Time_hour, Time_min, Time_hour_num, Time_min_num, Hour_3, min_3)) # I don't know if I still need the graphical estimation in degree so I'm keeping it for now 

# 2- extract the objective duration elapsed at the moment of estimation
# loop over the number of participant then loop over the number of train file to find the one corresponding to the participant

setwd("D:\\Yvan_PhD\\PROJECTS\\QTEV\\Analysis_YN\\GPS_data\\CSV_CLEAN") # go into train data directory
filenames <- list.files(pattern="*.csv", full.names=TRUE)	# get the list of files in the directory

for (i in 1:nrow(df_survey_filt)) { # loop over the number of participants 
  
    for (k in 1:length(filenames)) { # loop over the number of train data file
  
      name <- filenames[k]
      df_sncf <- read_csv(name)
      
      if (df_survey_filt$Date[i]==df_sncf$Jour.circulation[1]){ # Select the train data file with the corresponding date 
        
        if (floor(df_sncf$Depart.theo.SAT[1])==floor(df_survey_filt$Departure[i])) { # select the train data file with the corresponding time (by using the "partie entière" of the hour)
        
          if (floor(df_sncf$Depart.theo.SAT[1]) > 14) { # the duration and cumulative distance for the train data of the afternoon are incorrect (because they were calculated from Lyon-Perrache), hence we recompute them here
            
            df_sncf$duration <- (c(0,cumsum(diff(df_sncf$Heure.franchissement))))*60
            df_sncf$CumDist <- c(0,cumsum(diff(df_sncf$Distance)/1000))
            
          }
          
          idx <- which(round(df_sncf$Heure.franchissement,digits=2) %in% round(df_dur$Rep_dur[i],digits=2)) # get the index corresponding to the time of the response
          j <- idx[1]
          
          if (is.na(j)){ # if for whatever reason the corresponding time is not found
            idx <- which(round(df_sncf$Heure.franchissement,digits=1) %in% round(df_dur$Rep_dur[i],digits=1))
            j <- idx[1]            
            }
          
          df_dur$Obj_dur[i] <- df_sncf$duration[j]
          
          flag = 0
          while (is.na(df_dur$Obj_dur_tot[i])) {
            df_dur$Obj_dur_tot[i] <- df_sncf$duration[nrow(df_sncf) + flag]
            flag = flag + 1
          }
        }
      }
    }
}

###################################################################################################
################################# Duration as a function of travel time ###########################
###################################################################################################

#DEPARTURE HOUR
data_departure <- select(df_survey,Departure.hour,Departure.min)
data_departure["Departure"] <- data_departure$Departure.hour+(data_departure$Departure.min/60)


setwd("C:/Users/smile/OneDrive/M1 NEUROSCIENCES/Master 1/Stage/Stage 3/Distance and duration tables")
"D:\\Yvan_PhD\\PROJECTS\\QTEV\\Analysis_YN\\GPS_data\\CSV"

filenames <- list.files(pattern="*.csv", full.names=TRUE)

##duration
comparison_duration <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(comparison_duration) <- c('Real', 'Estimated num','Estimated gr', 'Real ratio','Estimated ratio','Estimated total','Real total')


for (i in 1:nrow(df_survey)) { # loop over the number of participants ?

  for (k in 1:length(filenames)) { # loop over the train data file?deri 
  name <- filenames[k]
  df_sncf <- read.csv(filenames[k], sep = ",")
  df_sncf[is.na(df_sncf)] = 0

    if (df_survey$Date[i]==df_sncf$Jour.circulation[1]){

      if (round(df_sncf$Depart.theo.SAT[1],digits=3)==round(data_departure$Departure[i],digits=3)) { 

      ##DURATION
        if (is.na(data_duration$`Time duration`[i])) {
          data_duration$`Time duration`[i] <- 0
          }

        else { 
          
          idx <- which(round(df_sncf$Heure.franchissement,digits=2) %in% round(data_duration$`Time duration`[i],digits=2))
          j <- idx[1]
          if (is.na(j)){
            idx <- which(round(df_sncf$Heure.franchissement,digits=1) %in% round(data_duration$`Time duration`[i],digits=1))
            j <- idx[1]            }
          #if the time t1 the passenger responded matches the time t2 on the data train
          Du <- data.frame(df_sncf$duration[j],data_duration$`Time num`[i],data_duration$`Time gr`[i],df_sncf$`Ratio.duration`[j],data_duration$`Time ratio`[i],data_duration$`Time`[i],df_sncf$duration[nrow(df_sncf)])
          names(Du) <- c('Real', 'Estimated num','Estimated gr', 'Real ratio','Estimated ratio','Estimated total', 'Real total')
          comparison_duration <- rbind(comparison_duration,Du)

        }
        }
      }
    }
}


comparison_duration[is.na(comparison_duration)] = 0
comparison_duration["Subjective deviation"] <- (comparison_duration$`Estimated num`-comparison_duration$Real)/comparison_duration$Real
plot(density(comparison_duration$'Subjective deviation'))


ggscatter(comparison_duration, x = "Real", y = "Estimated num", add = "reg.line") +
  stat_cor(label.x = 0.5, label.y = 2) +
  stat_regline_equation(label.x =0.5, label.y = 1.7)+
  theme_light()+
  labs(x='Real duration', y='Estimated numeric duration', title='Numeric time estimation since departure')+
  geom_smooth(formula = 'y ~ x',method='lm', se=FALSE, color='red3')

ggscatter(comparison_duration, x = "Real", y = "Estimated gr", add = "reg.line") +
  stat_cor(label.x = 0.5, label.y = 2) +
  stat_regline_equation(label.x =0.5, label.y = 1.7)+
  theme_light()+
  labs(x='Real duration', y='Estimated graphical duration', title='Graphical time estimation since departure')+
  geom_smooth(formula = 'y ~ x',method='lm', se=FALSE, color='purple3')

ggscatter(comparison_duration, x = "Real ratio", y = "Estimated ratio", add = "reg.line") +
  stat_cor(label.x = 25, label.y = 90) +
  stat_regline_equation(label.x =25, label.y = 80)+
  theme_light()+
  labs(x='Real duration ratio', y='Estimated duration ratio', title='Graphical time estimation ratio since departure')+
  geom_smooth(formula = 'y ~ x',method='lm', se=FALSE, color='pink3')

ggscatter(comparison_duration, x = "Real total", y = "Estimated total", add = "reg.line") +
  stat_cor(label.x = 2, label.y = 5.5) +
  stat_regline_equation(label.x =2, label.y = 4.5)+
  theme_light()+
  labs(x='Real total duration', y='Estimated total duration', title='Numeric total time estimation')+
  geom_smooth(formula = 'y ~ x',method='lm', se=FALSE, color='green3')
t.test(comparison_duration$"Real total",comparison_duration$"Estimated total")
#t = -1.7767, df = 86.021, p-value = 0.07915
#no significant difference


#############################################################################
################################ DISTANCE ##############################
##########################################################################

setwd("C:/Users/smile/OneDrive/M1 NEUROSCIENCES/Master 1/Stage/Stage 3/Distance and duration tables")

filenames <- list.files(pattern="*.csv", full.names=TRUE)

##distance
comparison_dist <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(comparison_dist) <- c('Real', 'Estimated num','Estimated gr', 'Real ratio','Estimated ratio','Estimated total', 'Real total')

for (i in 1:nrow(df_survey)) {
  
  for (k in 1:length(filenames)) {
    name <- filenames[k]
    df_sncf <- read.csv(filenames[k], sep = ",")
    df_sncf[is.na(df_sncf)] = 0
    
    if (df_survey$Date[i]==df_sncf$Jour.circulation[1]){
      
      if (round(df_sncf$Depart.theo.SAT[1],digits=3)==round(data_departure$Departure[i],digits=3)) { 
        
        ##DISTANCE
        if (is.na(data_dist$`Time dist`[i])) {
          data_dist$`Time dist`[i] <- 0
        }
        
        else { 
          
          idx <- which(round(df_sncf$Heure.franchissement,digits=2) %in% round(data_dist$`Time dist`[i],digits=2))
          j <- idx[1]
          if (is.na(j)){
            idx <- which(round(df_sncf$Heure.franchissement,digits=1) %in% round(data_dist$`Time dist`[i],digits=1))
            j <- idx[1]            }
          #if the time t1 the passenger responded matches the time t2 on the data train
          Di <- data.frame(df_sncf$Distance[j],data_dist$Dist.num[i],data_dist$`Dist gr`[i],df_sncf$`Ratio.dist`[j],data_dist$`Dist ratio`[i],data_dist$`Dist`[i],df_sncf$Distance[nrow(df_sncf)])
          names(Di) <- c('Real', 'Estimated num','Estimated gr', 'Real ratio','Estimated ratio','Estimated total', 'Real total')
          comparison_dist <- rbind(comparison_dist,Di)
          
        }
      }
    }
  }
}


comparison_dist[is.na(comparison_dist)] = 0
comparison_dist["Subjective deviation"] <- (comparison_dist$`Estimated num`-comparison_dist$Real)/comparison_dist$Real
plot(density(comparison_dist$'Subjective deviation'))

ggscatter(comparison_dist, x = "Real", y = "Estimated num", add = "reg.line") +
  stat_cor(label.x = 0, label.y = 600) +
  stat_regline_equation(label.x =0, label.y = 500)+
  theme_light()+
  labs(x='Real distance', y='Estimated numeric distance', title='Numeric distance estimation since departure')+
  geom_smooth(formula = 'y ~ x',method='lm', se=FALSE, color='red3')

ggscatter(comparison_dist, x = "Real", y = "Estimated gr", add = "reg.line") +
  stat_cor(label.x = 0, label.y = 600) +
  stat_regline_equation(label.x =0, label.y = 500)+
  theme_light()+
  labs(x='Real distance', y='Estimated graphical distance', title='Graphical distance estimation since departure')+
  geom_smooth(formula = 'y ~ x',method='lm', se=FALSE, color='purple3')

ggscatter(comparison_dist, x = "Real ratio", y = "Estimated ratio", add = "reg.line") +
  stat_cor(label.x = 25, label.y = 90) +
  stat_regline_equation(label.x =25, label.y = 80)+
  theme_light()+
  labs(x='Real distance ratio', y='Estimated distance ratio', title='Graphical distance estimation ratio since departure')+
  geom_smooth(formula = 'y ~ x',method='lm', se=FALSE, color='pink3')

ggscatter(comparison_dist, x = "Real total", y = "Estimated total", add = "reg.line") +
  stat_cor(label.x = 430, label.y = 700) +
  stat_regline_equation(label.x =430, label.y = 650)+
  theme_light()+
  labs(x='Real total distance', y='Estimated total distance', title='Numeric total distance estimation')+
  geom_smooth(formula = 'y ~ x',method='lm', se=FALSE, color='green3')
t.test(comparison_dist$"Real total",comparison_dist$"Estimated total")
#t = -1.352, df = 86.068, p-value = 0.1799
#no significant difference



#############################################################################
################################ SPEED ####################################
##########################################################################

setwd("C:/Users/smile/OneDrive/M1 NEUROSCIENCES/Master 1/Stage/Stage 3/Distance and duration tables")

filenames <- list.files(pattern="*.csv", full.names=TRUE)

##speed
comparison_speed <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(comparison_speed) <- c('Real', 'Estimated inst num','Estimated inst gr','Max speed', 'Real average speed','Real max speed','Estimated av num','Estimated av gr')

data_speed[is.na(data_speed)]=0


for (i in 1:nrow(df_survey)) {
  
  for (k in 1:length(filenames)) {
    name <- filenames[k]
    df_sncf <- read.csv(filenames[k], sep = ",")
    df_sncf[is.na(df_sncf)] = 0
    
    if (df_survey$Date[i]==df_sncf$Jour.circulation[1]){
      
      if (round(df_sncf$Depart.theo.SAT[1],digits=3)==round(data_departure$Departure[i],digits=3)) { 
        
        ##SPEED
        if (is.na(data_speed$`Time speed`[i])) {
          data_speed$`Time speed`[i] <- 0
        }
        
        else { 
          
          idx <- which(round(df_sncf$Heure.franchissement,digits=2) %in% round(data_speed$`Time speed`[i],digits=2))
          j <- idx[1]
          if (is.na(j)){
            idx <- which(round(df_sncf$Heure.franchissement,digits=1) %in% round(data_speed$`Time speed`[i],digits=1))
            j <- idx[1]            
            }
          #if the time t1 the passenger responded matches the time t2 on the data train
          Sp <- data.frame(df_sncf$Vitesse[j],data_speed$Inst.Speed.num[i],data_speed$Inst.Sp.Gr[i],data_speed$`Max.speed`[i],mean(df_sncf$Vitesse),max(df_sncf$Vitesse),data_speed$Av.speed.num[i],data_speed$Av.Sp.Gr[i])
          names(Sp) <- c('Real', 'Estimated inst num','Estimated inst gr','Max speed', 'Real average speed','Real max speed','Estimated av num','Estimated av gr')
          comparison_speed <- rbind(comparison_speed,Sp)
          
        }
      }
    }
  }
}


comparison_speed[is.na(comparison_speed)] = 0
comparison_speed["Subjective deviation"] <- (comparison_speed$`Estimated inst num` -comparison_speed$Real)/comparison_speed$Real
plot(density(comparison_speed$'Subjective deviation'))


comparison_speed1 <- select(comparison_speed,Real,'Estimated inst num')
comparison_speed1 <- comparison_speed1[apply(comparison_speed1, 1, function(row) all(row !=0 )), ]

ggscatter(comparison_speed1, x = "Real", y = "Estimated inst num", add = "reg.line") +
  stat_cor(label.x = 0, label.y = 350) +
  stat_regline_equation(label.x =0, label.y = 300)+
  theme_light()+
  labs(x='Real speed', y='Estimated numeric speed', title='Instantaneous numeric speed estimation')+
  geom_smooth(formula = 'y ~ x',method='lm', se=FALSE, color='red3')


comparison_speed2 <- select(comparison_speed,Real,'Estimated inst gr')
comparison_speed2 <- comparison_speed2[apply(comparison_speed2, 1, function(row) all(row !=0 )), ]

ggscatter(comparison_speed2, x = "Real", y = "Estimated inst gr", add = "reg.line") +
  stat_cor(label.x = 0, label.y = 350) +
  stat_regline_equation(label.x =0, label.y = 300)+
  theme_light()+
  labs(x='Real speed', y='Estimated graphical speed', title='Instantaneous graphical speed estimation')+
  geom_smooth(formula = 'y ~ x',method='lm', se=FALSE, color='purple3')


comparison_speed3 <- select(comparison_speed,'Real average speed','Estimated av num')
comparison_speed3 <- comparison_speed3[apply(comparison_speed3, 1, function(row) all(row !=0 )), ]

ggscatter(comparison_speed3, x = 'Real average speed', y = "Estimated av num", add = "reg.line") +
  stat_cor(label.x = 205, label.y = 350) +
  stat_regline_equation(label.x =205, label.y = 300)+
  theme_light()+
  labs(x='Real average speed', y='Estimated numeric speed', title='Average numeric speed estimation')+
  geom_smooth(formula = 'y ~ x',method='lm', se=FALSE, color='red3')

comparison_speed4 <- select(comparison_speed,'Real average speed','Estimated av gr')
comparison_speed4 <- comparison_speed4[apply(comparison_speed4, 1, function(row) all(row !=0 )), ]

ggscatter(comparison_speed4, x = 'Real average speed', y = "Estimated av gr", add = "reg.line") +
  stat_cor(label.x = 205, label.y = 350) +
  stat_regline_equation(label.x = 205, label.y = 300)+
  theme_light()+
  labs(x='Real average speed', y='Estimated graphical speed', title='Average graphical speed estimation')+
  geom_smooth(formula = 'y ~ x',method='lm', se=FALSE, color='purple3')


ggplot(comparison_speed,aes(x=comparison_speed$'Max speed')) +
  geom_density()

p <- plot(density(comparison_speed$'Max speed'))


t.test(comparison_speed$"Real max speed",comparison_speed$"Max speed")
#t = 1.8326, df = 86.039, p-value = 0.07033
#no significant difference

##################################################################################################
############################### RELATIONSHIP BETWEEN PARAMETERS #################################
####################################################################################################

op <- select(comparison_duration,Real)
op['estimated duration'] <- comparison_duration$`Estimated num`
op['estimated distance'] <- comparison_dist$`Estimated num`[1:86]
op['estimated speed'] <- comparison_speed$`Estimated inst num`[1:86]
op['calcul'] <- op$`estimated distance`/op$`estimated duration`


ggscatter(op,x = 'estimated duration', y = 'estimated distance', add = "reg.line") +
  stat_cor(label.x = 0, label.y = 600) +
  stat_regline_equation(label.x = 0, label.y = 550)+
  theme_light()+
  labs(x = 'Estimated duration', y = "Estimated distance", title='Distance estimation in function of duration estimation')+
  geom_smooth(formula = 'y ~ x',method='lm', se=FALSE, color='purple3')

ok <- lm( op$"estimated distance" ~ op$'estimated duration', data=op)
durbinWatsonTest(ok) #p=0.098 #no auto-correlation
ncvTest(ok) #p = 0.91372
shapiro.test(ok$residuals) #p-value = 0.03598


ggscatter(op,x = 'calcul', y = 'estimated speed', add = "reg.line") +
  stat_cor(label.x = 3000, label.y = 400) +
  stat_regline_equation(label.x = 3000, label.y = 350)+
  theme_light()+
  labs(x = 'Estimated distance/Estimated duration', y = "Estimated speed", title='Speed estimation in function of distance/duration estimation')+
  geom_smooth(formula = 'y ~ x',method='lm', se=FALSE, color='turquoise3')

on <- lm( op$"estimated speed" ~ op$'calcul', data=op)
durbinWatsonTest(on) #p= 0.642 #no auto-correlation
ncvTest(on) #p = 0.65599
shapiro.test(on$residuals) #p-value = 6.938e-05

#############################################################################################
########################### VERIFICATION ###################################################
#################################################################################################

### DURATION

model1 <- lm( comparison_duration$"Estimated num" ~ Real, data=comparison_duration)
durbinWatsonTest(model1) #p=0.328 #no auto-correlation
ncvTest(model1) #0.66055
shapiro.test(model1$residuals) # 1.193e-05

model2 <- lm( comparison_duration$"Estimated gr" ~ Real, data=comparison_duration)
durbinWatsonTest(model2) #p=0.232 #no auto-correlation
ncvTest(model2) #0.34539
shapiro.test(model2$residuals) # p-value = 3.128e-05

model3 <- lm(comparison_duration$"Estimated ratio" ~ comparison_duration$"Real ratio", data=comparison_duration)
durbinWatsonTest(model3) #p=0.302 #no auto correlation
ncvTest(model3) #0.34539
shapiro.test(model3$residuals) # p-value = 3.128e-05

model4 <-lm( comparison_duration$"Estimated total" ~ comparison_duration$"Real total", data=comparison_duration)    
durbinWatsonTest(model4) #p=0.876 #no auto correlation
ncvTest(model4) #0.57802
shapiro.test(model4$residuals) #p-value < 2.2e-16


######## DISTANCE

di1 <- lm( comparison_dist$"Estimated num" ~ Real, data=comparison_dist)
durbinWatsonTest(di1) #0.046 #almost
ncvTest(di1) #0.011116
shapiro.test(di1$residuals) #p-value = 0.00563

di2 <- lm( comparison_dist$"Estimated gr" ~ Real, data=comparison_dist)
durbinWatsonTest(di2) #0.722
ncvTest(di2) #0.033093
shapiro.test(di2$residuals) #p-value = 0.08102

di3  <- lm( comparison_dist$"Estimated ratio" ~ comparison_dist$"Real ratio", data=comparison_dist)    
durbinWatsonTest(di3) #0.974
ncvTest(di3) # 0.089466
shapiro.test(di3$residuals) #p-value = 2.747e-06

di4  <- lm( comparison_dist$"Estimated total" ~ comparison_dist$"Real total", data=comparison_dist)
durbinWatsonTest(di4) #0.314
ncvTest(di4) #0.33286
shapiro.test(di4$residuals) #p-value = 0.08724

############# SPEED

comparison_speed1 <- select(comparison_speed,Real,'Estimated inst num')
comparison_speed1 <- comparison_speed1[apply(comparison_speed1, 1, function(row) all(row !=0 )), ]
sp1 <- lm( comparison_speed1$"Estimated inst num" ~ comparison_speed1$"Real", data=comparison_speed1)
durbinWatsonTest(sp1) # 0.78
ncvTest(sp1) #0.60678
shapiro.test(sp1$residuals) #p-value = 0.5469


comparison_speed2 <- select(comparison_speed,Real,'Estimated inst gr')
comparison_speed2 <- comparison_speed2[apply(comparison_speed2, 1, function(row) all(row !=0 )), ]

x1 = comparison_speed2$"Real"
y1 = comparison_speed2$"Estimated inst gr"
sp2 <- lm(y1 ~ x1, comparison_speed2)
durbinWatsonTest(sp2) # 0.196
ncvTest(sp2) #0.58976
shapiro.test(sp2$residuals) #p-value = 0.6394


comparison_speed3 <- select(comparison_speed,'Real average speed','Estimated av num')
comparison_speed3 <- comparison_speed3[apply(comparison_speed3, 1, function(row) all(row !=0 )), ]

x2 = comparison_speed3$'Real average speed'
y2 = comparison_speed3$"Estimated av num"
sp3 <- lm(y2 ~ x2, comparison_speed3)
durbinWatsonTest(sp3) #0.282
ncvTest(sp3) #p = 0.36233
shapiro.test(sp3$residuals) #p-value = 0.1148


comparison_speed4 <- select(comparison_speed,'Real average speed','Estimated av gr')
comparison_speed4 <- comparison_speed4[apply(comparison_speed4, 1, function(row) all(row !=0 )), ]

x3 = comparison_speed4$'Real average speed'
y3 = comparison_speed4$"Estimated av gr"
sp4 <- lm(y3 ~ x3, comparison_speed4)
durbinWatsonTest(sp4) #0.608
ncvTest(sp4) #p = 0.021595
shapiro.test(sp4$residuals) #p-value = 0.3754
