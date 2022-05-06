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

setwd("C:\\Users\\yvann\\OneDrive\\Bureau\\Analysis_YN")

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
########################### Extract Duration data for quick analysis with jasp #######################
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
                 min_3,         # objective timing when the estimation was done (min)
                 Direction)			    

# convert data
df_dur["Rep_dur"] <-  df_dur$Hour_3            + (df_dur$min_3/60)    # convert response time in decimal hour to match the train data
df_dur["Dur_tot"] <- (df_dur$Time_hour*60) 	   + df_dur$Time_min 		  # convert total duration estimation (ref) in min
df_dur["Dur_num"] <- (df_dur$Time_hour_num*60) + df_dur$Time_min_num 	# convert numerical duration estimation in min
df_dur["Dur_grh"] <- (df_dur$Time_graphic/360)*df_dur$Dur_tot			    # convert graphical duration estimation in min - (X°/360)* total duration estimated (in min) 
df_dur["Obj_dur"] <- NA                                               # create an empty column for the objective duration
df_dur["Obj_dur_tot"] <- NA                                           # create an empty column for the total objective duration
df_dur$Direction[df_dur$Direction == 1] <- "FF"                       # convert direction into categorical data FF = Facing Forward
df_dur$Direction[df_dur$Direction == 2] <- "FB"                       # convert direction into categorical data FB = Facing backward

df_dur <- subset(df_dur, select = -c(Time_hour, Time_min, Time_hour_num, Time_min_num, Hour_3, min_3, Time_graphic)) # Remove unnecessary data 

# 2- extract the objective duration elapsed at the moment of estimation
# loop over the number of participant then loop over the number of train file to find the one corresponding to the participant

setwd("C:\\Users\\yvann\\OneDrive\\Bureau\\Analysis_YN\\GPS_data\\CSV_CLEAN") # go into train data directory
filenames <- list.files(pattern="*.csv", full.names=TRUE)	# get the list of files in the directory

for (i in 1:nrow(df_survey_filt)) { # loop over the number of participants 
  
    for (k in 1:length(filenames)) { # loop over the number of train data file
  
      name <- filenames[k]
      df_sncf <- read_csv(name)
      
      if (df_survey_filt$Date[i]==df_sncf$Jour.circulation[1]){ # Select the train data file with the corresponding date 
        
        if (floor(df_sncf$Depart.theo.SAT[1]) < 14 && floor(df_survey_filt$Departure[i]) < 14) { # select the train data file with the corresponding time (by using the "partie entière" of the hour)
          
          idx <- which(round(df_sncf$Heure.franchissement,digits=2) %in% round(df_dur$Rep_dur[i],digits=2)) # get the index corresponding to the time of the response
          j <- idx[1]
          
          if (is.na(j)){ # if for whatever reason the corresponding time is not found
            idx <- which(round(df_sncf$Heure.franchissement,digits=1) %in% round(df_dur$Rep_dur[i],digits=1))
            j <- idx[1]            
            }
          
          df_dur$Obj_dur[i] <- df_sncf$duration[j]
          
          df_dur$Obj_dur_tot[i] <- df_sncf$duration[nrow(df_sncf)] # for now it will not work for every one but I think that my while loop down below is not appreciated by my pc
          
          # flag = 0
          # while (is.na(df_dur$Obj_dur_tot[i])) {
          #   df_dur$Obj_dur_tot[i] <- df_sncf$duration[nrow(df_sncf) + flag]
          #   flag = flag + 1
          # }
        }
        
          if (floor(df_sncf$Depart.theo.SAT[1]) > 14 && floor(df_survey_filt$Departure[i]) > 14) { # select the train data file with the corresponding time (by using the "partie entière" of the hour and to contrast the morning vs afternoon)
            
            # the duration and cumulative distance for the train data of the afternoon are incorrect (because they were calculated from Lyon-Perrache), hence we recompute them here
              
            df_sncf$duration <- (c(0,cumsum(diff(df_sncf$Heure.franchissement))))*60
            df_sncf$CumDist <- c(0,cumsum(diff(df_sncf$Distance)/1000))
            
            idx <- which(round(df_sncf$Heure.franchissement,digits=2) %in% round(df_dur$Rep_dur[i],digits=2)) # get the index corresponding to the time of the response
            j <- idx[1]
            
            if (is.na(j)){ # if for whatever reason the corresponding time is not found
              idx <- which(round(df_sncf$Heure.franchissement,digits=1) %in% round(df_dur$Rep_dur[i],digits=1))
              j <- idx[1]            
            }
            
            df_dur$Obj_dur[i] <- df_sncf$duration[j]
            
            df_dur$Obj_dur_tot[i] <- df_sncf$duration[nrow(df_sncf)] # for now it will not work for every one but I think that my while loop down below is not appreciated by my pc
            
            # flag = 0
            # while (is.na(df_dur$Obj_dur_tot[i])) {
            #   df_dur$Obj_dur_tot[i] <- df_sncf$duration[nrow(df_sncf) + flag]
            #   flag = flag + 1
            # }
        }
      }
    }
}

df_dur["ER_num"] <- df_dur$Dur_num - df_dur$Obj_dur
df_dur["RE_num"] <- df_dur$Dur_num / df_dur$Obj_dur
df_dur["ER_grh"] <- df_dur$Dur_grh - df_dur$Obj_dur
df_dur["RE_grh"] <- df_dur$Dur_grh / df_dur$Obj_dur
df_dur["Speed_Episode"] <- NA
df_dur$Speed_Episode[df_dur$Obj_dur <= "mean duration of acceleration phase" ]                <- "acc" # sill need to compute it with the script identify_speed_episode
df_dur$Speed_Episode[df_dur$Obj_dur > "mean duration of acceleration phase" && df_dur$Obj_dur <- "mean duration of constant phase" ] <- "cst"# sill need to compute it with the script identify_speed_episode
df_dur$Speed_Episode[df_dur$Obj_dur >= "mean duration of  constant phase" ]                   <- "dec" # sill need to compute it with the script identify_speed_episode

# Saving the file to be used in jasp
write.csv(df_dur, "C:\\Users\\yvann\\OneDrive\\Bureau\\Analysis_YN\\Duration_jsp.csv")

#####################################################################################################
########################### Extract Distance data for quick analysis with jasp ######################
#####################################################################################################

# 1- extract the duration data and put them in the right format (min) 
# extract data

df_dist <- select(df_survey_filt, 
                 Distance, 		  # knowledge or best guess of the total dist. of the travel (km)
                 Dist_num,      # numerical estimation of elapsed dist. since departure (km)
                 Dist_graph, 	  # numerical estimation of elapsed dist. since departure (cm)
                 Hour_4, 		    # objective timing when the estimation was done (hour)
                 min_4)			    # objective timing when the estimation was done (min)

# convert data
df_dist["Rep_dist"] <-  df_dist$Hour_4    + (df_dist$min_4/60)          # convert response time in decimal hour to match the train data
df_dist["Dist_grh"] <- (df_dist$Dist_graph/6.3)*df_dist$Distance			  # convert graphical distance estimation in min - (X°/6.3)* total duration estimated (in km) 
df_dist["Obj_dist"] <- NA                                               # create an empty column for the objective distance
df_dist["Obj_dist_tot"] <- NA                                           # create an empty column for the total objective distance


# 2- extract the objective distance elapsed at the moment of estimation
# loop over the number of participant then loop over the number of train file to find the one corresponding to the participant

setwd("C:\\Users\\yvann\\OneDrive\\Bureau\\Analysis_YN\\GPS_data\\CSV_CLEAN") # go into train data directory
filenames <- list.files(pattern="*.csv", full.names=TRUE)	# get the list of files in the directory

for (i in 1:nrow(df_survey_filt)) { # loop over the number of participants 
  
  for (k in 1:length(filenames)) { # loop over the number of train data file
    
    name <- filenames[k]
    df_sncf <- read_csv(name)
    
    if (df_survey_filt$Date[i]==df_sncf$Jour.circulation[1]){ # Select the train data file with the corresponding date 
      
      if (floor(df_sncf$Depart.theo.SAT[1]) < 14 && floor(df_survey_filt$Departure[i]) < 14) { # select the train data file with the corresponding time (by using the "partie entière" of the hour)
        
        idx <- which(round(df_sncf$Heure.franchissement,digits=2) %in% round(df_dist$Rep_dist[i],digits=2)) # get the index corresponding to the time of the response
        j <- idx[1]
        
        if (is.na(j)){ # if for whatever reason the corresponding time is not found
          idx <- which(round(df_sncf$Heure.franchissement,digits=1) %in% round(df_dist$Rep_dist[i],digits=1))
          j <- idx[1]            
        }
        
        df_dist$Obj_dist[i] <- df_sncf$CumDist[j]
        
        df_dist$Obj_dist_tot[i] <- df_sncf$CumDist[nrow(df_sncf)] # for now it will not work for every one but I think that my while loop down below is not appreciated by my pc
        
        # flag = 0
        # while (is.na(df_dur$Obj_dur_tot[i])) {
        #   df_dur$Obj_dur_tot[i] <- df_sncf$duration[nrow(df_sncf) + flag]
        #   flag = flag + 1
        # }
      }
      
      if (floor(df_sncf$Depart.theo.SAT[1]) > 14 && floor(df_survey_filt$Departure[i]) > 14) { # select the train data file with the corresponding time (by using the "partie entière" of the hour and to contrast the morning vs afternoon)
        
        # the duration and cumulative distance for the train data of the afternoon are incorrect (because they were calculated from Lyon-Perrache), hence we recompute them here
          
        df_sncf$duration <- (c(0,cumsum(diff(df_sncf$Heure.franchissement))))*60
        df_sncf$CumDist <- c(0,cumsum(diff(df_sncf$Distance)/1000))
        
        idx <- which(round(df_sncf$Heure.franchissement,digits=2) %in% round(df_dist$Rep_dist[i],digits=2)) # get the index corresponding to the time of the response
        j <- idx[1]
        
        if (is.na(j)){ # if for whatever reason the corresponding time is not found
          idx <- which(round(df_sncf$Heure.franchissement,digits=1) %in% round(df_dist$Rep_dist[i],digits=1))
          j <- idx[1]            
        }
        
        df_dist$Obj_dist[i] <- df_sncf$CumDist[j]
        
        df_dist$Obj_dist_tot[i] <- df_sncf$CumDist[nrow(df_sncf)] # for now it will not work for every one but I think that my while loop down below is not appreciated by my pc
        
        # flag = 0
        # while (is.na(df_dur$Obj_dur_tot[i])) {
        #   df_dur$Obj_dur_tot[i] <- df_sncf$duration[nrow(df_sncf) + flag]
        #   flag = flag + 1
        # }
      }
    }
  }
}

# Saving the file to be used in jasp
write.csv(df_dist, "C:\\Users\\yvann\\OneDrive\\Bureau\\Analysis_YN\\Distance_jsp.csv")

#####################################################################################################
########################### Extract Speed data for quick analysis with jasp #########################
#####################################################################################################

# 1- extract the duration data and put them in the right format (min) 
# extract data

df_spd <- select(df_survey_filt, 
                  Max_speed, 		  # knowledge or best guess of the maximal speed of the travel (km/h)
                  Inst_Speed_num, # numerical estimation of instantaneous speed (km/h)
                  Inst_Speed_graphic, # graphic estimation of instantaneous speed in degree (max is 180°)
                  Av_speed_num, 	  # numerical estimation of average speed since departure (km/h)
                  Av_speed_graphic, # graphic estimation of average speed since departure (km/h)
                  Hour_2, 		    # objective timing when the estimation was done (hour)
                  min_2)			    # objective timing when the estimation was done (min)

# convert data
df_spd["Rep_spd"] <-  df_spd$Hour_2    + (df_spd$min_2/60)          # convert response time in decimal hour to match the train data
df_spd["Inst_Spd_grh"] <- (df_spd$Inst_Speed_graphic/180)*df_spd$Max_speed		# convert graphical instantaneous speed estimation in km/h - (X°/180)* total speed estimated (in km/h) 
df_spd["Ave_Spd_grh"] <- (df_spd$Av_speed_graphic/180)*df_spd$Max_speed			  # convert graphical average speed estimation in km/h - (X°/180)* total speed estimated (in km/h) 
df_spd["Obj_inst_spd"] <- NA                                                  # create an empty column for the objective inst. speed
df_spd["Obj_ave_spd"] <- NA                                                   # create an empty column for the objective ave. speed
df_spd["Obj_max_spd"] <- NA                                                   # create an empty column for the maximum speed

# 2- extract the objective speed at the moment of estimation
# loop over the number of participant then loop over the number of train file to find the one corresponding to the participant

setwd("C:\\Users\\yvann\\OneDrive\\Bureau\\Analysis_YN\\GPS_data\\CSV_CLEAN") # go into train data directory
filenames <- list.files(pattern="*.csv", full.names=TRUE)	# get the list of files in the directory

for (i in 1:nrow(df_survey_filt)) { # loop over the number of participants 
  
  for (k in 1:length(filenames)) { # loop over the number of train data file
    
    name <- filenames[k]
    df_sncf <- read_csv(name)
    
    # compute mean speed that was missing untile now
    df_sncf["Ave_speed"] <- NA
    m_speed = 0
    
    for (x in 1:nrow(df_sncf)) {
      m_speed <- (m_speed + df_sncf$Vitesse[x])/x
      df_sncf$Ave_speed[x] <- m_speed 
    }
    
    if (df_survey_filt$Date[i]==df_sncf$Jour.circulation[1]){ # Select the train data file with the corresponding date 
      
      if (floor(df_sncf$Depart.theo.SAT[1]) < 14 && floor(df_survey_filt$Departure[i]) < 14) { # select the train data file with the corresponding time (by using the "partie entière" of the hour)
        
        idx <- which(round(df_sncf$Heure.franchissement,digits=2) %in% round(df_spd$Rep_spd[i],digits=2)) # get the index corresponding to the time of the response
        j <- idx[1]
        
        if (is.na(j)){ # if for whatever reason the corresponding time is not found
          idx <- which(round(df_sncf$Heure.franchissement,digits=1) %in% round(df_spd$Rep_spd[i],digits=1))
          j <- idx[1]            
        }
        
        df_spd$Obj_inst_spd[i] <- df_sncf$Vitesse[j]
        
        df_spd$Obj_ave_spd[i] <- df_sncf$Ave_speed[j]
        
        df_spd$Obj_max_spd[i] <- max(df_sncf$Vitesse)
      }
      
      if (floor(df_sncf$Depart.theo.SAT[1]) > 14 && floor(df_survey_filt$Departure[i]) > 14) { # select the train data file with the corresponding time (by using the "partie entière" of the hour and to contrast the morning vs afternoon)
        
        # the duration and cumulative distance for the train data of the afternoon are incorrect (because they were calculated from Lyon-Perrache), hence we recompute them here
        
        df_sncf$duration <- (c(0,cumsum(diff(df_sncf$Heure.franchissement))))*60
        df_sncf$CumDist <- c(0,cumsum(diff(df_sncf$Distance)/1000))
        
        idx <- which(round(df_sncf$Heure.franchissement,digits=2) %in% round(df_spd$Rep_spd[i],digits=2)) # get the index corresponding to the time of the response
        j <- idx[1]
        
        if (is.na(j)){ # if for whatever reason the corresponding time is not found
          idx <- which(round(df_sncf$Heure.franchissement,digits=1) %in% round(df_spd$Rep_spd[i],digits=1))
          j <- idx[1]            
        }
        
        df_spd$Obj_inst_spd[i] <- df_sncf$Vitesse[j]
        
        df_spd$Obj_ave_spd[i] <- df_sncf$Ave_speed[j]
        
        df_spd$Obj_max_spd[i] <- max(df_sncf$Vitesse)
      }
    }
  }
}

# Saving the file to be used in jasp
write.csv(df_spd, "C:\\Users\\yvann\\OneDrive\\Bureau\\Analysis_YN\\Speed_jsp.csv")

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
