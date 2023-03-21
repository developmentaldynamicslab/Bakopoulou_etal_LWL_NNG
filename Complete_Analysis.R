#### Full Analysis Script for Bakapoulou et al. "Vocabulary creates automatic attention:
#### The Relation between novel words and gaze dynamics in noun generalization. 
#### 08/11/22 ######################################################### Edit date and update line at beginning of RT about prior code to run
#### Analyses and plots follow the order presented in the manuscript. After running the
#### first section that imports the data and sets up the initial structures it is possible 
#### to jump to later analyses. 


# Load Necessary Libraries ------------------------------------------------

library(MASS)
library(glmmTMB)
library(dplyr)
library(readr)
library(tidyr)
library(forcats)
library(lubridate)
library(eyetrackingR)
library(ggplot2); theme_set(theme_classic(base_size = 20))
library(ggpubr)
library(here)
library(papaja)
library(jtools)
library(tidyverse)
library(lme4) 
library(ggdist)
library(see)





# Import data and set up initial data structures --------------------------

#### This will set up the initial dataset that is the basis for all the datasets used
#### in the analyses. It combines the coded looking data and an excel file of participant
#### information.

#### Import the looking and NNG selection data
#### #################################See NAME OF README FILE for more information
Data <- readRDS('data//Data_public.rds')

#### Clean the data removing incomplete trials
Data <- Data %>%
  filter(Utrial != 'NR_NR_NR' & Utrial != 'nr_nr_nr')

#### import participant information including vocabulary and gender
#### See NAME OF README FILE for more information
part_info <- read_csv('data/OCDI_GENDER_CM.csv') 

#### remove the outliers
#### data from these three participants are analyzed separately in the supplemental materials
#### See NAME OF SUPPLEMENTAL SCRIPT for more information
part_info  <- part_info  %>%
  filter(!ID == "23ENNGxx059B",
         !ID == "26ENNGxx049B",
         !ID == "29ENNGxx074G") 

#### Recode object order information to create blocks matching order of presentation of the test sets
part_info <- part_info %>% 
  mutate(block1=substr(Novel_Object_Order,1,1),
         block2=substr(Novel_Object_Order,2,2),
         block3=substr(Novel_Object_Order,3,3),
         block4=substr(Novel_Object_Order,4,4))

#### Create a set variable
part_info_Order <- part_info %>% 
  gather(Order,value, block1, block2, block3, block4) %>% 
  mutate(value = ifelse(value== "F","Fum",value),
         value = ifelse(value== "M","Mip",value),
         value = ifelse(value== "K","Kiv",value),
         value = ifelse(value== "Z","Zup",value)) %>% 
  rename(Set=value)

#### create vocabulary groups
#### break used is based on the same percentage of nouns known on OCDI
#### as Samuelson and Smith (1999) used for MBCDI
#### Most analyses are run with vocabulary as a continuous variable
#### but these groups are sometimes used for visualization
part_info$Vgrp <- ifelse(part_info$OCDI_2_10 %in% 0:93, "Low",
                         ifelse(part_info$OCDI_2_10 %in% 94:194, "High",0))

#### Merge the the OCDI and the looking data 
Data <- left_join(part_info, Data, by = c("ID","Age")) 

#### Create an ENNG dataset to be used for analysis of selection, looking and transitions
#### and remove no response or incomplete trials
ENNG_data <- Data %>% 
  filter(!Final_Selection == "nr",
         !Final_Selection == "NA",
         !Final_Selection == "NR")


#### Create a dataframe used in the analysis of choice and familiarization data
#### This frame only includes the trials with novel words/objects and
#### pulls out the set labels from the unique trial identifier. 
selbytrial_wset <- ENNG_data %>% 
  group_by(ID, Utrial , Age, Gender, Age_days, `OCDI_2_10`,Vgrp) %>% 
  filter(TrialSection.type != 'f') %>% 
  summarise(Final_Selection = first(Final_Selection)) %>% 
  mutate(Set = ifelse(Utrial == "n_f_1", 'Fum',ifelse(Utrial == "n_f_2", 'Fum',
                                                      ifelse(Utrial == "n_f_3", 'Fum',
                                                             ifelse(Utrial == "n_f_4", 'Fum', 
                                                                    ifelse(Utrial == "n_z_1", 'Zup',
                                                                           ifelse(Utrial == "n_z_2", 'Zup',
                                                                                  ifelse(Utrial == "n_z_3", 'Zup',
                                                                                         ifelse(Utrial == "n_z_4", 'Zup', 
                                                                                                ifelse(Utrial == "n_k_1", 'Kiv',
                                                                                                       ifelse(Utrial == "n_k_2", 'Kiv',
                                                                                                              ifelse(Utrial == "n_k_3", 'Kiv',
                                                                                                                     ifelse(Utrial == "n_k_4", 'Kiv',
                                                                                                                            ifelse(Utrial == "n_m_1", 'Mip',
                                                                                                                                   ifelse(Utrial == "n_m_2", 'Mip',
                                                                                                                                          ifelse(Utrial == "n_m_3", 'Mip',
                                                                                                                                                 ifelse(Utrial == "n_m_4", 'Mip', 0 ))))))))))))))))) %>% 
  ungroup()

#### Add final selection count data based on codes for final selections
ChoiceByTrial <- selbytrial_wset %>% 
  group_by(ID, Age, Gender,  Age_days,`OCDI_2_10`, Utrial, Set, Vgrp) %>% 
  mutate(MaChoice = ifelse(Final_Selection == "m", 1, 0), 
         ShChoice = ifelse(Final_Selection == "s", 1, 0)) %>% 
  ungroup()





# Analysis of NNG Selection Data ------------------------------------------

#### Create a dataframe with overall proportion shape choices for each participant
#### Proportions are calculated as number of shape/material choices out of total responses
EachPart_PropShapeData <- ChoiceByTrial %>% 
  group_by(ID, Age, Gender,  Age_days,`OCDI_2_10`,Vgrp) %>%
  summarise(PropS = sum(ShChoice,na.rm = TRUE)/(sum(ShChoice,na.rm = TRUE) + 
                                                   sum(MaChoice,na.rm = TRUE)),
            PropM = sum(MaChoice,na.rm = TRUE)/(sum(MaChoice,na.rm = TRUE)+ 
                                                      sum(ShChoice,na.rm = TRUE))) %>%
  ungroup()

#### Create a total shape/material count per participant 
EachPart_ChoiceCounts <- ChoiceByTrial %>%
  group_by(ID, Age, Gender, Age_days,`OCDI_2_10`,Vgrp) %>%
  summarise(TotalShCount = sum(ShChoice,na.rm = TRUE),
            TotalMaCount = sum(MaChoice,na.rm = TRUE))%>%
  ungroup()

#### Mean, standard deviation and median for noun vocabulary
mean(EachPart_ChoiceCounts$OCDI_2_10) #105.8364
sd(EachPart_ChoiceCounts$OCDI_2_10) #67.64443
median(EachPart_ChoiceCounts$OCDI_2_10) #123


#### correlation between number of nouns produced and proportion shape selections
#### Figure 2 
ggscatter(EachPart_PropShapeData, x = "OCDI_2_10", y = "PropS",
          add = "reg.line", conf.int = F,
          cor.coef = TRUE, cor.method = "pearson") +
           scale_y_continuous(limits = c(0,1))+ 
  labs(x = 'Number of Nouns Produced', 
       y = 'Proportion Shape Choices')+ 
  geom_hline(yintercept = 0.5, colour = 'black', linetype = 2) +
  papaja::theme_apa(base_size = 22)

####
#### regression on proportion shape choices 

#### Create dataframe with proportion shape by stimulus set for each participant
PropShapeDataTrial_Set <- ChoiceByTrial %>%
  group_by(ID, Age, Gender, Age_days,`OCDI_2_10`,Vgrp,Set) %>%
  summarise(PropS = sum(ShChoice,na.rm = TRUE)/(sum(ShChoice,na.rm = TRUE) + 
                                                  sum(MaChoice,na.rm = TRUE)),
            PropM = sum(MaChoice,na.rm = TRUE)/(sum(MaChoice,na.rm = TRUE)+ 
                                                  sum(ShChoice,na.rm = TRUE))) %>%
  ungroup()

#### scale and center the factors
PropShapeDataTrial_Set$Gender_sc <-
  ifelse(PropShapeDataTrial_Set$Gender == 'G' , 0.5,
         ifelse(PropShapeDataTrial_Set$Gender == 'B', -0.5, NA))

PropShapeDataTrial_Set$OCDI_2_10_sc <- scale(PropShapeDataTrial_Set$OCDI_2_10 , center=TRUE, scale=TRUE)

#### Center the proportion shape data to enable interpretation of the intercept.
#### positive significant effect of intercept indicates shape bias
PropShapeDataTrial_Set$PropS_sc <- (PropShapeDataTrial_Set$PropS -0.5)

#### First model predicting proportion shape choices by gender, vocabulary and set
ModelShapeProp_sc1 <- lm(PropS_sc ~
                             Gender_sc * OCDI_2_10_sc * Set,
                           data = PropShapeDataTrial_Set)

summary(ModelShapeProp_sc1)

#### No effect of set. Remove that factor by going back to dataset without that factor to run a new model
#### Scale the remaining predictors in that dataframe
EachPart_PropShapeData$Gender_sc <-
  ifelse(EachPart_PropShapeData$Gender == 'G' , 0.5,
         ifelse(EachPart_PropShapeData$Gender == 'B', -0.5, NA))

EachPart_PropShapeData$OCDI_2_10_sc <- scale(EachPart_PropShapeData$OCDI_2_10 , center=TRUE, scale=TRUE)

#### Centering proportion shape data to enable interpretation of the intercept.
#### positive significant effect of intercept indicates shape bias
EachPart_PropShapeData$PropS_sc <- (EachPart_PropShapeData$PropS -0.5)

ModelShapeProp_sc2 <- lm(PropS_sc ~
                           Gender_sc * OCDI_2_10_sc,
                            data = EachPart_PropShapeData)

summary(ModelShapeProp_sc2)
#### Final model of noun generalization selections by continuous vocabulary
# Call:
#   lm(formula = PropS_sc ~ Gender_sc * OCDI_2_10_sc, data = EachPart_PropShapeData)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.30550 -0.13708  0.03184  0.11167  0.27365 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)             0.16772    0.02263   7.413 1.21e-09 ***
#   Gender_sc              -0.04287    0.04525  -0.947   0.3480    
#   OCDI_2_10_sc            0.06043    0.02280   2.650   0.0107 *  
#   Gender_sc:OCDI_2_10_sc -0.08324    0.04560  -1.826   0.0738 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#   
#   Residual standard error: 0.1663 on 51 degrees of freedom
#   Multiple R-squared:  0.1705,	Adjusted R-squared:  0.1217 
#   F-statistic: 3.494 on 3 and 51 DF,  p-value: 0.02204





# Analysis of Looking Time Course -----------------------------------------

#### Create eyetrackingR data, See (http://www.eyetracking-r.com/) for more information.
Etlooking_data <- make_eyetrackingr_data(data = ENNG_data,
                                 participant_column = 'ID',
                                 trial_column = 'Utrial',
                                 trackloss_column = 'Look_Off', 
                                 time_column = 'Timestamp',
                                 aoi_columns = c('Look_Shape', 'Look_Material', 'Look_Up'),
                                 treat_non_aoi_looks_as_missing = F)

#### Set timestamp 0 to be the onset of the name 
Etlooking_data2 <- subset_by_window(Etlooking_data,
                            window_start_msg = 'NAME',
                            msg_col = 'Name_Start',
                            rezero = T,
                            remove = F)
#### Note that a trial for participant 17ENNGxx124G will be dropped because 
#### there is no "name" code in the file due to a coding error.

####
#### Visualize the eyetracking timecourse data (Figure 3)

#### Trim the database to be used in the visualization (Figure 3)
#### Because children responded freely, some trials were much longer than others
#### We truncated the time widow visualized to 10000ms (but analysis is over a longer window, see below)  
Etlooking_data3 <- subset_by_window(data = Etlooking_data2,
                            window_start_time = -4000,
                            window_end_time = 11000,# this covers 75% and 93% of the trials from the low high vocabulary groups respectively
                            rezero = F,
                            remove = T)

#### Correlation of max trial length and vocabulary 
#### summarize max trial length data
Etlooking_data3_max <- Etlooking_data3 %>% 
  dplyr::group_by(ID,Utrial,OCDI_2_10) %>% 
  summarise(MaxTimestamp= max(Timestamp))%>% 
  ungroup()

ggscatter(Etlooking_data3_max, x = "OCDI_2_10", y = "MaxTimestamp",
          add = "reg.line", conf.int = F,
          cor.coef = TRUE, cor.method = "pearson") +
  labs(x = 'Number of Nouns Produced', 
       y = 'Trial Duration') # R = -0.36 p < 2.2e-16

#### Create sequence data for the figure (Figure3A)
looking_data_part_seq <- make_time_sequence_data(data = Etlooking_data3,
                               time_bin_size = 100,
                               aois = c('Look_Shape', 'Look_Material', 'Look_Up'),
                               predictor_columns = c('Final_Selection','Vgrp')) 

#### reorder the levels for the plot
looking_data_part_seq$Vgrp <- factor(looking_data_part_seq$Vgrp, levels=c("Low", "High"))

#### Convert codes for selections to fill names for the plot
looking_data_part_seq <- looking_data_part_seq %>% 
  mutate(Final_Selection = recode(Final_Selection, s = 'Shape Final Selection', m = 'Material Final Selection')) %>% 
  mutate(Look_to = recode(AOI, Look_Material = 'Material', Look_Shape = 'Shape',Look_Up = 'Up')) 

####
#### Figure 3, gaze trajectories by final selections, split by vocabulary groups for visualisation.
looking_data_part_seq %>%
  mutate(Time1 = Time / 1000) %>% 
  ggplot(
    aes(x = Time1, y = Prop, fill = Look_to)) +
  stat_summary(geom = 'ribbon', fun.data = 'mean_se', alpha = 0.8) +
  geom_vline(xintercept = 0, colour = 'black', linetype = 1) +  
  # stat_smooth() +
  labs(x = 'Time (s)',
       y = 'Proportion Looks to Object/Up',
       fill = " ") +
  papaja::theme_apa(base_size = 16)+
scale_x_continuous(breaks = c(-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9, 1))+   
  theme(axis.text.x = element_text(angle = 45,  hjust = 1))+
  scale_y_continuous(limits = c(0,1))+ 
  facet_wrap(~Vgrp+Final_Selection)+
  scale_color_manual(values=c("blue3", "red3","green4")) +
annotate("rect", xmin = 0, xmax = 0.3, ymin = -Inf, ymax = +Inf,
           alpha = .9,fill = "grey")

#### Note that data beyond the 10000ms window is removed resulting in a warning message when the graph is produced.

#### Get counts of the number of shape and material final selections
Etlooking_data3_counts <- Etlooking_data3 %>% 
  group_by(ID, Age, Gender,  Age_days,`OCDI_2_10`, Utrial,Vgrp) %>% 
  summarise(Final_Selection1=last(Final_Selection)) %>% 
  ungroup() # 1 trial less than the choice as we remove the 124G ,n_k_4 in the warning

ETlooking_data3_counts_sum <- Etlooking_data3_counts %>% 
  dplyr::group_by(Vgrp) %>% 
  summarise(Sh_choice_count=sum(Final_Selection1=='s'),
            Ma_choice_count=sum(Final_Selection1=='m')) %>% 
  ungroup()

####
#### Analyze the Time course Data 

#### First step is to create the database for analysis because in contrast to the 
#### visualization, which was truncated to 10000 ms, the dataset analyzed included
#### the entire presentation and test timecourse for all participants.
#### So we start with the same database as before but the sections before the trial proper started need to be removed.

#### Trim data to just include the presentation (p) and test (t) portions of the trial
Looking_ModelData <- Etlooking_data2 %>%
  filter(TrialSection.type != 'f')

#### longer window for the whole dataset -- longer than the 10k used in the visualization
etdata3_model_whole <- subset_by_window(data = Looking_ModelData,
                                        window_start_time = -4000,
                                        window_end_time = 89760, #max value in dataset 
                                        rezero = F,
                                        remove = T)

#### Convert the timestamps to numeric values for analysis
etdata3_model_whole$Timestamp<- as.numeric(etdata3_model_whole$Timestamp)

#### Create eyetracking data that removes looks up as we will make proportion 
#### data based on just looks to the two test objects
dsLookingdata_whole<- make_eyetrackingr_data(data = etdata3_model_whole,
                                       participant_column = 'ID',
                                       trial_column = 'Utrial',
                                       trackloss_column = 'Look_Off',
                                       time_column = 'Timestamp',
                                       aoi_columns = c('Look_Shape', 'Look_Material'),
                                       treat_non_aoi_looks_as_missing = T)

#### Create a variable to specify the portion of the trial before and After the naming event 
#### Also remove the 300ms after onset of the Name from the analysis window
dsLookingdata_whole$window <- ifelse(dsLookingdata_whole$Timestamp %in%  -4000:0, "Before",
                             ifelse(dsLookingdata_whole$Timestamp %in% 300:89760, "After",0))

#### Filter out  anything not during before/after window 
dsLookingdata_whole <- dsLookingdata_whole %>%
  filter(window != 0)

#### Calculate the proportion shape looking data
window_comb_graph_Final<-make_time_window_data(data = dsLookingdata_whole,
                                               aois = c('Look_Shape'),
                                               predictor_columns = c('Vgrp','window','Final_Selection',"OCDI_2_10"),
                                               summarize_by = c('ID'))

####  set up factors
window_comb_graph_Final$window <- factor(window_comb_graph_Final$window, levels=c("Before", "After"))

#### Convert codes for final selections to full names 
window_comb_graph_Final <- window_comb_graph_Final %>% 
  mutate(Final_Selection = recode(Final_Selection, s = 'Shape', m = 'Material'))

####
#### Analysis of proportion looking to shape BEFORE and AFTER the naming event

#### Create the Before and After datasets
Before_LookingMdata <- window_comb_graph_Final %>% 
  filter( window=="Before")

After_LookingMdata <- window_comb_graph_Final %>% 
  filter( window=="After")

#### remove one NaN datapoint in the before dataset caused by no looking on a trial
Before_LookingMdata <- Before_LookingMdata %>% 
  filter(!is.nan(Prop))

#### check data to determine model type
hist(Before_LookingMdata$Prop) # roughly normal
hist(After_LookingMdata$Prop) # not at all normal
#### will use a betabinomial due to non-normality of the after data (but see supplemental materials for additional supporting models)

####
#### Analysis of the BEFORE data

#### Center and scale the factors
Before_LookingMdata$OCDI_2_10_sc  <- scale(Before_LookingMdata$OCDI_2_10 , center=TRUE, scale=TRUE)
Before_LookingMdata$Final_Selection_s <-
  ifelse(Before_LookingMdata$Final_Selection == 'Shape', 0.5, -0.5)

proplookmodel_before <- glmmTMB(cbind(SamplesInAOI, SamplesTotal - SamplesInAOI) ~ Final_Selection_s*OCDI_2_10_sc+
                                  (1|ID),
                                family = betabinomial,
                                data = Before_LookingMdata) 

summary(proplookmodel_before)
DHARMa::simulateResiduals(proplookmodel_before, plot = T) # be aware you might be prompted to install patchwork 
performance::check_model(proplookmodel_before)
performance::posterior_predictive_check(proplookmodel_before) # blowup of Posterior Predictives Check
library(faraway)
faraway::halfnorm(resid(proplookmodel_before))

####
# Family: betabinomial  ( logit )
# Formula:          cbind(SamplesInAOI, SamplesTotal - SamplesInAOI) ~ Final_Selection_s *      OCDI_2_10_sc + (1 | ID)
# Data: Before_LookingMdata
# 
# AIC      BIC   logLik deviance df.resid 
# 1086.1   1102.1   -537.0   1074.1      100 
# 
# Random effects:
#   
#   Conditional model:
#   Groups Name        Variance Std.Dev.
# ID        (Intercept) 0.04186  0.2046  
# Number of obs: 106, groups:  ID, 55
# 
# Dispersion parameter for betabinomial family (): 4.31 
# 
# Conditional model:
#                                 Estimate Std. Error z value Pr(>|z|)
# (Intercept)                     0.07899    0.09119   0.866    0.386
# Final_Selection_s              -0.24596    0.17389  -1.414    0.157
# OCDI_2_10_sc                    0.03851    0.09114   0.423    0.673
# Final_Selection_s:OCDI_2_10_sc -0.01446    0.17362  -0.083    0.934

####
#### Visualization

#### Add predicted values to dataset for plot
Before_LookingMdata$predict <- predict(proplookmodel_before, type = 'response', re.form = NA)

#### Figure 4A: Before scatterplot
Before_LookingMdata %>% 
  ggplot(aes(x = OCDI_2_10, Prop, colour = Final_Selection, fill = Final_Selection)) +
  labs(x = 'Productive Noun Vocabulary', y = 'Proportion Looking to Shape-Matching Test Object')+
  geom_point() +
  stat_smooth(method = 'lm')+
  stat_summary(aes(y = predict), geom = 'line', fun = 'mean', size = 1, linetype = 2) +
  labs(fill = "Final Selection")+
  labs(colour = "Final Selection")+  
  scale_color_manual(values=c("firebrick","navy"))+ 
  scale_fill_manual(values=c("firebrick","navy"),name = "Final Selection")

####
#### Analysis of the After data 

#### Center and scale the factors
After_LookingMdata$OCDI_2_10_sc  <- scale(After_LookingMdata$OCDI_2_10 , center=TRUE, scale=TRUE)
After_LookingMdata$Final_Selection_s <-
  ifelse(After_LookingMdata$Final_Selection == 'Shape', 0.5, -0.5)

proplookmodel_after <- glmmTMB(cbind(SamplesInAOI, SamplesTotal - SamplesInAOI) ~ Final_Selection_s*OCDI_2_10_sc+
                                 (1|ID),
                               family = betabinomial,
                               data = After_LookingMdata) 

summary(proplookmodel_after)
DHARMa::simulateResiduals(proplookmodel_after, plot = T) 
performance::check_model(proplookmodel_after)
performance::posterior_predictive_check(proplookmodel_after)
faraway::halfnorm(resid(proplookmodel_after))
####
# Family: betabinomial  ( logit )
# Formula:          cbind(SamplesInAOI, SamplesTotal - SamplesInAOI) ~ Final_Selection_s *      OCDI_2_10_sc + (1 | ID)
# Data: After_LookingMdata
# 
# AIC      BIC   logLik deviance df.resid 
# 1201.8   1217.8   -594.9   1189.8      101 
# 
# Random effects:
#   
#   Conditional model:
#   Groups Name        Variance  Std.Dev. 
#   ID     (Intercept) 1.529e-08 0.0001236
# Number of obs: 107, groups:  ID, 55
# 
# Dispersion parameter for betabinomial family (): 4.61 
# 
# Conditional model:
#                               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                    -0.02367    0.09266  -0.255   0.7984    
# Final_Selection_s               2.32719    0.19358  12.022   <2e-16 ***
# OCDI_2_10_sc                   -0.04454    0.09288  -0.480   0.6315    
# Final_Selection_s:OCDI_2_10_sc  0.45971    0.18583   2.474   0.0134 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

####
#### Visualization
#### Add predicted values to dataset for plot
After_LookingMdata$predict <- predict(proplookmodel_after, type = 'response', re.form = NA)

#### Figure 4A: After scatterplot

After_LookingMdata %>% 
  ggplot(aes(x = OCDI_2_10, Prop, colour = Final_Selection, fill = Final_Selection)) +
  labs(x = 'Productive Noun Vocabulary', y = 'Proportion Looking to Shape-Matching Test Object')+
  geom_point() +
  stat_smooth(method = 'lm')+
  stat_summary(aes(y = predict), geom = 'line', fun = 'mean', size = 1, linetype = 2) +
  labs(fill = "Final Selection")+
  labs(colour = "Final Selection")+  
  scale_color_manual(values=c("firebrick","navy"))+ 
  scale_fill_manual(values=c("firebrick","navy"),name = "Final Selection")

################### Follow-up models on shape and material choices separately
#### Create the Shape and Material selection datasets
After_LookingMdata_S <- After_LookingMdata %>% 
  filter( Final_Selection=="Shape")

After_LookingMdata_M <- After_LookingMdata %>% 
  filter( Final_Selection=="Material")

#### Model on shape choice data
proplookmodel_after_S <- glmmTMB(cbind(SamplesInAOI, SamplesTotal - SamplesInAOI) ~ OCDI_2_10_sc+
                                 (1|ID),
                               family = betabinomial,
                               data = After_LookingMdata_S) 

summary(proplookmodel_after_S)
DHARMa::simulateResiduals(proplookmodel_after_S, plot = T) 
performance::check_model(proplookmodel_after_S)
performance::posterior_predictive_check(proplookmodel_after_S)
faraway::halfnorm(resid(proplookmodel_after_S))
####
# Family: betabinomial  ( logit )
# Formula:          cbind(SamplesInAOI, SamplesTotal - SamplesInAOI) ~ OCDI_2_10_sc +      (1 | ID)
# Data: After_LookingMdata_S
# 
# AIC      BIC   logLik deviance df.resid 
# 640.3    648.3   -316.2    632.3       51 
# 
# Random effects:
#   
#   Conditional model:
#   Groups Name        Variance Std.Dev.
# ID     (Intercept) 0.2216   0.4707  
# Number of obs: 55, groups:  ID, 55
# 
# Dispersion parameter for betabinomial family (): 11.4 
# 
# Conditional model:
#                  Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)     1.293      0.122  10.597   <2e-16 ***
#   OCDI_2_10_sc    0.232      0.114   2.036   0.0418 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#### Model on Material choice data
proplookmodel_after_M <- glmmTMB(cbind(SamplesInAOI, SamplesTotal - SamplesInAOI) ~ OCDI_2_10_sc+
                                   (1|ID),
                                 family = betabinomial,
                                 data = After_LookingMdata_M) 

summary(proplookmodel_after_M)
DHARMa::simulateResiduals(proplookmodel_after_M, plot = T) 
performance::check_model(proplookmodel_after_M)
performance::posterior_predictive_check(proplookmodel_after_M)
faraway::halfnorm(resid(proplookmodel_after_M))
#### 
# Family: betabinomial  ( logit )
# Formula:          cbind(SamplesInAOI, SamplesTotal - SamplesInAOI) ~ OCDI_2_10_sc +      (1 | ID)
# Data: After_LookingMdata_M
# 
# AIC      BIC   logLik deviance df.resid 
# 551.5    559.3   -271.7    543.5       48 
# 
# Random effects:
#   
#   Conditional model:
#   Groups Name        Variance Std.Dev.
# ID     (Intercept) 0.5309   0.7286  
# Number of obs: 52, groups:  ID, 52
# 
# Dispersion parameter for betabinomial family ():  5.2 
# 
# Conditional model:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -1.2008     0.1766  -6.798 1.06e-11 ***
# OCDI_2_10_sc  -0.2137     0.1700  -1.257    0.209    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

####
#### Model predicting selection from looking
#### Calculate the proportion shape looking data in dataset that includes trial-level data
triallevel_looking<-make_time_window_data(data = dsLookingdata_whole,
                                               aois = c('Look_Shape'),
                                               predictor_columns = c('Vgrp','window','Final_Selection',"OCDI_2_10", "Utrial"),
                                               summarize_by = c('ID'))

triallevel_looking <-(subset(triallevel_looking, select= c("ID","Vgrp","window", "Final_Selection","OCDI_2_10","Utrial","Prop")))

#### Convert codes for final selections to full names 
triallevel_looking <- triallevel_looking %>% 
  mutate(Final_Selection = recode(Final_Selection, s = 'Shape', m = 'Material'))

####  set up factors
triallevel_looking$window <- factor(triallevel_looking$window, levels=c("Before", "After"))
triallevel_looking$Final_Selection <- factor(triallevel_looking$Final_Selection, levels=c("Material", "Shape"))

#### Center and scale the factors
triallevel_looking$OCDI_2_10_sc  <- scale(triallevel_looking$OCDI_2_10 , center=TRUE, scale=TRUE)

#### Make before and afterdatsets
triallevel_looking_A <- triallevel_looking %>% 
  filter( window=="After")

triallevel_looking_B <- triallevel_looking %>% 
  filter( window=="Before")

#### Model predicting final selection by proportion looking to shape after naming event
selectionmodel_A <- glmmTMB(Final_Selection ~ Prop*OCDI_2_10_sc+
                                   (1|ID),
                                 family = binomial,
                                 data = triallevel_looking_A) 

summary(selectionmodel_A)
DHARMa::simulateResiduals(selectionmodel_A, plot = T) 
performance::check_model(selectionmodel_A) # won't run on this model
performance::posterior_predictive_check(selectionmodelA) # won't run on this model (see below)
faraway::halfnorm(resid(selectionmodel_A))

#### See if random effect is needed
selectionmodel_A2 <- glmmTMB(Final_Selection ~ Prop*OCDI_2_10_sc,
                          family = binomial,
                          data = triallevel_looking_A) 

summary(selectionmodel_A2)
DHARMa::simulateResiduals(selectionmodel_A2, plot = T) 
###Comparing the two models 
anova(selectionmodel_A2, selectionmodel_A) # model results nearly identical, sticking with random effect as in other looking models
#### Final After selection model
# Family: binomial  ( logit )
# Formula:          Final_Selection ~ Prop * OCDI_2_10_sc + (1 | ID)
# Data: triallevel_looking_A
# 
# AIC      BIC   logLik deviance df.resid 
# 571.5    594.7   -280.8    561.5      749 
# 
# Random effects:
#  
#   Conditional model:
#   Groups Name        Variance Std.Dev.
# ID     (Intercept) 0.1835   0.4284  
# Number of obs: 754, groups:  ID, 55
# 
# Conditional model:
#                       Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)        -1.9445     0.2087  -9.319   <2e-16 ***
#   Prop                4.9630     0.3422  14.504   <2e-16 ***
#   OCDI_2_10_sc       -0.0805     0.2097  -0.384    0.701    
#   Prop:OCDI_2_10_sc   0.4513     0.3295   1.370    0.171   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#### Same model on the before data
selectionmodel_B <- glmmTMB(Final_Selection ~ Prop*OCDI_2_10_sc+
                              (1|ID),
                            family = binomial,
                            data = triallevel_looking_B) 

summary(selectionmodel_B)
DHARMa::simulateResiduals(selectionmodel_B, plot = T)
#### selection model on before data
# Family: binomial  ( logit )
# Formula:          Final_Selection ~ Prop * OCDI_2_10_sc + (1 | ID)
# Data: triallevel_looking_B
# 
# AIC      BIC   logLik deviance df.resid 
# 881.2    904.0   -435.6    871.2      706 
# 
# Random effects:
#   
# Conditional model:
#   Groups Name        Variance Std.Dev.
# ID     (Intercept) 0.4174   0.646   
# Number of obs: 711, groups:  ID, 55
# 
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)        0.80175    0.15993   5.013 5.36e-07 ***
#   Prop              -0.02097    0.20368  -0.103    0.918    
#   OCDI_2_10_sc       0.11319    0.15816   0.716    0.474    
#   Prop:OCDI_2_10_sc  0.33175    0.20369   1.629    0.103    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

####
#### visualization

#### Filter NaN trials from datasets to allow prediction data to be incorporated, and join datasets
triallevel_looking_A <- drop_na(triallevel_looking_A)
triallevel_looking_B <- drop_na(triallevel_looking_B)

triallevel_looking_A$predict <- predict(selectionmodel_A, type = 'response', re.form = NA) 
triallevel_looking_B$predict <- predict(selectionmodel_B, type = 'response', re.form = NA) 

triallevel_looking_all <- bind_rows(triallevel_looking_A, triallevel_looking_B)

#### Create selection variable for plot
triallevel_looking_all <- triallevel_looking_all %>% 
  mutate(Selection = ifelse(Final_Selection == 'Shape', 1, 0))

####
#### Figure 4B 
triallevel_looking_all %>% 
  ggplot(aes(x = Prop, y = Selection, fill=window, colour=window)) + 
  stat_summary_bin(geom = 'pointrange', fun.data = 'mean_se', alpha = 0.4, bins=10) +
  stat_summary_bin(aes(y = predict), geom = 'line', fun = 'mean', size = 2) +
  scale_color_manual(values=c("cyan4", "orange1")) +
  labs(x = 'Proportion Looking to Shape-Matching Test Object',
       y = 'Proportion Shape Selections',) +
  papaja::theme_apa(base_size = 24)





# Looking Transition Analysis ---------------------------------------------

#### Create a transitions dataset and change numeral codes for looks into names in a column called "Looking.together"
TransitionData <- ENNG_data %>%  
  group_by(ID, Utrial) %>% 
  mutate(Looking.together = ifelse(Look_Material == 1,  'Material',
                                   ifelse(Look_Shape == 1, 'Shape',
                                          ifelse(Look_Off == 1, 'Off', 
                                                 ifelse(Look_Up == 1, 'Up', NA))))) %>% 
  ungroup()

#### Use names in Looking.together to code the looking transitions           
TransitionData <-TransitionData %>% 
  group_by(ID, Utrial) %>% 
  dplyr::mutate(SwitchObject =
           ifelse(lag(Looking.together) == 'Material' & Looking.together == 'Shape', 'MS',
                               ifelse(lag(Looking.together) == 'Material' & Looking.together == 'Up', 'MU',
                                      ifelse(lag(Looking.together) == 'Material' & Looking.together == 'Off', 'MO',
                                             ifelse(lag(Looking.together) == 'Material' & Looking.together == 'Material', 'MM',
                                                    ifelse(lag(Looking.together) == 'Shape' & Looking.together == 'Shape', 'SS',
                                                           ifelse(lag(Looking.together) == 'Shape' & Looking.together == 'Up', 'SU',
                                                                  ifelse(lag(Looking.together) == 'Shape' & Looking.together == 'Off', 'SO', 
                                                                         ifelse(lag(Looking.together) == 'Shape' & Looking.together == 'Material', 'SM',
                                                                                ifelse(lag(Looking.together) == 'Up' & Looking.together == 'Shape', 'US',
                                                                                       ifelse(lag(Looking.together) == 'Up' & Looking.together == 'Up', 'UU', 
                                                                                              ifelse(lag(Looking.together) == 'Up' & Looking.together == 'Off', 'UO', 
                                                                                                     ifelse(lag(Looking.together) == 'Up' & Looking.together == 'Material', 'UM',
                                                                                                            ifelse(lag(Looking.together) == 'Off' & Looking.together == 'Shape','OS',
                                                                                                                   ifelse(lag(Looking.together) == 'Off' & Looking.together == 'Up','OU',
                                                                                                                          ifelse(lag(Looking.together) == 'Off' & Looking.together == 'Off', 'OO',
                                                                                                                                 ifelse(lag(Looking.together) == 'Off' & Looking.together == 'Material', 'OM', NA))))))))))))))))) %>% 
  ungroup()

#### Create eyetracking data for transitions 
et_TransitionData <- make_eyetrackingr_data(data = TransitionData,
                                  participant_column = 'ID',
                                  trial_column = 'Utrial',
                                  trackloss_column = 'Look_Off',
                                  time_column = 'Timestamp',
                                  aoi_columns = c('Look_Shape', 'Look_Material', 'Look_Up'),
                                  treat_non_aoi_looks_as_missing = F)

#### trim data to just the portion after the naming event, and mark time starting at the naming event 
et_TransitionData_trimmed <- subset_by_window(et_TransitionData ,
                             window_start_msg = 'NAME',
                             msg_col = 'Name_Start',
                             rezero = T,
                             remove = F)
#### Note that a trial for participant 17ENNGxx124G will be dropped because 
#### there is no "name" code in the file due to a coding error.

#### Trim the time window to be after the 300ms delay for an eye movement (to match other eyetracking analysis)
#### and set the end to the max trial length
et_TransitionData_trimmed <- subset_by_window(data = et_TransitionData_trimmed,
                                              window_start_time = 300,
                                              window_end_time = 89760, 
                                              rezero = F,
                                              remove = T)

#### Remove rows that are not transitions between objects
et_TransitionData_trimmed <- et_TransitionData_trimmed %>%
  filter(!SwitchObject == "MM", 
         !SwitchObject == "SS",
         !SwitchObject == "UU",
         !SwitchObject == "OO") 

#### Create a dataset of trials that start with looking up 
TransitionsUp<- et_TransitionData_trimmed %>% 
  group_by(ID, Utrial,OCDI_2_10,Vgrp,Final_Selection) %>% 
  mutate(class= first(SwitchObject)) %>% 
  fill(class) %>% 
  ungroup() %>% 
  filter(substr(class,1,1)=="U")

#### Create a count of the number of switches per trial
TransitionsData_Up_counts <- TransitionsUp %>% 
  group_by(ID, Utrial,OCDI_2_10, Vgrp,Final_Selection) %>% 
  summarise(Trans_count = n()) %>% 
  ungroup()

#### Create a column to label these values as from trials starting up
TransitionsData_Up_counts$Start<- "Up"
# total number of rows in this dataset is 614 and is the total number of trials starting up
# because there is one row per participant (if that trial started up)
# so this value used as denominator in calculation of not up trials (count below) 

#### Calculate mean number of switches per participant
TransitionsData_Up_means  <- TransitionsData_Up_counts  %>% 
  group_by(ID,OCDI_2_10, Vgrp,Start,Final_Selection) %>%
  summarise(Mean_trans = mean(Trans_count)) %>%
  ungroup()

####Create a dataset of trials that do not start with looking up 
TransitionsNotUp<- et_TransitionData_trimmed %>% 
  group_by(ID, Utrial,OCDI_2_10,Vgrp,Final_Selection) %>% 
  mutate(class= first(SwitchObject)) %>% 
  fill(class) %>% 
  ungroup() %>% 
  filter(substr(class,1,1)!="U")

#### Create a count of the number of switches per trial
TransitionData_NotUp_counts <- TransitionsNotUp %>% 
  group_by(ID, Utrial, OCDI_2_10, Vgrp,Final_Selection) %>% 
  summarise(Trans_count = n()) %>% 
  ungroup()
# total rows in this dataset is 122 and is the number of trials not starting up
# used to calculate proportion of trials starting up. (= 614/(614+122) = .834 )


#### Create a column to label these values as from trials NOT starting up
TransitionData_NotUp_counts$Start<- "NotUp"

#### Calculate mean number of switches per participant
TransitionsData_NotUp_means <- TransitionData_NotUp_counts  %>% 
  group_by(ID,OCDI_2_10, Vgrp,Start,Final_Selection) %>%
  summarise(Mean_trans = mean(Trans_count)) %>%
  ungroup()

#### Combine Mean Start from up and not start from up datasets together 
Full_MeanTransitions<- bind_rows(TransitionsData_Up_means,TransitionsData_NotUp_means)

#### Check normality of the data then scale and center the factors and 
hist(Full_MeanTransitions$Mean_trans)
#### Outlier in histogram from a participant 18ENNGxx125G who had only 1 NotUp trial and 33 transitions on that one trial. 
#### Re-create the dataset without that outlier before analysis

TransitionData_NotUp_counts_NoOutlier <- TransitionData_NotUp_counts %>%
  filter(ID != "18ENNGxx125G")

#### Re-calculate mean number of switches per participant
TransitionsData_NotUp_NoOutlier_means <- TransitionData_NotUp_counts_NoOutlier  %>% 
  group_by(ID,OCDI_2_10, Vgrp,Start,Final_Selection) %>%
  summarise(Mean_trans = mean(Trans_count)) %>%
  ungroup()

Full_MeanTransitions_NoOutlier<- bind_rows(TransitionsData_Up_means,TransitionsData_NotUp_NoOutlier_means)

hist(Full_MeanTransitions_NoOutlier$Mean_trans)

Full_MeanTransitions_NoOutlier$OCDI_2_10_sc  <- scale(Full_MeanTransitions_NoOutlier$OCDI_2_10, center=TRUE, scale=TRUE)

#### Model predicting of mean number of transitions by vocabulary (continuous), start up/elsewhere and final selection
Transitions_Model <- glmmTMB(Mean_trans ~ OCDI_2_10_sc*Start*Final_Selection +
                                          (1|ID),
                                          family = Gamma(link = 'log'),
                                          data=Full_MeanTransitions_NoOutlier)

summary(Transitions_Model) 
DHARMa::simulateResiduals(Transitions_Model, plot = T)
###Family: Gamma  ( log )
# Formula:          Mean_trans ~ OCDI_2_10_sc * Start * Final_Selection + (1 | ID)
# Data: Full_MeanTransitions_NoOutlier
# 
# AIC      BIC   logLik deviance df.resid 
# 702.0    733.1   -341.0    682.0      156 
# 
# Random effects:
#   
#   Conditional model:
#   Groups Name        Variance Std.Dev.
# ID     (Intercept) 0.1628   0.4035  
# Number of obs: 166, groups:  ID, 55
# 
# Dispersion estimate for Gamma family (sigma^2): 0.287 
# 
# Conditional model:
#                                         Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                            1.22226    0.12940   9.445  < 2e-16 ***
#   OCDI_2_10_sc                          -0.40652    0.14434  -2.816  0.00486 ** 
#   StartUp                                0.09272    0.13920   0.666  0.50536    
#   Final_Selections                      -0.01825    0.15613  -0.117  0.90695    
#   OCDI_2_10_sc:StartUp                   0.17914    0.15417   1.162  0.24522    
#   OCDI_2_10_sc:Final_Selections          0.31472    0.16019   1.965  0.04945 *  
#   StartUp:Final_Selections              -0.19600    0.18855  -1.040  0.29857    
#   OCDI_2_10_sc:StartUp:Final_Selections -0.30693    0.19244  -1.595  0.11073    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#### Dropping start location
#### Model predicting of mean number of transitions by vocabulary (continuous) and Final selection ONLY
Transitions_Model_2 <- glmmTMB(Mean_trans ~ OCDI_2_10_sc*Final_Selection +
                                       (1|ID),
                                       family = Gamma(link = 'log'),
                                       data=Full_MeanTransitions_NoOutlier)

summary(Transitions_Model_2) 
DHARMa::simulateResiduals(Transitions_Model_2, plot = T)

# Family: Gamma  ( log )
# Formula:          Mean_trans ~ OCDI_2_10_sc * Final_Selection + (1 | ID)
# Data: Full_MeanTransitions_NoOutlier
# 
# AIC      BIC   logLik deviance df.resid 
# 697.4    716.0   -342.7    685.4      160 
# 
# Random effects:
#   
#   Conditional model:
#   Groups Name        Variance Std.Dev.
#   ID     (Intercept) 0.1603   0.4003  
#   Number of obs: 166, groups:  ID, 55
# 
# Dispersion estimate for Gamma family (sigma^2): 0.295 
# 
# Conditional model:
#                                  Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                    1.29899    0.08563  15.171  < 2e-16 ***
#   OCDI_2_10_sc                  -0.26725    0.08759  -3.051  0.00228 ** 
#   Final_Selections              -0.16476    0.09028  -1.825  0.06799 .  
#   OCDI_2_10_sc:Final_Selections  0.09858    0.09200   1.072  0.28391    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

###Comparing the two models 
anova(Transitions_Model_2, Transitions_Model)
#### Models not significantly different

#### Dropping Final Selection 
#### Model predicting of mean number of transitions by vocabulary (continuous)
Transitions_Model_3 <- glmmTMB(Mean_trans ~ OCDI_2_10_sc+ (1|ID),
                                            family = Gamma(link = 'log'),
                                            data=Full_MeanTransitions_NoOutlier)

summary(Transitions_Model_3) 
DHARMa::simulateResiduals(Transitions_Model_3, plot = T)
####
# Family: Gamma  ( log )
# Formula:          Mean_trans ~ OCDI_2_10_sc + (1 | ID)
# Data: Full_MeanTransitions_NoOutlier
# 
# AIC      BIC   logLik deviance df.resid 
# 697.9    710.4   -345.0    689.9      162 
# 
# Random effects:
#   
#   Conditional model:
#   Groups Name        Variance Std.Dev.
# ID     (Intercept) 0.1564   0.3954  
# Number of obs: 166, groups:  ID, 55
# 
# Dispersion estimate for Gamma family (sigma^2): 0.306 
# 
# Conditional model:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   1.21562    0.06978  17.420  < 2e-16 ***
# OCDI_2_10_sc -0.21669    0.06994  -3.098  0.00195 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

###Comparing the two models 
anova(Transitions_Model_3, Transitions_Model_2)
#### models not different so going with the the simpler model, DHARMa looks better too. 

#### Figure 5a: Scatterplot of mean transitions by vocabulary
Full_MeanTransitions_NoOutlier %>% 
  ggplot(aes(x = OCDI_2_10, y=Mean_trans)) +
  labs(x = 'Productive Noun Vocabulary', y = 'Mean Number of Transitions')+       
  geom_point() +
  stat_smooth(method = 'lm') +
  papaja::theme_apa(base_size = 24) 



# Reaction Time Analysis  -------------------------------------------------
        
#### Starting from TransitionsUp dataset because looking at how long to go from exeplar
#### to the shape or material test object by. Note that TransitionUp starts 300ms after 
#### name onset so this dataset only includes transitions after that point (to match the 
#### other eyetracking analyses). Be sure you've run the transitions analysis thru line 
#### 812 before starting here. 

ReactionTimedata <- TransitionsUp %>%
  dplyr::group_by(ID, Utrial,OCDI_2_10,Vgrp,Final_Selection) %>%
  filter(row_number()==1) %>%
  ungroup()

#### Remove one trial that is a look from the exemplar to off 
ReactionTimedata <- ReactionTimedata %>% 
  dplyr::group_by(ID, Utrial,OCDI_2_10,Vgrp,Final_Selection) %>% 
  filter(SwitchObject%in%c("UM","US")) %>% 
  ungroup() 

####
#### Collapse across trials to get the mean reaction time for each participant for
#### trials ending with a shape or material selection
ReactionTimedata_Mean_part <- ReactionTimedata %>% 
  group_by(ID, OCDI_2_10,Vgrp,SwitchObject,Final_Selection) %>% 
  summarise(MeanRT = mean(Timestamp)) %>%
  ungroup()

#### Scale and center the factors
ReactionTimedata_Mean_part$OCDI_2_10_sc  <- scale(ReactionTimedata_Mean_part$OCDI_2_10 , center=TRUE, scale=TRUE)
ReactionTimedata_Mean_part$SwitchObject_s <-
  ifelse(ReactionTimedata_Mean_part$SwitchObject == 'US' , 0.5, -0.5)

#### divide time by 1000 to convert to seconds (more managable for model and plot)
ReactionTimedata_Mean_part <- ReactionTimedata_Mean_part %>% 
  mutate(MeanRTs = MeanRT / 1000)

#### sanity check of normality
hist(ReactionTimedata_Mean_part$MeanRTs) 
library(lmerTest)

#### Reaction Time model
RT_logmodel <- lmer(MeanRTs ~ OCDI_2_10_sc*SwitchObject_s*Final_Selection +
                                     (1|ID),
                                   data = ReactionTimedata_Mean_part)
summary(RT_logmodel)
####
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: MeanRTs ~ OCDI_2_10_sc * SwitchObject_s * Final_Selection + (1 |      ID)
#    Data: ReactionTimedata_Mean_part
# 
# REML criterion at convergence: 618.1
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.9562 -0.3681 -0.1654  0.1652  6.1291 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  ID       (Intercept) 1.553    1.246   
#  Residual             1.125    1.061   
# Number of obs: 177, groups:  ID, 55
# 
# Fixed effects:
#                                              Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                                    1.88112    0.21344  84.09168   8.813  1.4e-13 ***
# OCDI_2_10_sc                                  -0.56968    0.21418  81.22250  -2.660  0.00942 ** 
# SwitchObject_s                                 0.25375    0.26377 122.33910   0.962  0.33794    
# Final_Selections                               0.18254    0.16992 120.24789   1.074  0.28486    
# OCDI_2_10_sc:SwitchObject_s                   -0.10537    0.25682 120.61535  -0.410  0.68233    
# OCDI_2_10_sc:Final_Selections                 -0.07607    0.16985 120.86164  -0.448  0.65506    
# SwitchObject_s:Final_Selections               -0.25297    0.34061 120.56840  -0.743  0.45912    
# OCDI_2_10_sc:SwitchObject_s:Final_Selections   0.28976    0.33716 119.68936   0.859  0.39183    
# ---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#### Dropping NS switch object effect 
RT_logmodel2 <- lmer(MeanRTs ~ OCDI_2_10_sc*Final_Selection +
                      (1|ID),
                    data = ReactionTimedata_Mean_part)
summary(RT_logmodel2)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: MeanRTs ~ OCDI_2_10_sc * Final_Selection + (1 | ID)
#    Data: ReactionTimedata_Mean_part
# 
# REML criterion at convergence: 615.8
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.9681 -0.3782 -0.1407  0.1544  6.2362 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  ID       (Intercept) 1.538    1.240   
#  Residual             1.111    1.054   
# Number of obs: 177, groups:  ID, 55
# 
# Fixed effects:
#                                Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                     1.86918    0.20962  81.12006   8.917 1.15e-13 ***
# OCDI_2_10_sc                   -0.59105    0.21133  79.44700  -2.797  0.00647 ** 
# Final_Selections                0.19034    0.16513 123.58581   1.153  0.25129    
# OCDI_2_10_sc:Final_Selections  -0.04469    0.16610 124.32437  -0.269  0.78835    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
###Comparing the two models 
anova(RT_logmodel, RT_logmodel2)
#### models not different, drop NS switch object

RT_logmodel3 <- lmer(MeanRTs ~ OCDI_2_10_sc+
                      (1|ID),
                    data = ReactionTimedata_Mean_part)
summary(RT_logmodel3)
###Comparing the two models 
anova(RT_logmodel3, RT_logmodel2)
#### models not different 

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: MeanRTs ~ OCDI_2_10_sc + (1 | ID)
#    Data: ReactionTimedata_Mean_part
# 
# REML criterion at convergence: 613.7
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.9214 -0.3778 -0.1429  0.1563  6.3187 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  ID       (Intercept) 1.554    1.247   
#  Residual             1.102    1.050   
# Number of obs: 177, groups:  ID, 55
# 
# Fixed effects:
#              Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)    1.9782     0.1866 52.5254  10.603 1.17e-14 ***
# OCDI_2_10_sc  -0.6083     0.1886 52.0260  -3.225  0.00218 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#### Figure 5b: Scatterplot of mean RT by vocabulary

ReactionTimedata_Mean_part %>% 
  ggplot(aes(x = OCDI_2_10, y=MeanRTs)) +
  labs(x = 'Productive Noun Vocabulary', y = 'Mean Reaction Time (s)')+       
  geom_point() +
  stat_smooth(method = 'lm') +
  papaja::theme_apa(base_size = 24) 






# Attention During Familiarization     ------------------------------------

#### Create touching data and reassign NAs to 0 from the touch columns 
touch_data <- Data %>%
  mutate(Touch_Shape = ifelse(is.na(Touch_Shape), 0, Touch_Shape),
         Touch_Material = ifelse(is.na(Touch_Material), 0, Touch_Material),
         Touch_Exemplar = ifelse(is.na(Touch_Exemplar), 0, Touch_Exemplar))

#### Trim data to just be from familiarization period
touch_data <- touch_data %>%
  filter(TrialSection.type == 'f')

#### Add Final selection to the dataframe (after clearing it)
touch_data$Final_Selection <- NULL

#### Drop no response trials (because we want to see how exploration during familiarization 
#### might be related to generalization choices)
touch_data <- left_join(ChoiceByTrial,touch_data,by=c("ID","Utrial","OCDI_2_10","Age","Gender","Vgrp","Age_days")) 

#### Create a dataset with the total time spent touching each object for each participant
#### First create separate datasets for each object
touch_dataMaterial <- touch_data %>% 
  dplyr::group_by(ID, Gender, OCDI_2_10, Age_days, Utrial,Vgrp, Final_Selection) %>% 
  filter(Touch_Material == 1) %>%
  summarise(TotalTouchMaterial= ((max(Timestamp)-min(Timestamp))))%>% 
  ungroup()

touch_dataShape <- touch_data %>% 
  dplyr::group_by(ID, Gender, OCDI_2_10, Age_days,Utrial, Vgrp,Final_Selection) %>% 
  filter(Touch_Shape == 1) %>% 
  summarise(TotalTouchShape= ((max(Timestamp)-min(Timestamp))))%>% 
  ungroup()

touch_dataExemplar <- touch_data %>% 
  dplyr::group_by(ID, Gender, OCDI_2_10, Age_days, Utrial,Vgrp,Final_Selection) %>% 
  filter(Touch_Exemplar == 1) %>% 
  summarise(TotalExemplar= ((max(Timestamp)-min(Timestamp))))%>% 
  ungroup()

#### join the columns together in one dataset
touch_dataTotals <- left_join(touch_dataMaterial, touch_dataShape , by = c("ID","Vgrp","Gender","OCDI_2_10","Age_days","Final_Selection","Utrial"), keep = FALSE, na.rn=T) %>%
  left_join(., touch_dataExemplar , by=c("ID","Vgrp","Gender", "OCDI_2_10","Age_days","Final_Selection","Utrial"),na.rn=T) 
# some NAs are produced because some participants did not touch an object at a particular trial

#### change NAs to 0 
touch_dataTotals[is.na(touch_dataTotals)] <- 0

#### Create a dataset with the total familiarization time for each participant
touch_dataTotalfam <- touch_data %>% 
  dplyr::group_by(ID, Gender, OCDI_2_10, Age_days, Vgrp,Final_Selection,Utrial) %>% 
  summarise(TotalFamtime= ((max(Timestamp)-min(Timestamp))))%>% 
  ungroup()

#### convert to seconds
touch_dataTotalfam$TotalFam_sec <- touch_dataTotalfam$TotalFamtime/1000

#### Create a dataset with the mean total familiarization time for each participant
touch_dataTotalfam_means <- touch_dataTotalfam %>% 
  dplyr::group_by(ID, Gender, OCDI_2_10, Age_days, Vgrp,Final_Selection) %>% 
  summarise(MeanFamtime= mean(TotalFam_sec))%>% 
  ungroup()

#### Mean, min, max for total touch time 
mean(touch_dataTotalfam_means$MeanFamtime) #29.20547
min(touch_dataTotalfam_means$MeanFamtime) #12.1575
max(touch_dataTotalfam_means$MeanFamtime) #75.34843

#### Correlation with noun vocabulary
ggscatter(touch_dataTotalfam_means, x = "OCDI_2_10", y = "MeanFamtime",
          add = "reg.line", conf.int = F,
          cor.coef = TRUE, cor.method = "pearson") +
  scale_y_continuous(limits = c(0,80))+ 
  labs(x = 'Number of Nouns Produced', 
       y = 'Mean Total Familiarization Time')+ 
  geom_hline(yintercept = 0.5, colour = 'black', linetype = 2) +
  papaja::theme_apa(base_size = 22)
####  R = 0.056, p = 0.57

#### Correlation with age
ggscatter(touch_dataTotalfam_means, x = "Age_days", y = "MeanFamtime",
          add = "reg.line", conf.int = F,
          cor.coef = TRUE, cor.method = "pearson") +
  scale_y_continuous(limits = c(0,80))+ 
  labs(x = 'Age (in days)', 
       y = 'Mean Total Familiarization Time')+ 
  geom_hline(yintercept = 0.5, colour = 'black', linetype = 2) +
  papaja::theme_apa(base_size = 22)
####  R = 0.11, p = 0.26

#### join total familiarization time to the dataset with total times for each object
touch_dataTotals <- left_join(touch_dataTotals, touch_dataTotalfam , by=c("ID","Vgrp","Gender", "OCDI_2_10","Age_days","Final_Selection","Utrial"), keep = FALSE, na.rn=T)

#### Create a dataset with proportion touch data 
touch_propdata <- touch_dataTotals %>% 
  dplyr::group_by(ID,Gender, OCDI_2_10, Age_days, Vgrp,Final_Selection,Utrial,TotalExemplar,TotalTouchShape,TotalTouchMaterial,TotalFamtime) %>%
  summarise(Shape= (TotalTouchShape/TotalFamtime),
            Material= (TotalTouchMaterial/TotalFamtime),
            Exemplar= (TotalExemplar/TotalFamtime))%>%
  ungroup()

#### trim off the columns with total touching time
touch_propdata$TotalExemplar <- NULL
touch_propdata$TotalTouchMaterial <- NULL
touch_propdata$TotalTouchShape <- NULL
touch_propdata$TotalFamtime <- NULL

#### Create a dataset with mean proportion touch data collapsing across trials 
touch_propdata <- touch_propdata %>% 
  dplyr::group_by(ID,Gender, OCDI_2_10, Age_days, Vgrp,Final_Selection) %>%
  summarise(Shape_prop=mean(Shape),
            Material_prop=mean(Material),
            Exemplar_prop=mean(Exemplar))%>%
  ungroup()

#### covert the dataset to long format  
touch_propdata_Long <- gather(touch_propdata, object, prop_touch, Shape_prop, Material_prop, Exemplar_prop)

####  
#### linear mixed-effects model predicting the proportion of time spent touching 
#### each object out of total familiarization time by vocabulary (continuous), Final
#### Selectionand object

TouchModel<- lme4::lmer(prop_touch ~  OCDI_2_10*object*Final_Selection+(1|ID), 
                        data=touch_propdata_Long,REML=F)

car::Anova(TouchModel,type=3) ## only marginal effect of final selection
DHARMa::simulateResiduals(TouchModel, plot = T)
#### 
# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: prop_touch
# Chisq Df Pr(>Chisq)    
# (Intercept)                      138.0620  1    < 2e-16 ***
#   OCDI_2_10                        0.3283  1    0.56665    
# object                             4.7177  2    0.09453 .  
# Final_Selection                    0.6739  1    0.41170    
# OCDI_2_10:object                   3.8322  2    0.14718    
# OCDI_2_10:Final_Selection          0.5217  1    0.47011    
# object:Final_Selection             0.0277  2    0.98623    
# OCDI_2_10:object:Final_Selection   0.4072  2    0.81579    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#### Dropping ns Final selection
TouchModel_2<- lme4::lmer(prop_touch ~  OCDI_2_10*object+(1|ID), 
                          data=touch_propdata_Long,REML=F)

car::Anova(TouchModel_2,type=3)
DHARMa::simulateResiduals(TouchModel_2, plot = T)
# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: prop_touch
# Chisq Df Pr(>Chisq)    
#   (Intercept)      189.9490  1    < 2e-16 ***
#   OCDI_2_10          0.0369  1    0.84764    
#   object             8.8784  2    0.01181 *  
#   OCDI_2_10:object   4.4700  2    0.10699    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

###Comparing the two models 
anova(TouchModel_2,TouchModel)
#### Anova shows no difference and model without Final selection has lower AIC. 

#### Dropping ns Vocabulary
TouchModel_3<- lme4::lmer(prop_touch ~  object+(1|ID), 
                          data=touch_propdata_Long,REML=F)

car::Anova(TouchModel_3,type=3)
DHARMa::simulateResiduals(TouchModel_3, plot = T)
# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: prop_touch
#             Chisq Df Pr(>Chisq)    
# (Intercept) 633.029  1  < 2.2e-16 ***
# object       39.934  2   2.13e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
###Comparing the two models 
anova(TouchModel_3,TouchModel_2)

#### Convert names for plot
touch_propdata_Long <- touch_propdata_Long %>% 
  mutate(object = recode(object, Exemplar_prop = 'Exemplar', Shape_prop = 'Shape', Material_prop = 'Material'))

#### Figure 6
touch_propdata_Long %>%
  ggplot(aes(y = prop_touch, x = object, fill=object)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(width = .2, position = position_dodge(width = .9)) +
  stat_summary(aes (colour=object),
               fun.data = "mean_se",
               geom = "pointrange",
               position = position_dodge(width = 0.9))+
  labs(x = "Object", y = "Proportion Familiarization Time")+
  papaja::theme_apa(base_size = 24) 

