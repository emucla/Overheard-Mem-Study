#set working directory 

setwd("C:/Users/wijayas/Documents/Berea College/Internship 2023/Overhearing Study/1_Data Analysis")
#install packages
library(tidyverse)
library(ggplot2)
library(mice)
library(dplyr)
library(svglite)
library(jtools)

#read in the data 
ovdat <- read.table('video_coding_data.txt', header=T)

#check data frame
summary(ovdat)

#change response variable to numeric
ovdat$response <- as.numeric(as.character(ovdat$response))

#data imputation code (https://stefvanbuuren.name/fimd/sec-pmm.html)
imp <- mice(ovdat, method = "pmm", m = 20, maxit = 1)
ovdat <- complete(imp)

#create function for standard error
se <- function(x){
  sd(x,na.rm = TRUE)/sqrt(length(x[!is.na(x)]))
}

#create a proportion of tar_duration/trial time 
#prop <- ovdat$tar_dur/ovdat$trial_time 

#adding prop value to dataframe 
#ovdat2 <- ovdat %>%
#  add_column(prop)

#try to create subset
med <- ovdat[ovdat$trial == 1, ]
medianvalue<- median(med$tar_dur)

highmed <- med[med$tar_dur >= medianvalue,]
lowmed <- med[med$tar_dur < medianvalue,]

#t-test
med1 <- read.table('med2.txt', header=T)

summary(med1)

#running t test for high vs low
t.test(med1$score ~ med1$med, var.equal = FALSE)

#descriptive data of attention to target object averaged by subject 
tar <- tapply(ovdat$tar_dur, ovdat$sub_id, mean)

#descriptive data of children's reponses in task averaged by subject
response <- tapply(ovdat$response, ovdat$sub_id, mean)

#new data frame grouping by subid - use this data frame for analyses 
df2 <- ovdat %>% group_by(sub_id, cond) %>% 
  summarise(attn=mean(tar_dur), score = sum(response), 
            .groups = 'drop') %>%
  as.data.frame()
#telling R to recognize sub_id as a character so that you can group by it
df2$sub_id <- as.character(as.numeric(df2$sub_id))

#Preliminary analysis:  independent samples t-tests with unequal variances because of different sample sizes
#Are there differences between children's correct responses based on learning schedule? 

#correct responses mean for each condition (descriptive)
m_res <- tapply(df2$score, df2$cond, mean)

t.test(df2$score ~ df2$cond, var.equal = FALSE)

#Are there differences between attention in the massed and spaced condition?

#mean of attention to target for each condition 
m_attn <- tapply(df2$attn, df2$cond, mean)

#running t test for attention 
t.test(df2$attn ~ df2$cond, var.equal = FALSE)

#plot massed and spaced comparison boxplot (overlay indv. data points) 
df2 %>% 
  ggplot(aes(x= cond, y= attn, color = cond)) +
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6, position = "jitter")+ 
  #this chunk of code inputs mean and se bar in boxplot
  stat_summary(fun.data=mean_se, fun.args = list(mult = 1),
               geom="pointrange", color="black",
               shape = 18, size = 1,
               position = position_dodge(width = 0.9))+
  #changes the scale of your plot
  coord_fixed(ratio =1/12)  +
  #y-axis scale 
  ylim(0, 30) +
  labs(x = "Learning Schedule", y = "Attention to Target (seconds)")+
  theme_classic()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5,face = "bold", size = 12, 
                                   color = "black"))



#Question: Does attention to target predict (or relate) to children’s correct responses on task regardless of learning schedule? 

#run correlation test
cor.test(df2$attn, df2$score, alternative = "greater", method = "pearson")

#regression test if we are going to claim directionality 
regress <- lm(formula = score ~ attn, data = df2) #runs the regression
summary(regress) #spits out statistics 

#plot scatterplot with trendline
df2 %>% 
  ggplot(aes(x= attn, y= score)) +
  geom_point(size = 3)+
  coord_fixed(ratio =6/2)  +
  scale_x_continuous(limits=c(0, 30))+
  scale_y_continuous (limits=c(0, 8))+
  #chance line
  geom_hline(yintercept=2,linetype="dotted")+
  labs(x = "Attention Duration (seconds)", y = "Correct Responses (Out of 8 Trials)")+
  geom_smooth(method=lm,se=FALSE,fullrange=TRUE,
              )+
  theme_classic()+
  theme(text = element_text(face = "bold", size = 16, vjust = 0.5)) +
  theme(axis.title.y = element_text(vjust = 5)) +
  theme(axis.title.x = element_text(hjust = 0.5, vjust = -.70)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=.5,face = "bold", size = 16, 
                                   color = "black")) 

#Exploratory Analyses 
#attention between target and distractor (t-test)
#looking more at distractor are they more likely to pick the distractor object? 
#create column where the distractor choice is coded as 1 and everything else is 0 
 

