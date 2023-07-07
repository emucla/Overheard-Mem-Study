#set working directory 

setwd("~/Desktop/")

#install packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(svglite)
library(jtools)

#read in the data 
ovdat <- read.table('ovdat.txt', header=T)

#create function for standard error
se <- function(x){
  sd(x,na.rm = TRUE)/sqrt(length(x[!is.na(x)]))
}

#create a proportion of tar_duration/trial time 
prop <- ovdat$tar_dur/ovdat$trial_time 

#adding prop value to dataframe: use this dataframe for the analyses 
ovdat2 <- ovdat %>%
  add_column(prop)

#descriptive data 

#mean age of children and sd 


#Question 1: Correlation (target attention and correct response) - example code below
#create data frame for correlation plot
df <- data.frame(
  mentalstate = cog/totalMSL,
  perspective = persp
)
#run correlation test
correlation <- cor.test(cog/totalMSL, persp, method = "pearson")

#plot scatterplot with trendline - below is example code
ovdat %>% 
  ggplot(aes(x= Age, y= Correct, color = Condition)) +
  geom_point(size = 3)+
  coord_fixed(ratio =1/7)  +
  #ylim(0, 8)+
  #xlim(1.75, 3)+
  scale_x_continuous(breaks=seq(1.75, 3, 0.25))+
  scale_y_continuous (limits=c(0, 8))+
  labs(x = "Age (years)", y = "Correct Responses at Test (Out of 8)")+
  geom_smooth(method=lm,se=FALSE,fullrange=TRUE,
              aes(color=Condition))+
  theme_classic()+
  theme(text = element_text(face = "bold", size = 16, vjust = 0.5)) +
  theme(axis.title.y = element_text(vjust = 5)) +
  theme(axis.title.x = element_text(hjust = 0.5, vjust = -.70)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=.5,face = "bold", size = 16, 
                                   color = "black")) 

#Question 2: T-test (massed and spaced attention)

#correct responses mean for each condition (descriptive)
#mean of attention to target for each condition 

#code for t-test comparing the means of the two conditions

#creating values that we can put in our t test 
mass1 <- tapply(mass$Correct, mass$Subjectid, mean)
space1 <- tapply(space$Correct, space$Subjectid, mean)

#running t test 
t.test(mass1, space1, var.equal = TRUE)

#plot massed and spaced comparison boxplot (overlay indv. data points) - below is example code

ovdat %>% 
  ggplot(aes(x= Condition, y= Correct, color=Condition)) +
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6)+ 
  stat_summary(fun.data=mean_se, fun.args = list(mult = 1),
               geom="pointrange", color="black",
               shape = 18, size = 2,
               position = position_dodge(width = 0.9))+
  #geom_point(aes(fill=cond), colour = "white",pch=21, position=position_dodge(width=0.9)) +
  coord_fixed(ratio =1/3)  +
  ylim(0, 8) +
  labs(x = "Learning Schedule", y = "Correct Responses at Test (Out of 8)")+
  theme_classic()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(legend.position = "none")+
  geom_hline(yintercept =2, colour = "black", linetype = 2)+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5,face = "bold", size = 12, 
                                   color = "black"))

#Question 3: regression analysis (target attention and correct response) - currently does not give a complete output because of fake dataset values
#link to simple linear regression in R: http://www.sthda.com/english/articles/40-regression-analysis/167-simple-linear-regression-in-r/
regress <- lm(formula = response ~ prop, data = ovdat2)
summary(regress)


#exploratory analyses 
#attention between target and distractor (t-test)
#looking more at distractor are they more likely to pick the distractor object? 
#age effects on attention 

