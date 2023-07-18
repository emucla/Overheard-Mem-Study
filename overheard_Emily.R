#set working directory 

setwd("/Users/emilyneer/Desktop")
#install packages
library(tidyverse)
library(ggplot2)
library(mice)
library(dplyr)
library(svglite)
library(jtools)

#read in the data 
ovdat <- read.table('video_coding_data.txt', header=T)
ovdat$response <- as.numeric(as.character(ovdat$response))

#copy over data imputation 
imp <- mice(ovdat, method = "pmm", m = 20, maxit = 1)
ovdat <- complete(imp)
head(ovdat)

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

tar <- tapply(ovdat2$prop, ovdat2$sub_id, mean)

response <- tapply(ovdat2$response, ovdat2$sub_id, mean)

df2 <- ovdat2 %>% group_by(sub_id, cond) %>% 
  summarise(attn=mean(prop), score = mean(response), 
            .groups = 'drop') %>%
  as.data.frame()
df2$sub_id <- as.character(as.numeric(df2$sub_id))

#Question 1: Correlation (target attention and correct response) 

#run correlation test
cor.test(df2$attn, df2$score, method = "pearson")

#plot scatterplot with trendline - below is example code
df2 %>% 
  ggplot(aes(x= attn, y= score)) +
  geom_point(size = 3)+
  coord_fixed(ratio =2.5/4)  +
  scale_x_continuous(limits=c(0, 1))+
  scale_y_continuous (limits=c(0, 1))+
  labs(x = "Attention Duration (proportion)", y = "Proportion of Correct Responses")+
  geom_smooth(method=lm,se=FALSE,fullrange=TRUE,
              )+
  theme_classic()+
  theme(text = element_text(face = "bold", size = 16, vjust = 0.5)) +
  theme(axis.title.y = element_text(vjust = 5)) +
  theme(axis.title.x = element_text(hjust = 0.5, vjust = -.70)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=.5,face = "bold", size = 16, 
                                   color = "black")) 

#Question 2: T-test (massed and spaced attention) - run t-test for unequal sample sizes 

#correct responses mean for each condition (descriptive)
m_res <- tapply(df2$score, df2$cond, mean)

#mean of attention to target for each condition 
m_attn <- tapply(df2$attn, df2$cond, mean)

#code for t-test comparing the means of the two conditions

#running t test 
t.test(df2$attn ~ df2$cond, var.equal = FALSE)

#plot massed and spaced comparison boxplot (overlay indv. data points) - below is example code

df2 %>% 
  ggplot(aes(x= cond, y= attn)) +
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6, position = "jitter")+ 
  stat_summary(fun.data=mean_se, fun.args = list(mult = 1),
               geom="pointrange", color="black",
               shape = 18, size = 1,
               position = position_dodge(width = 0.9))+
  #geom_point(aes(fill=cond), colour = "white",pch=21, position=position_dodge(width=0.9)) +
  coord_fixed(ratio =4/3)  +
  ylim(0, 1) +
  labs(x = "Learning Schedule", y = "Attention to Target (Proportion)")+
  theme_classic()+
  theme(text = element_text(face = "bold", size = 12)) +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=.5,face = "bold", size = 12, 
                                   color = "black"))

#Question 3: regression analysis (target attention and correct response) - currently does not give a complete output because of fake dataset values
#link to simple linear regression in R: http://www.sthda.com/english/articles/40-regression-analysis/167-simple-linear-regression-in-r/
regress <- lm(formula = score ~ attn, data = df2)
summary(regress)


#exploratory analyses 
#attention between target and distractor (t-test)
#looking more at distractor are they more likely to pick the distractor object? 
#age effects on attention 

