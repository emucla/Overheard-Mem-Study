setwd("~/Desktop//LCD Lab/251 Materials/Data Analyses 251/R Analyses")

library(MESS)
library(reshape2)
library(ggplot2)
library(dlookr)
library(CorrToolBox)

#create function for standard error
se <- function(x){
  sd(x,na.rm = TRUE)/sqrt(length(x[!is.na(x)]))
}

# read in the data file
OVdata <- read.table('OVData_massed.txt', header=F)

# add column names
names (OVdata) <- c('subj', 'sex', 'age (months)', 'age', 'age group', 'birth order', 'test' )

#mean by condition
#sum of the responses by subject
OVdata_sum <- tapply(OVdata$test,OVdata$subj, sum, na.rm = TRUE)

#mean and SE of the responses by subject 
mean(OVdata_sum)
mean_mass <-mean(OVdata_sum)
se1 <- se(OVdata_sum)

# read in the data file
OVdata2 <- read.table('OVdata_spaced.txt', header=F)
cor_spaced_re <- subset(OVdata2, subj !=11  & subj != 12 & subj != 16)

# add column names
names (OVdata2) <- c('subj', 'sex', 'age (months)', 'age', 'age group', 'birth order', 'test' )

#mean by condition
#sum of the responses by subject
OVdata2_sum <- tapply(OVdata2$test, OVdata2$subj, sum)

#mean and SE of the responses by subject 
mean(OVdata2_sum)
mean_space <-mean(OVdata2_sum)
se2 <- se(OVdata2_sum)

#barplot
data <- data.frame(mean_mass, mean_space)
#reformat dataframe
plt_data <- melt(data)
#calculate standard error for barplot
plt_data$SE_min[1] <- plt_data$value[1]-se1
plt_data$SE_min[2] <- plt_data$value[2] - se2

plt_data$SE_max[1] <- plt_data$value[1]+se1
plt_data$SE_max[2] <- plt_data$value[2] + se2

#need to: center title, bold labels, remove legend 
ggplot(data = plt_data, aes(x=variable, y= value)) + 
  geom_bar(aes(fill=variable),stat= "identity")+
  #y scale
  scale_y_continuous(breaks = seq.int(0,8),limits=c(0,8))+
  #x labels
  scale_x_discrete(
                   labels=c("Massed", "Spaced"))+
  #labels for axes
  labs(x = "Conditions")+
  labs(y = "Number of Correct Responses (Out of 8)")+
  #chance line
  geom_hline(yintercept=2,linetype="dotted")+
  #error bars
  geom_errorbar(aes(ymin=SE_min,ymax=SE_max),width=.1)+
  theme(legend.position = "none")+
  #use classic theme and square format
  theme_classic()+
  theme(legend.position = "none")+
  theme(aspect=1)
  
#t-test by condition
t.test(OVdata2$test, OVdata$test , alternative = "two.sided", var.equal = FALSE)

#-------------- Code below does not work and needs to be updated --------------####
#two way ANOVA
ov.aov2 <- aov()

#reading in attention data file
attn <- read.table('attn.txt', header=T)

#subsetting above or below two correct
att_below <- attn[attn$Above == 'n',]
att_above <- attn[attn$Above == 'y',]

m_dur_b <- tapply(att_below$Duration, att_below$SubID, mean)
mean(m_dur_b)

m_dur_a <- tapply(att_above$Duration, att_above$SubID, mean)
mean(m_dur_a)

m_freq_b <- tapply(att_below$Freq, att_below$SubID, mean)
mean(m_freq_b)

m_freq_a <- tapply(att_above$Freq, att_above$SubID, mean)
mean(m_freq_a)

t.test(m_dur_b, m_dur_a, var.equal = FALSE)
library(car)

leveneTest(Duration~Condition + Above, data = attn)
aov1 <- aov(Above ~ Condition, data=attn)

prop_dur <- attn$Duration/attn$TrialTime
#means of duration and freuency by subject for massed
att_massed <- attn[attn$Condition == 'Massed',]
att_spaced <- attn[attn$Condition == 'Spaced',]



m_freq <- tapply(att_massed$Freq, att_massed$SubID, mean)
m_dur <- tapply(att_massed$Duration, att_massed$SubID, mean)

s_freq <- tapply(att_spaced$Freq, att_spaced$SubID, mean)
s_dur <- tapply(att_spaced$Duration, att_spaced$SubID, mean)



#biserial correlation between number of correct responses and attention measures
#cor_spaced_re_test <- tapply(cor_spaced_re$test, cor_spaced_re$subj, )
#bs2pbs (bs= 0.6, cor_spaced_re$test, s_dur, p = .05)

#I think we need to make a separate file of test responses or add test responses to attention data file
#then find correlations between test responses and duration, freq, etc. 
#mmass <-tapply(OVdata$test,OVdata$subj, mean, na.rm = TRUE)
#mspace <-tapply(OVdata2$test,OVdata2$subj, mean)
