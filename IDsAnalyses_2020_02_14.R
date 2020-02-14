library(ggplot2)
library(dplyr)
library(psych)
library(lme4)
library(tidyr)
library(rcompanion)
library(corrplot)
library(PerformanceAnalytics)
library(Hmisc)

setwd("")

#--------------------------------------------#
#                 LOAD DATA                  #
#--------------------------------------------#

data <- read.csv(file= "IDsAnswers_2019_11_20.csv", header = TRUE, sep=",", stringsAsFactors=FALSE) %>%
  dplyr::select(-c(X))
head(data)

#----------------------------------------------------------------------
#         Add proportion of alignment per item and participant
#----------------------------------------------------------------------

AlignByPart <- data %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarise(AlignByPart = mean(as.numeric(as.character(aligncode)), na.rm = TRUE)*100, 
            AlignByPartFreq = sum(as.numeric(as.character(aligncode)), na.rm = T))

AlignProportions <- AlignByPart %>%
  dplyr::summarise(mean = mean(AlignByPart), 
                   sd = sd(AlignByPart))

AlignByItem <- data %>% 
  dplyr::group_by(item) %>%
  dplyr::summarise(AlignByItem = mean(as.numeric(as.character(aligncode)), na.rm = TRUE)*100)

#-----------------------------
#      Merge & transform
#-----------------------------

data2 <- merge(data, AlignByPart, by.x="ID", by.y="ID")

data2$AlignNorm <- car::logit(data2$AlignByPartFreq/28, adjust = 1/56)
hist(data2$AlignNorm)
hist(data2$AlignByPartFreq)
plotNormalHistogram(data2$AlignNorm)

data2$AgeNorm <- abs(data2$age)^(1/3)
hist(data2$age)
hist(data2$AgeNorm)
plotNormalHistogram(data2$AgeNorm)
cor.test(data2$AlignNorm, data2$AgeNorm) #sign negative interaction

par(mfrow=c(2,2))
plotNormalHistogram(car::logit(data2$PositiveSymptoms/26, adjust = 1/52))
plotNormalHistogram(abs(data2$PositiveSymptoms)^(1/3))
plotNormalHistogram(sqrt(data2$PositiveSymptoms))
plotNormalHistogram(log10(data2$PositiveSymptoms+1))
par(mfrow=c(1,1))
data2$PosNorm <- sqrt(data2$PositiveSymptoms+1)
cor.test(data2$AlignNorm, data2$PosNorm)

par(mfrow=c(2,2))
plotNormalHistogram(car::logit(data2$NegativeSymptoms/25, adjust = 1/50))
plotNormalHistogram(abs(data2$NegativeSymptoms)^(1/3))
plotNormalHistogram(sqrt(data2$NegativeSymptoms))
plotNormalHistogram(log10(data2$NegativeSymptoms+1))
par(mfrow=c(1,1))
data2$NegNorm <- sqrt(data2$NegativeSymptoms)
cor.test(data2$AlignNorm, data2$NegNorm) #super small coeff but sign?

par(mfrow=c(2,2))
plotNormalHistogram(car::logit(data2$ThoughtDisorder/26, adjust = 1/52))
plotNormalHistogram(abs(data2$ThoughtDisorder)^(1/3))
plotNormalHistogram(sqrt(data2$ThoughtDisorder))
plotNormalHistogram(log10(data2$ThoughtDisorder+1))
par(mfrow=c(1,1))
data2$DisNorm <- sqrt(data2$ThoughtDisorder)
cor.test(data2$AlignNorm, data2$DisNorm)

#----------------------------------------------#
#             ALIGNMENT EFFECT                 #
#----------------------------------------------#

freq1.table <- data %>% 
  dplyr::distinct(item, .keep_all = TRUE) %>% 
  dplyr::select(item, freq1) 

frequencies <- merge(AlignByItem, freq1.table, by.x="item", by.y="item")

wilcox.test(as.numeric(frequencies$freq1), as.numeric(frequencies$AlignByItem), paired=TRUE, exact = FALSE) #significant 

item <- as.character(frequencies$item)
freq1 <- as.numeric(as.character(frequencies$freq1))
alignitem <- as.numeric(as.character(frequencies$AlignByItem))
task <- c(rep("spontaneous", 28), rep("primed", 28))

df <- data.frame(c(rep(item, 2)), 
                 c(freq1, alignitem), 
                 task)
colnames(df) <- c("item", "freq1", "task")
summary(df)


alignplot <- ggplot(df, aes(x=item, y=freq1, group=task, color=task, linetype=task)) +
  geom_line(size=1) +
  geom_point() +
  theme(legend.position="right") + 
  labs(x = "Targets", y = "Percentage of use of disfavoured label") + 
  theme_bw() +
  theme(
    title = element_text(color="black", size=14, face="bold"),
    legend.position="right", 
    axis.title.x = element_text(color="black", size=10, face="bold"), 
    axis.title.y = element_text(color="black", size=10, face="bold"), 
    legend.title = element_text(color="black", size=10, face="bold"), 
    axis.text.x = element_blank(), 
    axis.text.y = element_text(color="black", size=10), 
    legend.text = element_text(color="black", size=10), 
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"))+
  expand_limits(y=c(0,75)) +
  scale_colour_manual(values=c("red3", "black"), 
                      name  ="Task",
                      breaks=c("primed", "spontaneous"),
                      labels=c("matching-and-naming", "spontaneous naming")) +
  scale_linetype_manual(values=c("solid", "dashed"), 
                        name  ="Task",
                        breaks=c("primed", "spontaneous"),
                        labels=c("matching-and-naming", "spontaneous naming"))
alignplot


#--------------------------

gender_a <- data %>% 
  group_by(gender) %>%
  summarise(align = mean(as.numeric(as.character(aligncode)), na.rm = TRUE))

age_a <- data %>% 
  group_by(age) %>%
  summarise(align = mean(as.numeric(as.character(aligncode)), na.rm= TRUE)*100, 
            N = n() /28)

summary(age_a$align)
sd(age_a$align)

ggplot(data = age_a, aes(x = age, y = align)) + 
  geom_point(position =  "jitter", size=1) +
  geom_smooth(method = "lm", se = T, col="red") +
  labs(x = "Age", 
       y = "Percentage of use of disfavoured label")+
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    title = element_text(color="black", size=14, face="bold"),
    axis.title.x = element_text(color="black", size=10, face="bold"), 
    axis.title.y = element_text(color="black", size=10, face="bold"), 
    axis.text.x = element_text(color="black", size=10), 
    axis.text.y = element_text(color="black", size=10)) +
  scale_x_continuous(breaks = c(26, 30, 35, 40, 45, 50, 55, 60, 65)) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                     limits = c(0,100)) 

AgePlot <- ggplot(data = data2, aes(x = age, y = AlignByPartFreq*100/28)) + 
  geom_point(size=1.5) +
  geom_smooth(method = "lm", se = T, col="red") +
  labs(x = "Age", 
       y = "Frequency of use of disfavoured label", 
       title = "A")+
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    title = element_text(color="black", size=14, face="bold"),
    axis.title.x = element_text(color="black", size=10, face="bold"), 
    axis.title.y = element_text(color="black", size=10, face="bold"), 
    axis.text.x = element_text(color="black", size=10), 
    axis.text.y = element_text(color="black", size=10)) +
  scale_x_continuous(breaks = c(26, 30, 35, 40, 45, 50, 55, 60, 65)) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                     limits = c(0,100)) 
AgePlot

DisPlot <- ggplot(data = data2, aes(x = ThoughtDisorder, y = AlignByPartFreq*100/28)) + 
  geom_point(size=1.5) +
  geom_smooth(method = "lm", se = T, col="red") +
  labs(title = "B", 
       x = "Disorganised Schizotypy", 
       y = "Frequency of use of disfavoured label")+
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    title = element_text(color="black", size=14, face="bold"),
    axis.title.x = element_text(color="black", size=10, face="bold"), 
    axis.title.y = element_text(color="black", size=10, face="bold"), 
    axis.text.x = element_text(color="black", size=10), 
    axis.text.y = element_text(color="black", size=10)) +
  scale_x_continuous(breaks = c(2,4,6,8,10,12,14,16,18,20,22,24,26)) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                     limits = c(0,100)) 

PosPlot <- ggplot(data = data2, aes(x = PositiveSymptoms, y = AlignByPartFreq*100/28)) + 
  geom_point(size=1.5) +
  geom_smooth(method = "lm", se = T, col="red") +
  labs(title = "C", 
       x = "Positive Symptoms", 
       y = "Frequency of use of disfavoured label")+
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    title = element_text(color="black", size=14, face="bold"),
    axis.title.x = element_text(color="black", size=10, face="bold"), 
    axis.title.y = element_text(color="black", size=10, face="bold"), 
    axis.text.x = element_text(color="black", size=10), 
    axis.text.y = element_text(color="black", size=10)) +
  scale_x_continuous(breaks = c(2,4,6,8,10,12,14,16,18,20,22,24,26)) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                     limits = c(0,100)) 

NegPlot <- ggplot(data = data2, aes(x = NegativeSymptoms, y = AlignByPartFreq*100/28)) + 
  geom_point(size=1.5) +
  geom_smooth(method = "lm", se = T, col="red") +
  labs(title = "D", 
       x = "Negative Symptoms", 
       y = "Frequency of use of disfavoured label")+
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    title = element_text(color="black", size=14, face="bold"),
    axis.title.x = element_text(color="black", size=10, face="bold"), 
    axis.title.y = element_text(color="black", size=10, face="bold"), 
    axis.text.x = element_text(color="black", size=10), 
    axis.text.y = element_text(color="black", size=10)) +
  scale_x_continuous(breaks = c(2,4,6,8,10,12,14,16,18,20,22,24,26)) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                     limits = c(0,100)) 

source("http://peterhaschke.com/Code/multiplot.R")
multiplot(AgePlot,DisPlot,PosPlot,NegPlot,cols=2)

#----------------------------------------------


data2 %>% select(AgeNorm, DisNorm, NegNorm, PosNorm) %>% cor() -> M
M
corrplot(M, method="number")

chart.Correlation(data2 %>% 
                    distinct(ID, AgeNorm, DisNorm,NegNorm, PosNorm) %>% 
                    select(-ID),
                  histogram=TRUE, pch=10)

mydata.rcorr = rcorr(as.matrix(data2 %>% 
                                 distinct(ID, AgeNorm, DisNorm,NegNorm, PosNorm) %>% 
                                 select(-ID)))

mydata.rcorr

mydata.coeff = mydata.rcorr$r
mydata.p = mydata.rcorr$P

#-----------------------------------------------
#                   Models                     #
#-----------------------------------------------

summary(data2)
data2$ID <-as.factor(data2$ID)
data2$item <- as.factor(data2$item)


m1.sg1 <- glmer(aligncode ~ AgeNorm + DisNorm + PosNorm + NegNorm + 
              + (1|ID) + (1+AgeNorm + DisNorm + PosNorm + NegNorm|item),
              data=data2, family=binomial, 
              glmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=400000)), na.action = na.omit)
summary(m1.sg1)  

m1.sg2 <- glmer(aligncode ~ AgeNorm + DisNorm + PosNorm + NegNorm + 
                  + (1|ID) + (1+AgeNorm + PosNorm + NegNorm|item), 
                data=data2, family=binomial,
                glmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=400000)), na.action = na.omit)
summary(m1.sg2)  

m1.sg3 <- glmer(aligncode ~ AgeNorm + DisNorm + PosNorm + NegNorm + 
                  + (1|ID) + (1+AgeNorm + PosNorm |item), 
                data=data2, family=binomial,
                glmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=400000)), na.action = na.omit)
summary(m1.sg3)  

m1<- glmer(aligncode ~ AgeNorm + DisNorm + PosNorm + NegNorm + 
                  + (1|ID) + (1+ AgeNorm |item), 
                data=data2, family=binomial,
                glmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=400000)), na.action = na.omit)
summary(m1)  

library(car)
vif(m1)

#------------------------------------------------------------
#         is there an effect of age in the pre-test?        #
#------------------------------------------------------------

pre_test <- read.csv(file= "pre_test.csv", header = TRUE, sep=",")
head(pre_test)

pb <- pre_test %>% gather("items", "word", 4:43)
pb$question <- 1:nrow(pb) 
pb$question <- grepl(pattern = "(1)$", pb$items)

for(i in 1:nrow(pb)){
  if(pb$question[i]=="TRUE"){
    pb$question[i]="1"
  }
  else {
    pb$question[i]="2"
  }
}

library(stringr)
pb$items <- str_sub(pb$items, 1, str_length(pb$items)-1)

summary(pb)
pb$word <- as.factor(pb$word)

pre_test <- glmer(word ~ 1 + (1|PROLIFIC.ID) + (1|items), data=pb, family=binomial)
summary(pre_test)

pre_test_age.sg <- glmer(word ~ log(AGE) + (1|PROLIFIC.ID) + (1+log(AGE)|items), data=pb, family=binomial, glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(pre_test_age.sg)

pre_test_age <- glmer(word ~ log(AGE) + (1|PROLIFIC.ID) + (1|items), data=pb, family=binomial, glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(pre_test_age)

anova(pre_test, pre_test_age)

