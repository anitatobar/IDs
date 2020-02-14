getwd()
setwd("")
getwd()

library(dplyr)
library(ggplot2)
library(lmerTest)
library(simr)

# Answers 
CB4 <- read.csv(file= "CB4_cleandata_2020_02_14.csv", 
                       header = TRUE, 
                       sep=",", 
                       stringsAsFactors=TRUE) %>%
  dplyr::select(-c("X"))

Neuro <- read.csv(file="CB4_Neuro.csv", 
                  header = T, 
                  sep = ",", 
                  stringsAsFactors = F)

CB4_merged <- merge(CB4, 
                    Neuro %>% select(neuro.prolificID, total) %>%
                      rename(Prolific = neuro.prolificID, 
                             Neuroticism = total),
                    by.x = "Prolific", 
                    by.y = "Prolific")
 

##############################################
# Plots 
##############################################

# Alignment Effect

AlignByPart <- CB4_merged %>% 
  dplyr::group_by(Prolific) %>% 
  dplyr::summarise(AlignByPart = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)*100)
describe(AlignByPart)

AlignByTarget <- CB4_merged %>% 
  dplyr::group_by(Target) %>% 
  dplyr::summarise(AlignByTarget = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)*100)
AlignByTarget

freq <- CB4_merged %>% 
  dplyr::select (Target, Frequency) %>% 
  dplyr::distinct(Target, .keep_all = TRUE)

align_effect <- merge(AlignByTarget, freq, by.x ="Target", by.y = "Target")

Frequency <- c(align_effect$Frequency, align_effect$AlignByTarget)
Target <- rep(align_effect$Target, 2)
Task <- c(rep("Spontaneous", 15), rep("Primed", 15))

Frequencies <- data.frame(Target, Frequency, Task)
summary(Frequencies)

FreqProportions <- Frequencies %>% 
  dplyr::group_by(Task) %>% 
  dplyr::summarise(mean = mean(as.numeric(as.character(Frequency)), na.rm = TRUE), 
                   sd = sd(Frequency),
                   N = n(),
                   Entrain.se = sd/sqrt(n()))

wilcox.test(as.numeric(align_effect$AlignByTarget), as.numeric(align_effect$Frequency), paired = TRUE, exact = F)
# there is an alignment effect 

# Conditions:

PartnerProportions <- CB4_merged %>%
  dplyr::group_by(ID, Partner) %>%
  dplyr::summarise(Entrain.m = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)) %>%
  dplyr::group_by(Partner) %>%
  dplyr::summarise(Mean = mean(Entrain.m),
                   SD = sd(Entrain.m),
                   N = n(),
                   Entrain.se = SD/sqrt(n()))
PartnerProportions

CyberballProportions <- CB4_merged %>%
  dplyr::group_by(ID, Cyberball) %>%
  dplyr::summarise(Entrain.m = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)) %>%
  dplyr::group_by(Cyberball) %>%
  dplyr::summarise(Mean = mean(Entrain.m),
                   SD = sd(Entrain.m),
                   N = n(),
                   Entrain.se = SD/sqrt(n()))
CyberballProportions

PartnerCyberball = CB4_merged %>%
  dplyr::group_by(Cyberball, Partner, ID) %>%
  dplyr::summarise(align = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)) %>%
  dplyr::group_by(Cyberball, Partner) %>%
  dplyr::summarise(Entrain.m = mean(align)*100,
                   Entrain.sd = sd(align)*100,
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n())) %>%
  dplyr::mutate(Cyberball2 = ifelse(Cyberball %in% c("Inclusion"), "Inclusion", "Ostracism"), 
                Partner2 = ifelse(Partner %in% c("Same"), "Same", "New"))
PartnerCyberball
PartnerCyberball$Cyberball <-as.character(PartnerCyberball$Cyberball)
PartnerCyberball$Partner <-as.character(PartnerCyberball$Partner)
PartnerCyberball[5,] =c("Pretest", "Pretest", 4.8, .6, 15, 1, "Pretest", "Pretest")
PartnerCyberball$Entrain.m <- as.numeric(PartnerCyberball$Entrain.m)
PartnerCyberball$Entrain.se <- as.numeric(PartnerCyberball$Entrain.se)
PartnerCyberball$Cyberball <-as.factor(PartnerCyberball$Cyberball)
PartnerCyberball$Partner <-as.factor(PartnerCyberball$Partner)

plot1 <- ggplot(subset(PartnerCyberball, Cyberball != "Pretest"),
                aes(x = Cyberball, y = as.numeric(as.character(Entrain.m)), fill=Partner))+
  geom_bar(stat = "identity",position = "dodge", width = .5)+
  geom_errorbar(aes(ymax=Entrain.m+Entrain.se,ymin=Entrain.m-Entrain.se),
                position=position_dodge(width=0.5),
                width=0.1)+
  geom_hline(aes(yintercept = subset(PartnerCyberball, Partner == "Pretest")$Entrain.m),linetype=8)+
  scale_linetype_manual(name = "", values = c(2))+
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.title.x = element_text(color = "black", size=24, face = "bold"), 
    axis.title.y = element_text(color="black", size=24, face="bold"), 
    legend.title = element_text(color="black", size=22, face="bold"),
    legend.text = element_text(color="black", size=20),
    axis.text.x = element_text(color="black", size=18), 
    axis.text.y = element_text(color="black", size=18), 
    legend.position=c(0.5, 0.95),
    legend.direction = "horizontal") +
  expand_limits(y=c(0,60)) + 
  scale_fill_manual( values=c("grey", "grey30"), 
                     name="Partner's Identity", 
                     breaks = c("Same", "New"), 
                     labels=c("Same Partner", "New Partner"))+
  scale_x_discrete( name="Cyberball Condition",
                    breaks = c("Inclusion", "Ostracism"), 
                    labels=c("Control", "Exclusion"))+
  labs(y="Use of Disfavoured Labels (%)")
plot1


Sex = CB4_merged %>%
  dplyr::group_by(Sex, ID) %>%
  dplyr::summarise(align = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)) %>%
  dplyr::group_by(Sex) %>%
  dplyr::summarise(Entrain.m = mean(align),
                   Entrain.sd = sd(align),
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n())) 
Sex

Sex_plot <- ggplot(Sex, aes(x=Sex, y=Entrain.m)) + 
  geom_bar(stat="identity", position="dodge", width=.3) +
  geom_errorbar(aes(ymax=Entrain.m+Entrain.se,ymin=Entrain.m-Entrain.se),position=position_dodge(width=0.5),width=0.1)+
  coord_cartesian(ylim=c(0,1))+
  theme_bw() +
  theme(axis.title.x = element_text(color="black", size=12, face = "bold", margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(color="black", size=12, face = "bold"), 
        axis.text = element_text(color="black", size=12), 
        legend.title = element_text(color= "black", size=12, face= "bold"), 
        title = element_text(color="black", size=14, face="bold")) +
  labs(x="Sex", 
       title="Proportion of Entrainment by Sex") +
  scale_y_continuous(name = "Proportion of entrainment") 
Sex_plot


PartnerCyberballSex = CB4_merged %>%
  dplyr::group_by(Cyberball, Partner, Sex, ID) %>%
  dplyr::summarise(align = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)) %>%
  dplyr::group_by(Cyberball, Partner, Sex) %>%
  dplyr::summarise(Entrain.m = mean(align),
                   Entrain.sd = sd(align),
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n())) 
PartnerCyberballSex


CyberballPartnerSex_plot <- ggplot(PartnerCyberballSex, aes(x=Cyberball, y=Entrain.m, fill=Partner)) + 
  geom_bar(stat="identity", position="dodge", width=.3) +
  geom_errorbar(aes(ymax=Entrain.m+Entrain.se,ymin=Entrain.m-Entrain.se),position=position_dodge(width=0.5),width=0.1)+
  coord_cartesian(ylim=c(0,1))+
  scale_fill_manual( values=c("grey", "deeppink3"), name="Partner in \nnaming task")+
  facet_wrap(~Sex) +
  theme_bw() +
  theme(axis.title.x = element_text(color="black", size=12, face = "bold", margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(color="black", size=12, face = "bold"), 
        axis.text = element_text(color="black", size=12), 
        legend.title = element_text(color= "black", size=12, face= "bold"), 
        title = element_text(color="black", size=14, face="bold")) +
  labs(x="Cyberball condition", 
       title="Proportion of Entrainment by Cyberball Condition, \nPartner Condition, and Sex") +
  scale_y_continuous(name = "Proportion of entrainment") 

CyberballPartnerSex_plot

describe(CB4_merged$Neuroticism)
hist(CB4_merged$Neuroticism)

AlignByPart_Ostra <- CB4_merged %>% 
  dplyr::filter(Cyberball == "Ostracism") %>%
  dplyr::group_by(Prolific, Neuroticism) %>% 
  dplyr::summarise(AlignByPart = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)*100)
describe(AlignByPart_Ostra)
cor.test(log(AlignByPart_Ostra$Neuroticism), log(AlignByPart_Ostra$AlignByPart+1))

NeuroPlot_Ostra <- ggplot(data = AlignByPart_Ostra, aes(x = Neuroticism, y = AlignByPart)) + 
  geom_point(position =  "jitter", size=1.5) +
  geom_smooth(method = "lm", se = T, col="red") +
  labs(title = "B", 
       x = "Neuroticism", 
       y = "Use of Disfavoured Labels (%)")+
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    title = element_text(color="black", size=16, face="bold"),
    axis.title.x = element_text(color="black", size=16, face="bold"), 
    axis.title.y = element_text(color="black", size=16, face="bold"), 
    axis.text.x = element_text(color="black", size=16), 
    axis.text.y = element_text(color="black", size=16)) +
  scale_x_continuous(breaks = c(5,10,15,20,25,30,35,40)) +
  scale_y_continuous(breaks = c(0,20,40,60,80,100), 
                     limits = c(0,100)) 
NeuroPlot_Ostra

AlignByPart_Control <- CB4_merged %>% 
  dplyr::filter(Cyberball == "Inclusion") %>%
  dplyr::group_by(Prolific, Neuroticism) %>% 
  dplyr::summarise(AlignByPart = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)*100)
describe(AlignByPart_Control)
cor.test(log(AlignByPart_Control$Neuroticism), log(AlignByPart_Control$AlignByPart))

NeuroPlot_Control <- ggplot(data = AlignByPart_Control, aes(x = Neuroticism, y = AlignByPart)) + 
  geom_point(size=1.5) +
  geom_smooth(method = "lm", se = T, col="red") +
  labs(title = "A", 
       x = "Neuroticism", 
       y = "Use of Disfavoured Labels (%)")+
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    title = element_text(color="black", size=16, face="bold"),
    axis.title.x = element_text(color="black", size=16, face="bold"), 
    axis.title.y = element_text(color="black", size=16, face="bold"), 
    axis.text.x = element_text(color="black", size=16), 
    axis.text.y = element_text(color="black", size=16)) +
  scale_x_continuous(breaks = c(5,10,15,20,25,30,35,40)) +
  scale_y_continuous(breaks = c(0,20,40,60,80,100), 
                     limits = c(0,100)) 
NeuroPlot_Control

source("http://peterhaschke.com/Code/multiplot.R")
multiplot(NeuroPlot_Control,NeuroPlot_Ostra,cols=2)


#----------------------------------------------#
#             VARIABLES 
#----------------------------------------------#

summary(CB4_merged)
CB4_merged <- na.omit(CB4_merged)

#INDEPENDENT VARIABLES:

#1.Partner
CB4_merged$devpartner <- ifelse(CB4_merged$Partner == "Same", -.5, .5)

#2.Cyberball Condition
CB4_merged$devconditions <- ifelse(CB4_merged$Cyberball == "Inclusion", -.5, .5)

#4.Personality
CB4_merged$devNeuroticism <- log(CB4_merged$Neuroticism)
CB4_merged$SNeuroticism <- scale(CB4_merged$devNeuroticism)

#4.Sex
CB4_merged$devSex <- ifelse(CB4_merged$Sex == "Female", -.5, .5)

#Random Effects
CB4_merged$Target <- as.factor(CB4_merged$Target)
CB4_merged$ID <- as.factor(CB4_merged$ID)

summary(CB4_merged)

#---------------------------------------#
#             MODELS
#---------------------------------------#

fullmodel.sgfit <- glmer(as.factor(Aligncode) ~ devconditions*devpartner + devSex +(1+devpartner*devconditions + devSex|Target) + (1|ID), 
                   data = CB4_merged, family=binomial, 
                   na.action = na.omit, 
                   glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel.sgfit) 

fullmodel.sgfit2 <- glmer(as.factor(Aligncode) ~ devconditions*devpartner + devSex +(1+devconditions+devpartner+ devSex|Target) + (1|ID), 
                   data = CB4_merged, family=binomial, 
                   na.action = na.omit, 
                   glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel.sgfit2) 

fullmodel.sgfit3 <- glmer(as.factor(Aligncode) ~ devconditions*devpartner + devSex +(1+devpartner+devSex|Target) + (1|ID), 
                          data = CB4_merged, family=binomial, 
                          na.action = na.omit, 
                          glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel.sgfit3) 

fullmodel.sgfit4 <- glmer(as.factor(Aligncode) ~ devconditions*devpartner + devSex+(1+devconditions+devSex|Target) + (1|ID), 
                          data = CB4_merged, family=binomial, 
                          na.action = na.omit, 
                          glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel.sgfit4) 

fullmodel.sgfit5 <- glmer(as.factor(Aligncode) ~ devconditions*devpartner+ devSex +(1|Target) + (1|ID), 
                   data = CB4_merged, family=binomial, 
                   na.action = na.omit, 
                   glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel.sgfit5) 

fullmodel <- glmer(as.factor(Aligncode) ~ devconditions*devpartner+ devSex+(1|Target) + (1|ID), 
                   data = CB4_merged, family=binomial, 
                   na.action = na.omit, 
                   glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel) 

partnermodel <- glmer(as.factor(Aligncode) ~ devpartner+(1|Target) + (1|ID), 
                   data = CB4_merged, family=binomial, 
                   na.action = na.omit, 
                   glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(partnermodel) 

nullmodel <- glmer(as.factor(Aligncode) ~ 1+(1|Target) + (1|ID), 
                      data = CB4_merged, family=binomial, 
                      na.action = na.omit, 
                      glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(nullmodel) 

BIC(partnermodel) #1675.8
BIC(nullmodel) #1670.8

BF_BIC = exp((BIC(partnermodel) - BIC(nullmodel))/2)  # BICs to Bayes factor
BF_BIC

BF_BIC / (BF_BIC + 1)


fullmodel_Ostra.sg <- glmer(as.factor(Aligncode) ~ devpartner +(1 +devpartner |Target) + (1|ID), 
                   data = subset(CB4_merged, Cyberball=="Ostracism"), family=binomial, 
                   na.action = na.omit, 
                   glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel_Ostra.sg) 

fullmodel_Ostra <- glmer(as.factor(Aligncode) ~ devpartner +(1 |Target) + (1|ID), 
                         data = subset(CB4_merged, Cyberball=="Ostracism"), family=binomial, 
                         na.action = na.omit, 
                         glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel_Ostra) 

fullmodel_Control.sg <- glmer(as.factor(Aligncode) ~ devpartner +(1 +devpartner |Target) + (1|ID), 
                           data = subset(CB4_merged, Cyberball=="Inclusion"), family=binomial, 
                           na.action = na.omit, 
                           glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel_Control.sg) 

fullmodel_Control <- glmer(as.factor(Aligncode) ~ devpartner +(1 |Target) + (1|ID), 
                         data = subset(CB4_merged, Cyberball=="Inclusion"), family=binomial, 
                         na.action = na.omit, 
                         glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel_Control) 

#BF expresses the probability of the data given H1 relative to H0 (i.e., values larger than 1 are in favour of H1)

# Neuroticism in Inclusion Trials

fullmodel_Neuroticism_In.sg1 <- glmer(as.factor(Aligncode) ~ SNeuroticism +(1 +SNeuroticism |Target) + (1|ID), 
                               data = subset(CB4_merged, Cyberball=="Inclusion"), family=binomial, 
                               na.action = na.omit, 
                               glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel_Neuroticism_In.sg1) 

fullmodel_Neuroticism_In <- glmer(as.factor(Aligncode) ~ SNeuroticism + (1 |Target) + (1|ID), 
                               data = subset(CB4_merged, Cyberball=="Inclusion"), family=binomial, 
                               na.action = na.omit, 
                               glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel_Neuroticism_In) 

null_neuro <- glmer(as.factor(Aligncode) ~ 1 +(1|Target) + (1|ID), 
                    data = subset(CB4_merged, Cyberball=="Inclusion"), family=binomial, 
                    na.action = na.omit, 
                    glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(null_neuro) 

BIC(fullmodel_Neuroticism_In) #883.2187
BIC(null_neuro) #876.6573

BF_BIC_neuro = exp((BIC(fullmodel_Neuroticism_In) - BIC(null_neuro))/2)  # BICs to Bayes factor
BF_BIC_neuro

BF_BIC_neuro / (BF_BIC_neuro + 1)

# Neuroticism in Exclusion Trials 

fullmodel_Neuroticism.sg1 <- glmer(as.factor(Aligncode) ~ SNeuroticism +(1 +SNeuroticism |Target) + (1|ID), 
                             data = subset(CB4_merged, Cyberball=="Ostracism"), family=binomial, 
                             na.action = na.omit, 
                             glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel_Neuroticism.sg1) 

fullmodel_Neuroticism <- glmer(as.factor(Aligncode) ~ SNeuroticism +(1 |Target) + (1|ID), 
                              data = subset(CB4_merged, Cyberball=="Ostracism"), family=binomial, 
                              na.action = na.omit, 
                              glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel_Neuroticism) 

full_neuro_Os <- glmer(as.factor(Aligncode) ~ SNeuroticism +(1|Target) + (1|ID), 
                    data = subset(CB4_merged, Cyberball=="Ostracism"), family=binomial, 
                    na.action = na.omit, 
                    glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(full_neuro_Os) 

null_neuro_Os <- glmer(as.factor(Aligncode) ~ 1 +(1|Target) + (1|ID), 
                    data = subset(CB4_merged, Cyberball=="Ostracism"), family=binomial, 
                    na.action = na.omit, 
                    glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(null_neuro_Os) 

BIC(full_neuro_Os) #824.80
BIC(null_neuro_Os) #819.36

BF_BIC_neuro = exp((BIC(full_neuro_Os) - BIC(null_neuro_Os))/2)  # BICs to Bayes factor
BF_BIC_neuro

BF_BIC_neuro / (BF_BIC_neuro + 1)

###############

TargetsOrder <- as.data.frame(matrix(c(
  c("bicycle", "toad", "memory stick", 
    "refrigerator", "mobile", "loo", 
    "rodent", "flower", "bathtub", "pistol", 
    "bunny","brolly", "hen", "nectarine", "spectacles"), 
  c(1:15)), 
  ncol=2))
colnames(TargetsOrder) = c("Target", "Order")
TargetsOrder

CB4_final <- merge(CB4_merged, TargetsOrder, by.x="Target", by.y="Target")

trial_model.sg <- glmer(as.factor(Aligncode) ~ scale(as.numeric(Order)) + (1+scale(as.numeric(Order))|ID), 
                    data = CB4_final, family=binomial, 
                    na.action = na.omit, 
                    glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(trial_model.sg) 

trial_model <- glmer(as.factor(Aligncode) ~ scale(as.numeric(Order)) + (1|ID), 
                        data = CB4_final, family=binomial, 
                        na.action = na.omit, 
                        glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(trial_model) 

TrialsEntrainmentProportions <- CB4_final %>%
  dplyr::group_by(Order, Cyberball) %>%
  dplyr::summarise(mean=mean(as.numeric(as.character(Aligncode)))*100, 
                   Entrain.sd = sd(as.numeric(as.character(Aligncode)))*100,
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n()))
TrialsEntrainmentProportions <- with(TrialsEntrainmentProportions, TrialsEntrainmentProportions[order(as.numeric(as.character(Order))),])

TrialsOrder_plot1 <- ggplot() +
  geom_point(data=TrialsEntrainmentProportions, 
             mapping=aes(x=as.numeric(as.character(Order)), y=mean, group=Cyberball), 
             size=3, 
             shape=21) +
  geom_errorbar(data=TrialsEntrainmentProportions, 
                mapping=aes(x=as.numeric(as.character(Order)), 
                            ymin=mean-Entrain.se, ymax=mean+Entrain.se), 
                width=0.2, 
                size=.8, 
                color="black") +
  geom_line(data=TrialsEntrainmentProportions, 
            mapping=aes(x=as.numeric(as.character(Order)), 
                        y=mean, 
                        linetype=Cyberball, color=Cyberball))+
  labs(x = "Trial Number", y = "Use of Disfavoured Labels (%)", title="Experiment 1") +  
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title.x = element_text(color="black", size=16, face="bold"), 
    axis.title.y = element_text(color="black", size=16, face="bold"), 
    axis.text.x = element_text(color="black", size=16), 
    axis.text.y = element_text(color="black", size=16), 
    legend.text = element_text(color="black", size=16),
    legend.title = element_text(color = "black", size=16),
    plot.title = element_text(color="black", size=16, face = "bold", hjust = 0.5), 
    legend.position=c(0.5, 0.95),
    legend.direction = "horizontal") +
  scale_y_continuous(breaks=seq(0,100,by=10), limits=c(0,85))+
  scale_x_continuous(
    breaks = c(1:15)) +
  scale_colour_manual(values=c("grey", "black"), 
                      name  ="Cyberball Condition:",
                      breaks=c("Inclusion", "Ostracism"),
                      labels=c("Control", "Ostracism"))+
  scale_linetype_manual(values=c("solid", "dashed"), 
                        name  ="Cyberball Condition:",
                        breaks=c("Inclusion", "Ostracism"),
                        labels=c("Control", "Ostracism")) 

TrialsOrder_plot1

