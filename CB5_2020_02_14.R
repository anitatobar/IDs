getwd()
setwd("/home/anita/Dropbox/R/EnglishCoordination/CB/FifthStudy")
getwd()

library(dplyr)
library(ggplot2)
library(lmerTest)
library(psych)

# Answers 

CB5 <- read.csv(file= "CB5_cleandata_2020_02_14.csv", 
                header = TRUE, 
                sep=",", 
                stringsAsFactors=TRUE) %>%
  dplyr::select(-c("X"))

Neuro <- read.csv(file="CB5_Neuro.csv", 
                  header = T, 
                  sep = ",", 
                  stringsAsFactors = F) %>%
  dplyr::select(-c("X")) %>%
  dplyr::rename(Prolific=neuro.Q10, 
                Neuroticism=total)

CB5_merged <- merge(CB5, 
                    Neuro %>% select(Prolific, Neuroticism),
                    by.x = "Prolific", 
                    by.y = "Prolific")



##############################################
# Plots 
##############################################

# Alignment Effect

AlignByTarget <- CB5_merged %>% 
  dplyr::group_by(Target) %>% 
  dplyr::summarise(AlignByTarget = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)*100)
summary(AlignByTarget)
sd(AlignByTarget$AlignByTarget)

AlignByPart <- CB5_merged %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarise(AlignByPart = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)*100)
summary(AlignByPart)
sd(AlignByPart$AlignByPart)

freq <- CB5_merged %>% 
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

PartnerProportions <- CB5_merged %>%
  dplyr::group_by(ID, Partner) %>%
  dplyr::summarise(Entrain.m = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)) %>%
  dplyr::group_by(Partner) %>%
  dplyr::summarise(Mean = mean(Entrain.m),
                   SD = sd(Entrain.m),
                   N = n(),
                   Entrain.se = SD/sqrt(n()))
PartnerProportions

Partner_plot = ggplot(PartnerProportions, aes(x = factor(Partner), y = Mean)) +
  geom_bar(stat="identity",position="dodge", width=.3)+
  geom_errorbar(aes(ymax=Mean+Entrain.se,ymin=Mean-Entrain.se),position=position_dodge(width=0.5),width=0.1)+
  coord_cartesian(ylim=c(0,1))+
  labs(x = "Conditions", 
       y = "Percentage of entrainment", 
       fill = "Entrainment", 
       title = "Proportion of entrainment\n by Condition") +
  theme_minimal(base_size = 14) +
  theme(axis.title.x = element_blank())
Partner_plot

CyberballProportions <- CB5_merged %>%
  dplyr::group_by(ID, SocialExclusion) %>%
  dplyr::summarise(Entrain.m = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)) %>%
  dplyr::group_by(SocialExclusion) %>%
  dplyr::summarise(Mean = mean(Entrain.m),
                   SD = sd(Entrain.m),
                   N = n(),
                   Entrain.se = SD/sqrt(n()))
CyberballProportions

Cyberball_plot = ggplot(CyberballProportions, aes(x = factor(SocialExclusion), y = Mean)) +
  geom_bar(stat="identity",position="dodge", width=.3)+
  geom_errorbar(aes(ymax=Mean+Entrain.se,ymin=Mean-Entrain.se),position=position_dodge(width=0.5),width=0.1)+
  coord_cartesian(ylim=c(0,1))+
  labs(x = "Conditions", 
       y = "Percentage of entrainment", 
       fill = "Entrainment", 
       title = "Proportion of entrainment\n by Condition") +
  theme_minimal(base_size = 14) +
  theme(axis.title.x = element_blank())
Cyberball_plot

PartnerCyberball = CB5_merged %>%
  dplyr::group_by(SocialExclusion, Partner, ID) %>%
  dplyr::summarise(align = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)) %>%
  dplyr::group_by(SocialExclusion, Partner) %>%
  dplyr::summarise(Entrain.m = mean(align)*100,
                   Entrain.sd = sd(align)*100,
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n())) %>%
  dplyr::mutate(Cyberball = ifelse(SocialExclusion %in% c("Inclusion"), "Inclusion", "Ostracism"), 
                Partner = ifelse(Partner %in% c("Same"), "Same", "New")) %>%
  dplyr::ungroup() %>%
  dplyr::select(Cyberball, Partner, Entrain.m, Entrain.sd, N, Entrain.se)

PartnerCyberball

PartnerCyberball$Cyberball <-as.character(PartnerCyberball$Cyberball)
PartnerCyberball$Partner <-as.character(PartnerCyberball$Partner)
PartnerCyberball[5,] =c("Pretest", "Pretest", 4.8, 6, 15, 1)
PartnerCyberball$Entrain.m <- as.numeric(PartnerCyberball$Entrain.m)
PartnerCyberball$Entrain.se <- as.numeric(PartnerCyberball$Entrain.se)
PartnerCyberball$Cyberball <-as.factor(PartnerCyberball$Cyberball)
PartnerCyberball$Partner <-as.factor(PartnerCyberball$Partner)


plot1 <- ggplot(subset(PartnerCyberball, Cyberball != "Pretest"),
                aes(x = Cyberball, y = as.numeric(as.character(Entrain.m)), fill=Partner))+
  geom_bar(stat = "identity",position = "dodge", width = .8)+
  geom_errorbar(aes(ymax=Entrain.m+Entrain.se,ymin=Entrain.m-Entrain.se),
                position=position_dodge(width=0.8),
                width=0.1)+
  geom_hline(aes(yintercept = subset(PartnerCyberball, Partner == "Pretest")$Entrain.m),linetype=8)+
  scale_linetype_manual(name = "", values = c(2))+
  theme_bw() +
  theme(
    legend.position=c(0.5, 0.95),
    legend.direction = "horizontal",
    axis.line= element_line(), 
    axis.title.y = element_text(color="black", size=16, face = "bold"), 
    axis.title.x = element_text(color="black", size=16, face = "bold"), 
    axis.text.x = element_text(color="black", size=16), 
    axis.text.y = element_text(color="black", size=16), 
    legend.title=element_text(color="black", size=16, face = "bold"), 
    legend.text=element_text(color="black", size=16),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(), 
    strip.text = element_text(color="black", size=16)) +
  expand_limits(y=c(0,60)) + 
  scale_fill_manual( values=c("grey", "grey30"), 
                     name="Partner's Identity", 
                     breaks = c("Same", "New"), 
                     labels=c("Same Partner", "New Partner"))+
  scale_x_discrete( breaks = c("Inclusion", "Ostracism"), 
                    labels=c("Control", "Ostracism"), 
                    name="Cyberball Condition")+
  labs(y="Use of Disfavored Labels (%)")

plot1



Sex = CB5_merged %>%
  dplyr::group_by(Gender, ID) %>%
  dplyr::summarise(align = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)) %>%
  dplyr::group_by(Gender) %>%
  dplyr::summarise(Entrain.m = mean(align),
                   Entrain.sd = sd(align),
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n())) 
Sex

Sex_plot <- ggplot(Sex, aes(x=Gender, y=Entrain.m)) + 
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

describe(CB5_merged$Neuroticism)
hist(CB5_merged$Neuroticism)

AlignByPart_Ostra <- CB5_merged %>% 
  dplyr::filter(SocialExclusion == "Exclusion") %>%
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

AlignByPart_Control <- CB5_merged %>% 
  dplyr::filter(SocialExclusion == "Inclusion") %>%
  dplyr::group_by(Prolific, Neuroticism) %>% 
  dplyr::summarise(AlignByPart = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)*100)
describe(AlignByPart_Control)
cor.test(log(AlignByPart_Control$Neuroticism), log(AlignByPart_Control$AlignByPart+1))

NeuroPlot_Control <- ggplot(data = AlignByPart_Control, aes(x = Neuroticism, y = AlignByPart)) + 
  geom_point(position =  "jitter", size=1.5) +
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
  scale_x_continuous(breaks = c(15,20,25,30,35,40), 
                     limits = c(15,40)) +
  scale_y_continuous(breaks = c(0,20,40,60,80,100), 
                     limits = c(0,100)) 
NeuroPlot_Control

source("http://peterhaschke.com/Code/multiplot.R")
multiplot(NeuroPlot_Control,NeuroPlot_Ostra,cols=2)


PartnerCyberballSex = CB5_merged %>%
  dplyr::group_by(SocialExclusion, Partner, Gender, ID) %>%
  dplyr::summarise(align = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)) %>%
  dplyr::group_by(SocialExclusion, Partner, Gender) %>%
  dplyr::summarise(Entrain.m = mean(align),
                   Entrain.sd = sd(align),
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n())) 
PartnerCyberballSex


CyberballPartnerSex_plot <- ggplot(PartnerCyberballSex, aes(x=SocialExclusion, y=Entrain.m, fill=Partner)) + 
  geom_bar(stat="identity", position="dodge", width=.3) +
  geom_errorbar(aes(ymax=Entrain.m+Entrain.se,ymin=Entrain.m-Entrain.se),position=position_dodge(width=0.5),width=0.1)+
  coord_cartesian(ylim=c(0,1))+
  scale_fill_manual( values=c("grey", "deeppink3"), name="Partner in \nnaming task")+
  facet_wrap(~Gender) +
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

#----------------------------------------------#
#             VARIABLES 
#----------------------------------------------#

summary(CB5_merged)
CB5_merged <- na.omit(CB5_merged)

#INDEPENDENT VARIABLES:

#1.Partner
CB5_merged$devpartner <- ifelse(CB5_merged$Partner == "Same", -.5, .5)

#2.Cyberball Condition
CB5_merged$devconditions <- ifelse(CB5_merged$SocialExclusion == "Inclusion", -.5, .5)

#4.Personality
CB5_merged$devNeuroticism <- log(CB5_merged$Neuroticism)
CB5_merged$SNeuroticism <- scale(CB5_merged$devNeuroticism)

#4.Sex
CB5_merged$devSex <- ifelse(CB5_merged$Gender == "Female", -.5, .5)

#Random Effects
CB5_merged$Target <- as.factor(CB5_merged$Target)
CB5_merged$ID <- as.factor(CB5_merged$ID)

summary(CB5_merged)

#---------------------------------------#
#             MODELS
#---------------------------------------#

fullmodel.sgfit <- glmer(as.factor(Aligncode) ~ devconditions*devpartner + devSex +(1+devpartner*devconditions + devSex|Target) + (1|ID), 
                         data = CB5_merged, family=binomial, 
                         na.action = na.omit, 
                         glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel.sgfit) 

fullmodel.sgfit2 <- glmer(as.factor(Aligncode) ~ devconditions*devpartner + devSex +(1+devconditions+devpartner+ devSex|Target) + (1|ID), 
                          data = CB5_merged, family=binomial, 
                          na.action = na.omit, 
                          glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel.sgfit2) 

fullmodel.sgfit3 <- glmer(as.factor(Aligncode) ~ devconditions*devpartner + devSex +(1+devpartner+devSex|Target) + (1|ID), 
                          data = CB5_merged, family=binomial, 
                          na.action = na.omit, 
                          glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel.sgfit3) 

fullmodel.sgfit4 <- glmer(as.factor(Aligncode) ~ devconditions*devpartner + devSex+(1+devconditions+devSex|Target) + (1|ID), 
                          data = CB5_merged, family=binomial, 
                          na.action = na.omit, 
                          glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel.sgfit4) 

fullmodel.sgfit5 <- glmer(as.factor(Aligncode) ~ devconditions*devpartner + devSex+(1+devSex|Target) + (1|ID), 
                          data = CB5_merged, family=binomial, 
                          na.action = na.omit, 
                          glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel.sgfit5) 

fullmodel <- glmer(as.factor(Aligncode) ~ devconditions*devpartner+ devSex+(1|Target) + (1|ID), 
                   data = CB5_merged, family=binomial, 
                   na.action = na.omit, 
                   glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel) 


ostramodel <- glmer(as.factor(Aligncode) ~ devconditions +(1|Target) + (1|ID), 
                   data = CB5_merged, family=binomial, 
                   na.action = na.omit, 
                   glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(ostramodel) 
BIC(ostramodel) #1728.322

partnermodel <- glmer(as.factor(Aligncode) ~ devpartner+(1|Target) + (1|ID), 
                      data = CB5_merged, family=binomial, 
                      na.action = na.omit, 
                      glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(partnermodel) 

nullmodel <- glmer(as.factor(Aligncode) ~ 1+(1|Target) + (1|ID), 
                   data = CB5_merged, family=binomial, 
                   na.action = na.omit, 
                   glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(nullmodel) 

BIC(partnermodel) #1728.592
BIC(nullmodel) #1723.639

BF_BIC_partner = exp((BIC(partnermodel) - BIC(nullmodel))/2)  # BICs to Bayes factor
BF_BIC_partner

BF_BIC_partner / (BF_BIC_partner + 1)

BF_BIC_ostra = exp((BIC(ostramodel) - BIC(nullmodel))/2)  # BICs to Bayes factor
BF_BIC_ostra

BF_BIC_ostra / (BF_BIC_ostra + 1)

fullmodel_Ostra <- glmer(as.factor(Aligncode) ~ devpartner +(1 |Target) + (1|ID), 
                         data = subset(CB5_merged, SocialExclusion=="Exclusion"), family=binomial, 
                         na.action = na.omit, 
                         glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel_Ostra) 

fullmodel_Control.sg <- glmer(as.factor(Aligncode) ~ devpartner +(1+devpartner |Target) + (1|ID), 
                           data = subset(CB5_merged, SocialExclusion=="Inclusion"), family=binomial, 
                           na.action = na.omit, 
                           glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel_Control.sg) 

fullmodel_Control <- glmer(as.factor(Aligncode) ~ devpartner +(1 |Target) + (1|ID), 
                           data = subset(CB5_merged, SocialExclusion=="Inclusion"), family=binomial, 
                           na.action = na.omit, 
                           glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel_Control) 

#BF expresses the probability of the data given H1 relative to H0 (i.e., values larger than 1 are in favour of H1)



# Neuroticism in Control Condition

fullmodel_Neuroticism <- glmer(as.factor(Aligncode) ~ SNeuroticism +(1 +SNeuroticism |Target) + (1|ID), 
                                   data = subset(CB5_merged, SocialExclusion=="Inclusion"), family=binomial, 
                                   na.action = na.omit, 
                                   glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel_Neuroticism) 

full_neuro <- glmer(as.factor(Aligncode) ~ SNeuroticism +(1|Target) + (1|ID), 
                    data = subset(CB5_merged, SocialExclusion=="Inclusion"), family=binomial, 
                    na.action = na.omit, 
                    glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(full_neuro) 

null_neuro <- glmer(as.factor(Aligncode) ~ 1 +(1|Target) + (1|ID), 
                    data = subset(CB5_merged, SocialExclusion=="Inclusion"), family=binomial, 
                    na.action = na.omit, 
                    glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(null_neuro) 

BIC(full_neuro) #897.9845
BIC(null_neuro) #893.0673

BF_BIC_neuro = exp((BIC(full_neuro) - BIC(null_neuro))/2)  # BICs to Bayes factor
BF_BIC_neuro

BF_BIC_neuro / (BF_BIC_neuro + 1)



# Neuro in Exclusion condition

fullmodel_Neuroticism.sg1 <- glmer(as.factor(Aligncode) ~ SNeuroticism +(1 +SNeuroticism |Target) + (1|ID), 
                               data = subset(CB5_merged, SocialExclusion=="Exclusion"), family=binomial, 
                               na.action = na.omit, 
                               glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel_Neuroticism.sg1) 

fullmodel_Neuroticism_os <- glmer(as.factor(Aligncode) ~ SNeuroticism+(1 |Target) + (1|ID), 
                               data = subset(CB5_merged, SocialExclusion=="Exclusion"), family=binomial, 
                               na.action = na.omit, 
                               glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel_Neuroticism_os) 

full_neuro_os <- glmer(as.factor(Aligncode) ~ SNeuroticism +(1|Target) + (1|ID), 
                    data = subset(CB5_merged, SocialExclusion=="Exclusion"), family=binomial, 
                    na.action = na.omit, 
                    glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(full_neuro_os) 

null_neuro_os <- glmer(as.factor(Aligncode) ~ 1 +(1|Target) + (1|ID), 
                    data = subset(CB5_merged, SocialExclusion=="Exclusion"), family=binomial, 
                    na.action = na.omit, 
                    glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(null_neuro_os) 

BIC(full_neuro_os) #867.1338
BIC(null_neuro_os) #863.9878

BF_BIC_neuro = exp((BIC(full_neuro_os) - BIC(null_neuro_os))/2)  # BICs to Bayes factor
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

CB5_final <- merge(CB5_merged, TargetsOrder, by.x="Target", by.y="Target")

TrialsEntrainmentProportions <- CB5_final %>%
  dplyr::group_by(Order, SocialExclusion) %>%
  dplyr::summarise(mean=mean(as.numeric(as.character(Aligncode)))*100, 
                   Entrain.sd = sd(as.numeric(as.character(Aligncode)))*100,
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n()))
TrialsEntrainmentProportions <- with(TrialsEntrainmentProportions, TrialsEntrainmentProportions[order(as.numeric(as.character(Order))),])

TrialsOrder_plot2 <- ggplot() +
  geom_point(data=TrialsEntrainmentProportions, 
             mapping=aes(x=as.numeric(as.character(Order)), y=mean, group=SocialExclusion), 
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
                        linetype=SocialExclusion, color=SocialExclusion))+
  labs(x = "Trial Number", y = "Use of Disfavoured Labels (%)", title="Experiment 2") +  
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
  scale_colour_manual(values=c("black", "grey"), 
                      name  ="Cyberball Condition:",
                      breaks=c("Inclusion", "Exclusion"),
                      labels=c("Control", "Ostracism"))+
  scale_linetype_manual(values=c("dashed", "solid"), 
                        name  ="Cyberball Condition:",
                        breaks=c("Inclusion", "Exclusion"),
                        labels=c("Control", "Ostracism")) 

TrialsOrder_plot2

trial_model <- glmer(as.factor(Aligncode) ~ scale(as.numeric(Order)) + (1|ID), 
                     data = CB5_final, family=binomial, 
                     na.action = na.omit, 
                     glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(trial_model) 

###

CB4 <- read.csv(file= "CB4_cleandata_2020_02_14.csv", header = TRUE, sep=",", stringsAsFactors=TRUE) %>%
  dplyr::select(Prolific, 
                ID, 
             Frequency, 
             Partner, 
             Cyberball, 
             Target, 
             Sex, 
             Aligncode, 
             Neuroticism) %>%
  dplyr::mutate(Partner = ifelse(Partner %in% c("new", "New"), "New", "Same"), 
                SocialExclusion = ifelse(Cyberball %in% c("inclusion", "Inclusion"), "Inclusion", "Exclusion"), 
                Gender=Sex) %>%
  dplyr::select(-c(Sex, Cyberball))

Neuro_CB4 <- read.csv(file="CB4_Neuro.csv", 
                  header = T, 
                  sep = ",", 
                  stringsAsFactors = F)

CB4_tomerge <- merge(CB4, 
                    Neuro_CB4 %>% select(neuro.prolificID, total) %>%
                      rename(Prolific = neuro.prolificID, 
                             Neuroticism = total),
                    by.x = "Prolific", 
                    by.y = "Prolific") %>%
  dplyr::select(-c(Prolific, Neuroticism.x)) %>%
  dplyr::rename(Neuroticism = Neuroticism.y)

CB4_tomerge$Exp <- "First"
CB5_merged$Exp <- "Second"

CB45 <- merge(rbind(CB4_tomerge, CB5_merged %>%
                dplyr::select(ID, 
                              Frequency, 
                              Partner, 
                              SocialExclusion, 
                              Target, 
                              Gender, 
                              Aligncode, 
                              Neuroticism, 
                              Exp)), 
              TargetsOrder, 
              by.x= "Target", 
              by.y = "Target")
              

# Conditions:

AlignByTarget45 <- CB45 %>% 
  dplyr::group_by(Target) %>% 
  dplyr::summarise(AlignByTarget = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)*100)
summary(AlignByTarget45)
sd(AlignByTarget45$AlignByTarget)

AlignByPart45 <- CB45 %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarise(AlignByPart = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)*100)
summary(AlignByPart45)
sd(AlignByPart45$AlignByPart)

PartnerProportions2 <- CB45 %>%
  dplyr::group_by(ID, Partner) %>%
  dplyr::summarise(Entrain.m = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)) %>%
  dplyr::group_by(Partner) %>%
  dplyr::summarise(Mean = mean(Entrain.m),
                   SD = sd(Entrain.m),
                   N = n(),
                   Entrain.se = SD/sqrt(n()))
PartnerProportions2

Partner_plot2 = ggplot(PartnerProportions2, aes(x = factor(Partner), y = Mean)) +
  geom_bar(stat="identity",position="dodge", width=.3)+
  geom_errorbar(aes(ymax=Mean+Entrain.se,ymin=Mean-Entrain.se),position=position_dodge(width=0.5),width=0.1)+
  coord_cartesian(ylim=c(0,1))+
  labs(x = "Conditions", 
       y = "Percentage of entrainment", 
       fill = "Entrainment", 
       title = "Proportion of entrainment\n by Condition") +
  theme_minimal(base_size = 14) +
  theme(axis.title.x = element_blank())
Partner_plot2

CyberballProportions2 <- CB45 %>%
  dplyr::group_by(ID, SocialExclusion) %>%
  dplyr::summarise(Entrain.m = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)) %>%
  dplyr::group_by(SocialExclusion) %>%
  dplyr::summarise(Mean = mean(Entrain.m),
                   SD = sd(Entrain.m),
                   N = n(),
                   Entrain.se = SD/sqrt(n()))
CyberballProportions2

Cyberball_plot2 = ggplot(CyberballProportions2, aes(x = factor(SocialExclusion), y = Mean)) +
  geom_bar(stat="identity",position="dodge", width=.3)+
  geom_errorbar(aes(ymax=Mean+Entrain.se,ymin=Mean-Entrain.se),position=position_dodge(width=0.5),width=0.1)+
  coord_cartesian(ylim=c(0,1))+
  labs(x = "Conditions", 
       y = "Percentage of entrainment", 
       fill = "Entrainment", 
       title = "Proportion of entrainment\n by Condition") +
  theme_minimal(base_size = 14) +
  theme(axis.title.x = element_blank())
Cyberball_plot2

PartnerCyberball2 = as.data.frame(CB45 %>%
  dplyr::group_by(SocialExclusion, Partner, ID) %>%
  dplyr::summarise(align = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)) %>%
  dplyr::group_by(SocialExclusion, Partner) %>%
  dplyr::summarise(Entrain.m = mean(align),
                   Entrain.sd = sd(align),
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n())) %>%
  dplyr::mutate(Cyberball2 = ifelse(SocialExclusion %in% c("Inclusion"), "Inclusion", "Ostracism"), 
                Partner2 = ifelse(Partner %in% c("Same"), "Same", "New")) %>%
  dplyr::select(Cyberball2, Partner2, Entrain.m, Entrain.sd, Entrain.se, N))
PartnerCyberball2

PartnerCyberball2$Cyberball2 <-as.character(PartnerCyberball2$Cyberball2)
PartnerCyberball2$Partner2 <-as.character(PartnerCyberball2$Partner2)
PartnerCyberball2[5,] =c("Pretest", "Pretest", "Pretest", 4.8, 6, 15, 1)
PartnerCyberball2$Entrain.m <- as.numeric(PartnerCyberball2$Entrain.m)
PartnerCyberball2$Entrain.se <- as.numeric(PartnerCyberball2$Entrain.se)
PartnerCyberball2$Cyberball2 <-as.factor(PartnerCyberball2$Cyberball2)
PartnerCyberball2$Partner2 <-as.factor(PartnerCyberball2$Partner2)

CyberballPartner_plot2 <- ggplot(subset(PartnerCyberball2, Cyberball2 != "Pretest"),
                                 aes(x=Cyberball2, y=as.numeric(as.character(Entrain.m*100)), fill=Partner2)) + 
  geom_bar(stat="identity", position="dodge", width=.8) +
  geom_errorbar(aes(ymax=Entrain.m*100+Entrain.se*100,ymin=Entrain.m*100-Entrain.se*100),
                position=position_dodge(width=0.8),
                width=0.1)+
  geom_hline(aes(yintercept = subset(PartnerCyberball2, Cyberball2 == "Pretest")$Entrain.m),linetype=8)+
  scale_linetype_manual(name = "", values = c(2))+
  theme_bw()+
  theme(
    legend.position=c(0.5, 0.95),
    legend.direction = "horizontal",
    axis.line= element_line(), 
    axis.title.y = element_text(color="black", size=16, face = "bold"), 
    axis.title.x = element_text(color="black", size=16, face = "bold"), 
    axis.text.x = element_text(color="black", size=16), 
    axis.text.y = element_text(color="black", size=16), 
    legend.title=element_text(color="black", size=16, face = "bold"), 
    legend.text=element_text(color="black", size=16),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(), 
    strip.text = element_text(color="black", size=16)) +
  expand_limits(y=c(0,60)) + 
  scale_fill_manual( values=c("grey", "grey30"), 
                     name="Partner's Identity", 
                     breaks = c("Same", "New"), 
                     labels=c("Same Partner", "New Partner"))+
  scale_x_discrete( name="Cyberball Condition",
                    breaks = c("Inclusion", "Ostracism"), 
                    labels=c("Control", "Ostracism"))+
  labs(y="Use of Disfavoured Labels (%)")
CyberballPartner_plot2
  
describe(CB45$Neuroticism)
hist(CB45$Neuroticism)

AlignByPart_Ostra <- CB45 %>% 
  dplyr::filter(SocialExclusion == "Exclusion") %>%
  dplyr::group_by(ID, Neuroticism) %>% 
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
  scale_x_continuous(breaks = c(5,10,15,20,25,30,35,40), 
                     limits = c(15,40)) +
  scale_y_continuous(breaks = c(0,20,40,60,80,100), 
                     limits = c(0,100)) 
NeuroPlot_Ostra


AlignByPart_Control <- CB45 %>% 
  dplyr::filter(SocialExclusion == "Inclusion") %>%
  dplyr::group_by(ID, Neuroticism) %>% 
  dplyr::summarise(AlignByPart = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)*100)
describe(AlignByPart_Control)
cor.test(log(AlignByPart_Control$Neuroticism), log(AlignByPart_Control$AlignByPart+1))

NeuroPlot_Control <- ggplot(data = AlignByPart_Control, aes(x = Neuroticism, y = AlignByPart)) + 
  geom_point(position =  "jitter", size=1.5) +
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
  scale_x_continuous(breaks = c(15,20,25,30,35,40), 
                     limits = c(15,40)) +
  scale_y_continuous(breaks = c(0,20,40,60,80,100), 
                     limits = c(0,100)) 
NeuroPlot_Control

source("http://peterhaschke.com/Code/multiplot.R")
multiplot(NeuroPlot_Control,NeuroPlot_Ostra,cols=2)



Sex2 = CB45 %>%
  dplyr::group_by(Gender, ID) %>%
  dplyr::summarise(align = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)) %>%
  dplyr::group_by(Gender) %>%
  dplyr::summarise(Entrain.m = mean(align),
                   Entrain.sd = sd(align),
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n())) 
Sex2

Sex_plot2 <- ggplot(Sex2, aes(x=Gender, y=Entrain.m)) + 
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
Sex_plot2


PartnerCyberballSex2 = CB45 %>%
  dplyr::group_by(SocialExclusion, Partner, Gender, ID) %>%
  dplyr::summarise(align = mean(as.numeric(as.character(Aligncode)), na.rm = TRUE)) %>%
  dplyr::group_by(SocialExclusion, Partner, Gender) %>%
  dplyr::summarise(Entrain.m = mean(align),
                   Entrain.sd = sd(align),
                   N = n(),
                   Entrain.se = Entrain.sd/sqrt(n())) 
PartnerCyberballSex2


CyberballPartnerSex_plot2 <- ggplot(PartnerCyberballSex2, aes(x=SocialExclusion, y=Entrain.m, fill=Partner)) + 
  geom_bar(stat="identity", position="dodge", width=.3) +
  geom_errorbar(aes(ymax=Entrain.m+Entrain.se,ymin=Entrain.m-Entrain.se),position=position_dodge(width=0.5),width=0.1)+
  coord_cartesian(ylim=c(0,1))+
  scale_fill_manual( values=c("grey", "deeppink3"), name="Partner in \nnaming task")+
  facet_wrap(~Gender) +
  theme_bw() +
  theme(axis.title.x = element_text(color="black", size=12, face = "bold", margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(color="black", size=12, face = "bold"), 
        axis.text = element_text(color="black", size=12), 
        legend.title = element_text(color= "black", size=12, face= "bold"), 
        title = element_text(color="black", size=14, face="bold")) +
  labs(x="Cyberball condition", 
       title="Proportion of Entrainment by Cyberball Condition, \nPartner Condition, and Sex") +
  scale_y_continuous(name = "Proportion of entrainment") 

CyberballPartnerSex_plot2


CB45$Aligncode <- as.factor(CB45$Aligncode)
CB45$devpartner <- ifelse(CB45$Partner == "Same", -.5, .5)
CB45$devconditions <- ifelse(CB45$SocialExclusion == "Inclusion", -.5, .5)
CB45$devexp <- ifelse(CB45$Exp == "First", -.5, .5)
CB45$devsex <- ifelse(CB45$Gender == "Female", -.5, -5)

fullmodel.sg1 <- glmer(as.factor(Aligncode) ~ devpartner*devconditions + devconditions*devexp + devsex + (1 + devpartner*devconditions + devconditions*devexp + devsex|Target) + (1|ID), 
                   data = CB45, family=binomial, 
                   na.action = na.omit, 
                   glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel.sg1) 

fullmodel.sg2 <- glmer(as.factor(Aligncode) ~ devpartner*devconditions + devconditions*devexp + devsex + (1 + devpartner+devconditions + devconditions*devexp + devsex|Target) + (1|ID), 
                       data = CB45, family=binomial, 
                       na.action = na.omit, 
                       glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel.sg2)

fullmodel.sg3 <- glmer(as.factor(Aligncode) ~ devpartner*devconditions + devconditions*devexp + devsex + (1 + devpartner+devconditions+devexp + devsex|Target) + (1|ID), 
                       data = CB45, family=binomial, 
                       na.action = na.omit, 
                       glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel.sg3) 

fullmodel.sg4 <- glmer(as.factor(Aligncode) ~ devpartner*devconditions + devconditions*devexp + devsex + (1 + devpartner+devconditions+devexp|Target) + (1|ID), 
                       data = CB45, family=binomial, 
                       na.action = na.omit, 
                       glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel.sg4) 

fullmodel.sg5 <- glmer(as.factor(Aligncode) ~ devpartner*devconditions + devconditions*devexp + devsex + (1 + devconditions+devexp|Target) + (1|ID), 
                       data = CB45, family=binomial, 
                       na.action = na.omit, 
                       glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel.sg5) 

fullmodel.sg6 <- glmer(as.factor(Aligncode) ~ devpartner*devconditions + devconditions*devexp + devsex + (1 + devexp|Target) + (1|ID), 
                       data = CB45, family=binomial, 
                       na.action = na.omit, 
                       glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel.sg6) 

fullmodel <- glmer(as.factor(Aligncode) ~ devpartner*devconditions + devconditions*devexp + devsex + (1|Target) + (1|ID), 
                   data = CB45, family=binomial, 
                   na.action = na.omit, 
                   glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel) 

fullmodel_exp <- glmer(as.factor(Aligncode) ~ devexp + (1|Target) + (1|ID), 
                   data = CB45, family=binomial, 
                   na.action = na.omit, 
                   glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel_exp) 

nullmodel_exp <- glmer(as.factor(Aligncode) ~ 1 + (1|Target) + (1|ID), 
                       data = CB45, family=binomial, 
                       na.action = na.omit, 
                       glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(nullmodel_exp) 

BIC(fullmodel_exp) #3369.065
BIC(nullmodel_exp) #3355.427

BF_BIC_exp = exp((BIC(fullmodel_exp) - BIC(nullmodel_exp)) /2)  # BICs to Bayes factor
BF_BIC_exp

BF_BIC_exp / (BF_BIC_exp + 1)

# Neuroticism Inclusion

fullmodel_neuro.in.sg1 <- glmer(as.factor(Aligncode) ~ log(Neuroticism) + (1+log(Neuroticism)|Target) + (1|ID), 
                            data = subset(CB45,SocialExclusion=="Inclusion"), family=binomial, 
                            na.action = na.omit, 
                            glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel_neuro.in.sg1) 

fullmodel_neuro2.in <- glmer(as.factor(Aligncode) ~ log(Neuroticism) + (1|Target) + (1|ID), 
                          data = subset(CB45,SocialExclusion=="Inclusion"), family=binomial, 
                          na.action = na.omit, 
                          glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel_neuro2.in) 

nullmodel_neuroin2 <- glmer(as.factor(Aligncode) ~ 1 + (1|Target) + (1|ID), 
                          data = subset(CB45,SocialExclusion=="Inclusion"), family=binomial, 
                          na.action = na.omit, 
                          glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(nullmodel_neuroin2) 

BIC(fullmodel_neuro2.in) #1749.329
BIC(nullmodel_neuroin2) #1742.514

BF_BIC_neuro2 = exp((BIC(fullmodel_neuro2.in) - BIC(nullmodel_neuroin2))/2)  # BICs to Bayes factor
BF_BIC_neuro2

BF_BIC_neuro2 / (BF_BIC_neuro2 + 1)


# Neuro in exclusion trials 

fullmodel_neuro.sg1 <- glmer(as.factor(Aligncode) ~ log(Neuroticism) + (1+log(Neuroticism)|Target) + (1|ID), 
                   data = subset(CB45,SocialExclusion=="Exclusion"), family=binomial, 
                   na.action = na.omit, 
                   glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel_neuro.sg1) 

fullmodel_neuro2 <- glmer(as.factor(Aligncode) ~ log(Neuroticism) + (1|Target) + (1|ID), 
                             data = subset(CB45,SocialExclusion=="Exclusion"), family=binomial, 
                             na.action = na.omit, 
                             glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(fullmodel_neuro2) 

nullmodel_neuro2 <- glmer(as.factor(Aligncode) ~ 1 + (1|Target) + (1|ID), 
                          data = subset(CB45,SocialExclusion=="Exclusion"), family=binomial, 
                          na.action = na.omit, 
                          glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(nullmodel_neuro2) 

BIC(fullmodel_neuro2) #1655.12
BIC(nullmodel_neuro2) #1651.79

BF_BIC_neuro2 = exp((BIC(fullmodel_neuro2) - BIC(nullmodel_neuro2))/2)  # BICs to Bayes factor
BF_BIC_neuro2

BF_BIC_neuro2 / (BF_BIC_neuro2 + 1)


# trials 

trial_model.sg <- glmer(as.factor(Aligncode) ~ scale(as.numeric(Order))*Exp + (1+scale(as.numeric(Order))|ID), 
                     data = subset(CB45), family=binomial, 
                     na.action = na.omit, 
                     glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(trial_model.sg) 

trial_model <- glmer(as.factor(Aligncode) ~ scale(as.numeric(Order))*Exp + scale(as.numeric(Order))*SocialExclusion + (1|ID), 
                     data = subset(CB45), family=binomial, 
                     na.action = na.omit, 
                     glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=400000)))
summary(trial_model) 

source("http://peterhaschke.com/Code/multiplot.R")
multiplot(TrialsOrder_plot1,TrialsOrder_plot2,cols=2)


