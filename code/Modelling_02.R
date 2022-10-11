
rm(list = ls())

library(reshape2)
library(MuMIn)
library(ggplot2)
library(lme4)

###load in data
BirdRich<-read.csv("C:\\Users\\rbuxton\\Documents\\eBird public  health\\data\\RichDiv_95 (2).csv")
Hospitalizations<-read.csv("C:\\Users\\rbuxton\\Documents\\eBird public  health\\data\\AnxData_Zipcodes_2000to2018.csv")

###load in function to reshape the dataframe
source("C:\\Users\\rbuxton\\Documents\\eBird public  health\\Code\\ReshapeDataframes_01.R")

###reshape the dataframe
MergedSpRichHosp<-ReshapeFunction(BirdRich, Hospitalizations)

###Make some basic plots

# plot(log(MergedSpRichHosp$Sp_Diversity+1), MergedSpRichHosp$Anxiety_Mood_Hosp)
# plot(MergedSpRichHosp$Sp_Richness/MergedSpRichHosp$Area, MergedSpRichHosp$Anxiety_Mood_Hosp/MergedSpRichHosp$Area)

# plot(MergedSpRichHosp$Sp_Diversity, MergedSpRichHosp$Anxiety_Mood_Hosp)
# plot(MergedSpRichHosp$Sp_Richness, MergedSpRichHosp$Area)

####Center scale numeric covariates for modelling
MergedSpRichHosp$Income_s<-scale(MergedSpRichHosp$Income)[,1]
MergedSpRichHosp$Black_s<-scale(MergedSpRichHosp$Prop_black)[,1]
MergedSpRichHosp$Sp_Richness_s<-scale(MergedSpRichHosp$Sp_Richness)[,1]
MergedSpRichHosp$Sp_Diversity_s<-scale(MergedSpRichHosp$Sp_Diversity)[,1]
MergedSpRichHosp$Area_s<-scale(MergedSpRichHosp$Area)[,1]
MergedSpRichHosp$Year_s<-scale(MergedSpRichHosp$Year)[,1]

###Remove missing data
MergedSpRichHosp_cc<-MergedSpRichHosp[complete.cases(MergedSpRichHosp$Prop_black),]
MergedSpRichHosp_cc<-MergedSpRichHosp_cc[complete.cases(MergedSpRichHosp_cc$Sp_Richness),]
MergedSpRichHosp_cc<-MergedSpRichHosp_cc[complete.cases(MergedSpRichHosp_cc$Income),]

###Take out postal codes with fewer than 17 eBird checklists
Over17sub<-subset(MergedSpRichHosp_cc, Completed_lists>=17)

###Correlations
cor(Over17sub[,c("Sp_Diversity", "Sp_Richness", "Area", "Income", "Prop_black")], method="spearman")
#Sp_Diversity and Sp_Richness are correlated (R>0,5)

###Create a global model - compare sp_diversity and sp_richness
m1<-lm(Anxiety_Mood_Hosp~Income_s+Black_s+Sp_Richness_s+Area_s+Year_s+ZIP,data=Over17sub, na.action = na.fail)
m2<-lm(Anxiety_Mood_Hosp~Income_s+Black_s+Sp_Diversity_s+Area_s+Year_s+ZIP,data=Over17sub, na.action = na.fail)

AIC(m1,m2) #m1 lower AIC - choose this one

m3<-lmer(Anxiety_Mood_Hosp~Income_s+Black_s+Sp_Richness_s+Area_s+(1|ZIP)+(1|Year),data=Over17sub, na.action = na.fail)
m4<-lmer(Anxiety_Mood_Hosp~Income_s+Black_s+Sp_Diversity_s+Area_s+(1|ZIP)+(1|Year),data=Over17sub, na.action = na.fail)
AIC(m3,m4) #m3 lower AIC - choose this one

##Dredge models
DredgeMixedMod<-dredge(m3)
DredgeLinMod<-dredge(m1)

#TOP  MODELS
TopMixedMod<-lmer(Anxiety_Mood_Hosp~Sp_Richness_s+Income_s+(1|ZIP)+(1|Year),data=Over17sub, na.action = na.fail)
TopLinMod<-lm(Anxiety_Mood_Hosp~Income_s+Black_s+Sp_Richness_s+Area_s+ZIP,data=Over17sub, na.action = na.fail)

#Confidence intervals around PE
confint(TopMixedMod)
confint(TopLinMod)

#R squared
r.squaredGLMM(TopMixedMod)
summary(TopLinMod)$r.squared


#############################MEAN INCOME OF HIGHER/LOWER HOSP
mean(subset(Over17sub, Income>=80000)$Anxiety_Mood_Hosp) #hospitalization rate is lower for higher income
mean(subset(Over17sub, Income<=20000)$Anxiety_Mood_Hosp)

mean(na.omit(subset(MergedSpRichHosp, Anxiety_Mood_Hosp<=20)$Income))#income is higher (53510.91) for lower hospitalization
mean(na.omit(subset(MergedSpRichHosp, Anxiety_Mood_Hosp>=20)$Income))#income is lower (39992.17)

##Try running a model for middle-upper class
MergedSpRichHosp<-subset(Over17sub, Income>=40000)

m5<-lm(Anxiety_Mood_Hosp~Income_s+Black_s+Sp_Richness_s+Area_s+Year_s+ZIP,data=MergedSpRichHosp, na.action = na.fail)
m6<-lmer(Anxiety_Mood_Hosp~Income_s+Black_s+Sp_Richness_s+Area_s+(1|ZIP)+(1|Year),data=MergedSpRichHosp, na.action = na.fail)

DredgeMixedMod_midhighincome<-dredge(m6)
DredgeLinMod_midhighincome<-dredge(m5)


#TOP  MODELS
TopMixedMod<-lmer(Anxiety_Mood_Hosp~Black_s+Income_s+Area_s+(1|ZIP)+(1|Year),data=Over17sub, na.action = na.fail)
TopLinMod<-lm(Anxiety_Mood_Hosp~Income_s+Sp_Richness_s+Area_s+ZIP,data=Over17sub, na.action = na.fail)


##Plots

ggplot(MergedSpRichHosp, aes(Sp_Richness, Anxiety_Mood_Hosp)) +
  geom_point()+
  geom_smooth(method = lm)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # this removes the linse in the back.
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))

ggplot(MergedSpRichHosp, aes(Completed_lists, Income)) +
  geom_point()+
  geom_smooth(method = lm)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # this removes the linse in the back.
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))

ggplot(MergedSpRichHosp, aes(Anxiety_Mood_Hosp, Income)) +
  geom_point()+
  geom_smooth(method = lm)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # this removes the linse in the back.
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))
