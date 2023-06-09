

rm(list = ls())

library(MuMIn)
library(lme4)

library(auk)
library(lubridate)
library(sf)
library(gridExtra)
library(cowplot)
library(tidyverse)

library(readr)
library(dplyr)
library(plyr)
library(ggplot2)
library(vegan)
library(reshape2)
library(data.table)
library(tidyr)
## install iNEXT package from CRAN 
# install.packages("iNEXT") 
## install the latest version from github 
# install.packages('devtools') 
# library(devtools) 
# install_github('AnneChao/iNEXT') 
library(iNEXT) 
library(ggplot2)
library(sjmisc)

library(raster)

library(sjPlot)

# setwd("I:\\eBIrd")

#################################################################################################################
################################         LOAD UP DATA     #######################################################
#################################################################################################################

# ##Loop through and clean all eBird data
# ALLYEARS_list<-list.files("eBird Michigan 2000-2018", full.names = TRUE) #All checklists filtered by complete checklists
# source("C:\\Users\\rbuxton\\Documents\\eBird public  health\\Code\\CleaningTheData_01.R")
# 
# ALLYEARS<-SpeciesMatrices(ALLYEARS_list) #ignore warnings (NAs introduced)
# 
# ##Loop through cleaned data and calculate diversity and richness indices
# source("C:\\Users\\rbuxton\\Documents\\eBird public  health\\Code\\SpeciesRichnessDiversity_02.R")
# 
# BirdSpeciesResults<-BirdRichnessDiversity(ALLYEARS)
# 
# ##Long to wide + clean bird results
# BirdSpeciesResults_w<-inner_join(subset(BirdSpeciesResults[,c("ZCTA5CE10","Diversity","Observed","Estimator","qD","Checklists", "Year")], Diversity=="Species richness"),
# subset(BirdSpeciesResults[,c("ZCTA5CE10","Diversity","Observed","Estimator","qD","Checklists", "Year")], Diversity=="Shannon diversity"),
# by = c("ZCTA5CE10", "Checklists", "Year"))%>%
#   rename(Observed_SpRich=Observed.x)%>%
#   rename(ChaoEst_SpRich=Estimator.x)%>%
#   rename(RarefiedEst_SpRich=qD.x)%>%
#   rename(Observed_SpDiv=Observed.y)%>%
#   rename(ChaoEst_SpDiv=Estimator.y)%>%
#   rename(RarefiedEst_SpDiv=qD.y)%>%
#   dplyr::select(., -c(Diversity.x, Diversity.y))
# 
# ##Load in hospitalization data
# Hospitalizations<-read.csv("C:\\Users\\rbuxton\\Documents\\eBird public  health\\data\\AnxData_Zipcodes_2000to2018.csv")
# 
# #Reshape hospitalizations
# Hospitals<-pivot_longer(Hospitalizations[,1:12], -c(ZIP), values_to = "Anxiety_Mood_Hosp", names_to = "Year")%>%
#   mutate(Year = gsub("X", "", Year))%>%
#   mutate(Year = as.numeric(Year))%>%
#   inner_join(., Hospitalizations[,c("ZIP", "black", "median_income")] , by = "ZIP")
# 
# #Calculate Zipcode Area
# Zips = read_sf("C:/Users/rbuxton/Documents/eBird public  health/Data/Mich ZCTA 2010/tl_2010_26_zcta510.shp")
# Area<- data.frame(ZIP=Zips$ZCTA5CE10, Area=st_area(Zips))%>%
#   mutate(ZIP=as.integer(ZIP))
# 
# Hospitals<-inner_join(Hospitals, Area , by = "ZIP")%>%
#   rename(ZCTA5CE10 = ZIP)
# 
# ###Merge in bird data
# MergedSpRichHosp<-inner_join(BirdSpeciesResults_w, Hospitals, by = c("ZCTA5CE10", "Year"))%>%
#   mutate(black=as.numeric(black))%>%
#   mutate(black = coalesce(black, 0))#ignore warning
# 
# ##Save to avoid rerunning functions
# write.csv(MergedSpRichHosp, "C:/Users/rbuxton/Documents/eBird public  health/Data/FinalDatasetForAnalysis.csv", row.names = FALSE)


#################################################################################################################
################################         GET COVARIATES READY     ###############################################
#################################################################################################################

MergedSpRichHosp<-read.csv("C:/Users/rbuxton/Documents/eBird public  health/Data/FinalDatasetForAnalysis.csv")

###Remove missing data
MergedSpRichHosp<-MergedSpRichHosp[complete.cases(MergedSpRichHosp$median_income),]

#####################################

####Center scale numeric covariates for modelling
MergedSpRichHosp$Income_s<-scale(MergedSpRichHosp$median_income)[,1]
MergedSpRichHosp$Black_s<-scale(as.numeric(MergedSpRichHosp$black))[,1]
MergedSpRichHosp$Area_s<-scale(MergedSpRichHosp$Area)[,1]
MergedSpRichHosp$Year_s<-scale(MergedSpRichHosp$Year)[,1]

MergedSpRichHosp$Observed_SpRich_s<-scale(MergedSpRichHosp$Observed_SpRich)[,1]
MergedSpRichHosp$ChaoEst_SpRich_s<-scale(MergedSpRichHosp$ChaoEst_SpRich)[,1]
MergedSpRichHosp$RarefiedEst_SpRich_s<-scale(MergedSpRichHosp$RarefiedEst_SpRich)[,1]

MergedSpRichHosp$Observed_SpDiv<-log(MergedSpRichHosp$Observed_SpDiv)
MergedSpRichHosp$ChaoEst_SpDiv<-log(MergedSpRichHosp$ChaoEst_SpDiv)
MergedSpRichHosp$RarefiedEst_SpDiv<-log(MergedSpRichHosp$RarefiedEst_SpDiv)

MergedSpRichHosp$Observed_SpDiv_s<-scale(MergedSpRichHosp$Observed_SpDiv)[,1]
MergedSpRichHosp$ChaoEst_SpDiv_s<-scale(MergedSpRichHosp$ChaoEst_SpDiv)[,1]
MergedSpRichHosp$RarefiedEst_SpDiv_s<-scale(MergedSpRichHosp$RarefiedEst_SpDiv)[,1]

###Take out postal codes with fewer than 17 eBird checklists
MergedSpRichHosp_Over17sub<-subset(MergedSpRichHosp, Checklists>=17)

#####################################
##Make a map of total number of checklists by zip
TotalChecklists<-ddply(MergedSpRichHosp, .(ZCTA5CE10), summarize, TotalChecklists=sum(Checklists))
Zips = read_sf("C:/Users/rbuxton/Documents/eBird public  health/Data/Mich ZCTA 2010/tl_2010_26_zcta510.shp")%>%
  mutate(ZCTA5CE10=as.integer(ZCTA5CE10))

TotalChecklists_sp<-left_join(Zips, TotalChecklists, by = "ZCTA5CE10")

##Make a map of number of years with more than 17 checklists
CompleteYears<-ddply(MergedSpRichHosp_Over17sub, .(ZCTA5CE10), summarize, 
                     TotalYearsWith17Checklists=length(Year))

CompleteYears_sp<-left_join(Zips, CompleteYears, by = "ZCTA5CE10")

######MAKE NICE MAPS
# par(mfrow=c(2,2))
# plot(TotalChecklists_sp["TotalChecklists"], main="Total checklists")
# plot(CompleteYears_sp["TotalYearsWith17Checklists"], main="Years with >17 checklists")


A<-ggplot() + 
  geom_sf(data = TotalChecklists_sp,  alpha=0.5, aes(fill = TotalChecklists)) +
  scale_fill_gradient(high = "red", low = "blue")+
  guides(fill=guide_legend(title="Total # checklists"))

B<-ggplot() + 
  geom_sf(data = CompleteYears_sp,  alpha=0.5, aes(fill = TotalYearsWith17Checklists)) +
  scale_fill_gradient(low = "green", high = "yellow")+
  guides(fill=guide_legend(title="Yrs >17 checklists"))

plot_grid(A, B, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

#Merge in NDVI - Review 05/17

#Read in the NDVI data (zonal statistics - mean and sum of NDVI from June 2018)
NDVI_1<-st_read("H:\\NDVI\\NDVI_2018_monthly_JuneJulyAug\\ZonalStats_June_NDVI_2.shp")

#Make final NDVI column from both sides of Michigan
NDVI_1$NDVI<-ifelse(is.na(NDVI_1$NDVImean), NDVI_1$NDVI_2mean, ifelse(
  is.na(NDVI_1$NDVI_2mean), NDVI_1$NDVImean,
  (NDVI_1$NDVIsum+NDVI_1$NDVI_2sum)/(NDVI_1$NDVIcount+NDVI_1$NDVI_2coun)))

NDVI_1<-NDVI_1[c("ZCTA5CE10", "NDVI")]%>%
  mutate(ZCTA5CE10 = as.integer(ZCTA5CE10))

MergedSpRichHosp_Over17sub<-inner_join(MergedSpRichHosp_Over17sub, NDVI_1, by = "ZCTA5CE10")
MergedSpRichHosp_Over17sub$NDVI_s<-scale(MergedSpRichHosp_Over17sub$NDVI)[,1]


cor2018<-subset(MergedSpRichHosp_Over17sub, Year==2018)
cor(cor2018[,c("Observed_SpRich", "Observed_SpDiv", 
                                  "ChaoEst_SpRich", "ChaoEst_SpDiv",
                                  "RarefiedEst_SpRich", "RarefiedEst_SpDiv",
                                  "NDVI")], method="spearman")

###Correlations
cor(MergedSpRichHosp_Over17sub[,c("Observed_SpRich", "Observed_SpDiv", 
                 "ChaoEst_SpRich", "ChaoEst_SpDiv",
                 "RarefiedEst_SpRich", "RarefiedEst_SpDiv",
                 "median_income", "black", "Area", "Checklists", "NDVI")], method="spearman")

#R>0.5 = among all estimates of species richness and diversity
#For checklists - only >0.5 for Chao and observed

cor(MergedSpRichHosp_Over17sub[,c("Observed_SpRich", "Observed_SpDiv", 
                        "ChaoEst_SpRich", "ChaoEst_SpDiv",
                        "RarefiedEst_SpRich", "RarefiedEst_SpDiv",
                        "median_income", "black", "Area")], method="spearman")
  


###Create a global model - compare sp_diversity and sp_richness
m3<-lmer(Anxiety_Mood_Hosp~Income_s+Black_s+Observed_SpRich_s+Area_s+(1|ZCTA5CE10)+(1|Year),data=MergedSpRichHosp_Over17sub, na.action = na.fail)
m4<-lmer(Anxiety_Mood_Hosp~Income_s+Black_s+Observed_SpDiv_s+Area_s+(1|ZCTA5CE10)+(1|Year),data=MergedSpRichHosp_Over17sub, na.action = na.fail)

m5<-glmer(Anxiety_Mood_Hosp~Income_s+Black_s+Observed_SpDiv_s+Area_s+(1|ZCTA5CE10)+(1|Year),data=MergedSpRichHosp_Over17sub, family=poisson, na.action = na.fail)

# m1<-lmer(Anxiety_Mood_Hosp~Income_s+Black_s+ChaoEst_SpRich_s+Area_s+(1|ZCTA5CE10)+(1|Year),data=MergedSpRichHosp_Over17sub, na.action = na.fail)
m2<-lmer(Anxiety_Mood_Hosp~Income_s+Black_s+ChaoEst_SpDiv_s+Area_s+(1|ZCTA5CE10)+(1|Year),data=MergedSpRichHosp_Over17sub, na.action = na.fail)

# m5<-lmer(Anxiety_Mood_Hosp~Income_s+Black_s+RarefiedEst_SpRich_s+Area_s+(1|ZCTA5CE10)+(1|Year),data=MergedSpRichHosp_Over17sub, na.action = na.fail)
m6<-lmer(Anxiety_Mood_Hosp~Income_s+Black_s+RarefiedEst_SpDiv_s+Area_s+(1|ZCTA5CE10)+(1|Year),data=MergedSpRichHosp_Over17sub, na.action = na.fail)

m7<-lmer(Anxiety_Mood_Hosp~Income_s+Black_s+Observed_SpDiv_s+Area_s+(1|ZCTA5CE10)+(1|Year),data=MergedSpRichHosp_Over17sub, na.action = na.fail)

m8<-lmer(Anxiety_Mood_Hosp~Income_s+Black_s+Observed_SpDiv_s+Area_s+NDVI_s+(1|ZCTA5CE10)+(1|Year),data=MergedSpRichHosp_Over17sub, na.action = na.fail)
m9<-lmer(Anxiety_Mood_Hosp~Income_s+Black_s+Observed_SpDiv_s+Area_s+NDVI_s+(1|ZCTA5CE10)+(1|Year),data=MergedSpRichHosp_Over17sub, na.action = na.fail)

AIC(m3,m4,m8, m9) #m8 lower AIC - choose this one

source("C:\\Users\\rbuxton\\Documents\\eBird public  health\\Code\\MakeNiceTable_04.R")
Results<-Makenicetable(m8)
write.csv(Results, "C:\\Users\\rbuxton\\Documents\\eBird public  health\\ModelResultsTable.csv", row.names = FALSE)

#R squared
r.squaredGLMM(m8)
# r.squaredGLMM(m2)
# r.squaredGLMM(m6)

# #hectares 
# summary(MergedSpRichHosp_Over17sub$Area*0.0001)
# MergedSpRichHosp_Over17sub$Area_ha<-MergedSpRichHosp_Over17sub$Area*0.0001
# 
# #Take the third quartile
# MergedSpRichHosp_Over17sub_city<-subset(MergedSpRichHosp_Over17sub, Area_ha<=24125)
# 
# m9<-lmer(Anxiety_Mood_Hosp~Income_s+Black_s+Observed_SpDiv_s+Area_s+(1|ZCTA5CE10)+(1|Year),data=MergedSpRichHosp_Over17sub_city, na.action = na.fail)
# m10<-lmer(Anxiety_Mood_Hosp~Income_s+Black_s+Observed_SpDiv_s+Area_s+NDVI_s+(1|ZCTA5CE10)+(1|Year),data=MergedSpRichHosp_Over17sub_city, na.action = na.fail)
# 
# #no longer significant
# 
# #Smaller than 5000 ha
# MergedSpRichHosp_Over17sub_city<-subset(MergedSpRichHosp_Over17sub, Area<=4000000)
# 
# m11<-lmer(Anxiety_Mood_Hosp~Income_s+Black_s+Observed_SpDiv_s+(1|ZCTA5CE10)+(1|Year),data=MergedSpRichHosp_Over17sub_city, na.action = na.fail)
# m12<-lmer(Anxiety_Mood_Hosp~Income_s+Black_s+Observed_SpDiv_s+NDVI_s+(1|ZCTA5CE10)+(1|Year),data=MergedSpRichHosp_Over17sub_city, na.action = na.fail)
# 
# confint(m11)
# confint(m12)


# MergedSpRichHosp_Over17sub$Predval <- predict(m4,type="response")

##Plots

plot_model(m8, type = "pred", terms = "Observed_SpDiv_s", 
           axis.title = c("Shannon diversity", "Hospitalizations"),
           axis.lim	= list(c(-3,2), c(7,12)))

ggplot(MergedSpRichHosp_Over17sub, aes(Observed_SpDiv_s, Anxiety_Mood_Hosp)) +
  geom_point()+
  geom_smooth(method = lm)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # this removes the linse in the back.
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))

ggplot(MergedSpRichHosp_Over17sub, aes(Observed_SpDiv, Predval)) +
  geom_point()+
  geom_smooth(method = lm)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # this removes the linse in the back.
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  labs(x = "Shannon diversity", y = "Predicted hospitalizations")+
  theme(axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=12,face="bold"))

# ggplot(MergedSpRichHosp, aes(Completed_lists, Income)) +
#   geom_point()+
#   geom_smooth(method = lm)+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # this removes the linse in the back.
#         panel.background = element_blank(), axis.line = element_line(colour = "grey"))
