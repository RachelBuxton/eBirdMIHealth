

ReshapeFunction<-function(BirdRich, Hospitalizations){
###Reshape the datasets

##Species richness/diversity first
BirdRich_melted <- cbind(melt(BirdRich[,c(1,grep("Sp_diversity", colnames(BirdRich)))], id="ZipcodeZones", value.name="Sp_Diversity"),
                         melt(BirdRich[,c(1,grep("Sp_Richness", colnames(BirdRich)))], id="ZipcodeZones", value.name="Sp_Richness")[,3],
                         melt(BirdRich[,c(1,grep("completed_lists", colnames(BirdRich)))], id="ZipcodeZones", value.name="Completed_lists")[,3])

BirdRich_melted<-merge(BirdRich_melted,BirdRich[,c(1,35)],by="ZipcodeZones")

colnames(BirdRich_melted)[c(1,2,4:5)]<-c("ZIP","Year","Sp_Richness", "Completed_lists")
BirdRich_melted$Year<-sapply(strsplit(as.character(BirdRich_melted$Year), "_"), "[", 3)
BirdRich_melted$Year<-as.numeric(BirdRich_melted$Year)

##Then hospitalizations
Hospitalizations_melted <- melt(Hospitalizations[,c(1,grep("X20", colnames(Hospitalizations)))], id="ZIP", value.name="Anxiety_Mood_Hosp")
Hospitalizations_melted$variable<-gsub("X", "", Hospitalizations_melted$variable)
Hospitalizations_melted$variable<-as.numeric(Hospitalizations_melted$variable)
colnames(Hospitalizations_melted)[2]<-c("Year")
Hospitalizations_melted<-cbind(Hospitalizations_melted,
                               melt(Hospitalizations[,c(1,grep("median_income", colnames(Hospitalizations)))], id="ZIP", value.name="Income")[,3],
                               melt(Hospitalizations[,c(1,grep("black", colnames(Hospitalizations)))], id="ZIP", value.name="Income")[,3])
colnames(Hospitalizations_melted)[4:5]<-c("Income", "Prop_black")

##Merge data frames
MergedSpRichHosp<-merge(BirdRich_melted, Hospitalizations_melted, by=c("ZIP", "Year"))
MergedSpRichHosp$Prop_black<-as.numeric(as.character(MergedSpRichHosp$Prop_black))

return(MergedSpRichHosp)
}