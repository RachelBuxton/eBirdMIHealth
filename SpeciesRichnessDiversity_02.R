

BirdRichnessDiversity<-function(ALLYEARS){

#LOOP TO CALCULATE SPECIES DIVERSITY AND RICHNESS
BirdSpecies<-lapply(1:length(ALLYEARS), function(ss){
  
ALLYEARS_1<-ALLYEARS[ss]

Checklists<-ALLYEARS_1[[1]][[1]]%>%
group_by(ZCTA5CE10) %>% summarise(Checklists = length(unique(SAMPLING_EVENT_IDENTIFIER))) 

  
##Chao
Chao<-ALLYEARS_1[[1]][[1]] %>% 
  dplyr::select(COMMON_NAME, OBSERVATION_COUNT, SAMPLING_EVENT_IDENTIFIER, ZCTA5CE10) %>% 
  group_by(ZCTA5CE10, COMMON_NAME) %>% 
  # mutate(OBSERVATION_COUNT=1)%>% #if you want 'incidence frequency in iNEXT'
  summarise(TotalCount = sum(OBSERVATION_COUNT)) %>% 
  spread(COMMON_NAME, TotalCount) %>%# spread the columns to a matrix form
  replace(is.na(.), 0) %>%
  # inner_join(Checklists , .,  by = "ZCTA5CE10")%>%
  # mutate(Checklists = as.numeric(Checklists))%>%
  # filter(Checklists > 1)%>%
  
  # split(data, seq(nrow(.)))
  rotate_df() %>% #transpose dataframe
  set_names(unlist(.[1,])) %>% #set postal code as column name
  slice(-1)
  
# Chao<-lapply(Chao,function(x) x[x!=0])
# out1 <- iNEXT(Chao[1:10], q=c(0,1,2), datatype="incidence_freq")

test2<- iNEXT(Chao, datatype = "abundance", conf = 0.95, q=c(0,1), size=17)

#Chao richness and diversity
results<- subset(test2$AsyEst, Diversity=="Species richness"|Diversity=="Shannon diversity")
  
#Rarefied species richness and diversity
results2<-subset(test2$iNextEst$size_based, m==17)[,c("Assemblage", "Order.q", "qD", "qD.LCL", "qD.UCL")] 
results2$Diversity<-ifelse(results2$Order.q==0, "Species richness", "Shannon diversity")
results2<-results2[,-2]

resultsFinal<-inner_join(results, results2, by=c("Assemblage", "Diversity"))%>% 
  rename(ZCTA5CE10 = Assemblage)%>%
  mutate(ZCTA5CE10 = as.integer(ZCTA5CE10))%>%
  inner_join(., Checklists , by = "ZCTA5CE10")%>%
mutate(Year=unique(ALLYEARS_1[[1]][[1]]$YEAR))

resultsFinal

})

BirdSpeciesResults<-do.call("rbind", BirdSpecies)
  
return(BirdSpeciesResults)

}







