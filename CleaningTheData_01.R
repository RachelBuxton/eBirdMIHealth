######### Bird diversity and human health project
######### RACHEL BUXTON
######### 11.21.2022
######### THIS FUNCTION CREATES A LIST OF MATRICES FOR EACH YEAR (2008-2018)
######### WITH SPECIES FROM EBIRD CHECKLISTS. THE FIRST LIST IS ALL SPECIES
######### THE SECOND HAS REMOVED SPECIES ONLY OBSERVED IN <5% OF CHECKLISTS AT A LOCATION
######### ZIPCODES HAVE BEEN DETERMINED USING QGIS

####Two csv folders created by Hsien-Yung
####One file for each year - only want 2008-2018 (start at file 9)

#QGIS details
######################## use QGIS to joint zipcode and checklists instead #######################################
# 3.1 how many checklists per zipcode zone
# vector -> analysis tools -> count points in polygon 
# 3.2 assign ZCTA 2010 to ebird salmping events
# processing -> toolbox -> vector general -> join attribute by location 

##ZIP CODES
# Michigan<-read_sf("C:/Users/rbuxton/Documents/eBird public  health/Data/Mich ZCTA 2010/tl_2010_26_zcta510.shp")
# plot(st_geometry(Michigan))

SpeciesMatrices<-function(ALLYEARS_list){
  
  # 
  # list.files("eBird Michigan 2000-2018 sampling event") #Distinct sampling events matched with Zipcodes in QGIS
  # 
  
  #CREATE AN LAPPLY LOOP THROUGH EACH YEAR - output a list of 2 matrixes: 1 with all sp, one eliminating rare (<5% of site)
  #species
  
  ALLYEARS<-lapply(9:length(ALLYEARS_list), function(ss){
    ###########Massage data and choose best quality data based on our predefined filters
    
    BV<-read.csv(ALLYEARS_list[ss])
    
    #Convert column names to match Callaghan
    colnames(BV)<-toupper(colnames(BV))
    
    # Format Date
    BV$OBSERVATION_DATE <- as.Date(BV$OBSERVATION_DATE, format = "%Y-%m-%d")
    
    # add year to the dataframe
    BV$YEAR <- year(BV$OBSERVATION_DATE)
    
    # add all the columns needed for the analysis (that don't vary within checklist)
    sampling_event_info <- BV %>% dplyr::select(SAMPLING_EVENT_IDENTIFIER, LOCALITY, LOCALITY_ID, 
                                         OBSERVATION_DATE, PROTOCOL_TYPE, ALL_SPECIES_REPORTED, EFFORT_DISTANCE_KM, 
                                         EFFORT_AREA_HA, DURATION_MINUTES, YEAR, GROUP_IDENTIFIER, LATITUDE, LONGITUDE) %>% 
      distinct()
    
    
    # Counts how many 'x's per checklist
    X_missing <- BV %>% group_by(SAMPLING_EVENT_IDENTIFIER) %>% summarise(number_X = sum(OBSERVATION_COUNT == "X"))
    
    # accounts for the instance in which people submit the same species both at
    # the species and subspecies level also makes it so only 'species' and
    # 'issf' category are included in analysis
    BV_clean <- BV %>% filter(CATEGORY %in% c("species", "issf")) %>% ##Only species and issf included
      group_by(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME) %>% ##Group things by sampling event identifier and common name
      summarise(COUNT_SPP = sum(as.numeric(as.character(OBSERVATION_COUNT)))) %>% 
      rename(OBSERVATION_COUNT = COUNT_SPP) %>% inner_join(., sampling_event_info, 
                                                           by = "SAMPLING_EVENT_IDENTIFIER") %>% inner_join(., X_missing, by = "SAMPLING_EVENT_IDENTIFIER")
    
    #IGNORE ERRORS
    
    ################################################## apply some basic criteria to filter by #####
    
    # apply some filtering criteria
    analysis_data <- BV_clean %>% ## filter for only complete checklists
      filter(ALL_SPECIES_REPORTED == 1) %>% ## only using stationary, traveling, and exhaustive area type checklists
      filter(PROTOCOL_TYPE %in% c("eBird - Exhaustive Area Count", "eBird - Stationary Count", 
                                  "eBird - Traveling Count", "Traveling", "Stationary", "Area")) %>% ## Get rid of any checklists that had a single X
      filter(number_X == 0) %>% 
      group_by(SAMPLING_EVENT_IDENTIFIER) %>% 
      summarise(Species_Richness = length(unique(COMMON_NAME)), Species_Diversity = diversity(OBSERVATION_COUNT), 
                Species_Abundance = sum(OBSERVATION_COUNT,  na.rm = TRUE), Minutes = mean(DURATION_MINUTES, na.rm = TRUE), 
                Distance_km = mean(EFFORT_DISTANCE_KM,  na.rm = TRUE), Area_ha = mean(EFFORT_AREA_HA, na.rm = TRUE)) %>% 
      inner_join(BV_clean, ., by = "SAMPLING_EVENT_IDENTIFIER")
    
    ######################################################## filtering out group_identifier data to eliminate 'duplicated' checklists
    
    # first select the group_identifiers and associated checklists
    duplicated <- analysis_data %>% drop_na(GROUP_IDENTIFIER) %>% 
      dplyr::select(GROUP_IDENTIFIER, SAMPLING_EVENT_IDENTIFIER) %>% 
      distinct(.keep_all = TRUE) %>% group_by(GROUP_IDENTIFIER) %>% 
      # randomly sample one checklist for each group_identifier
      sample_n(., 1) %>% .$SAMPLING_EVENT_IDENTIFIER
    
    duplicated_data <- analysis_data %>% filter(SAMPLING_EVENT_IDENTIFIER %in% duplicated)
    
    ## now, append the selected checklists for each group_identifier with the non
    ## group_identifier checklists from the data
    analysis_data <- analysis_data %>% filter(!grepl("G", GROUP_IDENTIFIER)) %>% 
      bind_rows(., duplicated_data)
    
    
    ######################################################### apply distance and duration caps 
    analysis_data <- analysis_data %>% filter(DURATION_MINUTES >= 5 & DURATION_MINUTES <= 
                                                240) %>% filter(EFFORT_DISTANCE_KM <= 10)
    
    
    ## rename analysis_data to signify it is the 'complete' checklist usage
    analysis_data.all <- analysis_data
    
    ########################################################################Join in Zipcode
    
    ##TAKES WAY TOO LONG
    # ##Read in Zipcodes
    # Zips = read_sf("C:/Users/rbuxton/Documents/eBird public  health/Data/Mich ZCTA 2010/tl_2010_26_zcta510.shp")
    # 
    # # plot(Zips[2])
    # 
    # #Make lat.long of checklist into spatial object
    # pnts_checklists <- st_as_sf(analysis_data.all, coords = c('LONGITUDE', 'LATITUDE'), crs = st_crs(Zips))
    # # plot(pnts_checklists$geometry, add=TRUE)
    # 
    # start_time <- Sys.time()
    # #Get zipcodes that intersect with checklists
    # pnts <- pnts_checklists %>% mutate(
    #   intersection = as.integer(st_intersects(geometry, Zips)), ZipCode = if_else(is.na(intersection), '', Zips$ZCTA5CE10[intersection]),
    #   Area=if_else(is.na(intersection), '', Zips$ZCTA5CE10[intersection])) 
    # end_time <- Sys.time()
    # end_time - start_time
    
    list2000<-read.csv(list.files("eBird Michigan 2000-2018 sampling event", full.names = TRUE)[ss])
    colnames(list2000)<-toupper(colnames(list2000))
    
    Zips<-list2000%>%dplyr::select( LATITUDE, LONGITUDE, ZCTA5CE10) %>% distinct()
    
    analysis_data.all<- analysis_data.all%>%
      inner_join( ., Zips, by = c("LATITUDE", "LONGITUDE"))
    
    
    ###################################################################### get rid of species which did not occur on >95% of checklists ####
    
    ## Exlude the species that rarely occur
    checklists_hotspots <- analysis_data.all %>% group_by(LOCALITY_ID) %>% summarise(total_checklists = length(unique(SAMPLING_EVENT_IDENTIFIER)))
    
    ## create a dataframe which removes the species that are on <=5% of
    ## checklists in a hotspot
    analysis_data.95 <- analysis_data.all %>% group_by(LOCALITY_ID, COMMON_NAME) %>% 
      summarise(species_count = length(COMMON_NAME)) %>% 
      inner_join(checklists_hotspots, ., by = "LOCALITY_ID") %>% 
      mutate(percentage_of_checklists = (species_count/total_checklists) * 100) %>% 
      inner_join(analysis_data.all, ., by = c("LOCALITY_ID", "COMMON_NAME")) %>% 
      filter(percentage_of_checklists >= 5)  ## removing species that are on < 5% of checklists in a hotspot
    
    
    
    ## Clean up workspace
    rm(list = setdiff(ls(), c("analysis_data.all", "analysis_data.95")))
    
    list(analysis_data.95, analysis_data.all)
    
  })
  
  return(ALLYEARS)
}


