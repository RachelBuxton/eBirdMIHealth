

Makenicetable<-function(m8){
##MAKE A NICE TABLE

CI<-data.frame(confint(m8))
CI$Covariates<-row.names(CI)

Summary<-data.frame(summary(m8)$coefficients)
Summary$Covariates<-row.names(Summary)

CI<-left_join(Summary,CI, by="Covariates")

#Clear covariate and column names
CI$Covariates<-gsub("_s", "", CI$Covariates)
colnames(CI)[which(colnames(CI)=="X2.5.."|colnames(CI)=="X97.5..")]<-c("CI2.5", "CI97.5")

#Round
Results<-select_if(CI, is.numeric)%>%
  round(2)

Results$ParameterEstimateSE<-paste(Results[,"Estimate"], "\u00b1", Results[,"Std..Error"], sep=" ")
Results$Covariates<-CI$Covariates

Results<-Results[,c("Covariates","ParameterEstimateSE", "CI2.5", "CI97.5")]
  
return(Results)
}