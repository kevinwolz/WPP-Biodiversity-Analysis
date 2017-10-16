### INSECTS LIBRARY
### Programmer: Kevin Wolz
### Originally Created: 5 Jan 2015
### Last Updated: 5 Jan 2015

if(LIBRARY){

  ## READ IN RAW COUNT DATA THAT HAS BEEN COMPILED INTO A SINGLE FILE
  count.data = read.csv(paste(inputDataPath, "Insect_Data_Compiled.csv", sep=""), header = TRUE)
  
  ## READ IN ESTIMATES OUTPUT DATA
  alpha.data = read.csv(paste(inputDataPath, "EstimateS_Diversity_Output_", yr, ".csv", sep=""), header = TRUE)
  beta.data = read.csv(paste(inputDataPath, "EstimateS_SharedSpecies_Output_", yr, ".csv", sep=""), header = TRUE)
  
  count.pitfall = subset(count.data, metric=="pitfall")
  alpha.pitfall = subset(alpha.data, metric=="pitfall")
  beta.pitfall = subset(beta.data, metric=="pitfall")

}