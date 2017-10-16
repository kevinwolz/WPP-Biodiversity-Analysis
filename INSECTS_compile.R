### INSECTS COMPILE
### Programmer: Kevin Wolz
### Originally Created: 20 Nov 2014
### Last Updated: 6 Dec 2014

if(COMPILE){
  
may.2013 <- read.csv(paste(inputDataPath, "orig_monthly_data/may_2013.csv", sep=""), header = TRUE)
jun.2013 <- read.csv(paste(inputDataPath, "orig_monthly_data/jun_2013.csv", sep=""), header = TRUE)
jul.2013 <- read.csv(paste(inputDataPath, "orig_monthly_data/jul_2013.csv", sep=""), header = TRUE)
aug.2013 <- read.csv(paste(inputDataPath, "orig_monthly_data/aug_2013.csv", sep=""), header = TRUE)
sep.2013 <- read.csv(paste(inputDataPath, "orig_monthly_data/sep_2013.csv", sep=""), header = TRUE)

apr.2014 <- read.csv(paste(inputDataPath, "orig_monthly_data/apr_2014.csv", sep=""), header = TRUE)
may.2014 <- read.csv(paste(inputDataPath, "orig_monthly_data/may_2014.csv", sep=""), header = TRUE)
jun.2014 <- read.csv(paste(inputDataPath, "orig_monthly_data/jun_2014.csv", sep=""), header = TRUE)
jul.2014 <- read.csv(paste(inputDataPath, "orig_monthly_data/jul_2014.csv", sep=""), header = TRUE)
aug.2014 <- read.csv(paste(inputDataPath, "orig_monthly_data/aug_2014.csv", sep=""), header = TRUE)
sep.2014 <- read.csv(paste(inputDataPath, "orig_monthly_data/sep_2014.csv", sep=""), header = TRUE)

## Bind togeter all monthly data frames, adding in NAs to data frame rows where columns weren't before
#out1 <- smartbind(may.2013, jun.2013, jul.2013, aug.2013, sep.2013, apr.2014, may.2014, jun.2014, jul.2014, aug.2014, sep.2014)
out <- rbind.fill(may.2013, jun.2013, jul.2013, aug.2013, sep.2013, apr.2014, may.2014, jun.2014, jul.2014, aug.2014, sep.2014)

#sum(as.matrix(may.2013[,15:ncol(may.2013)]), na.rm = TRUE)+sum(as.matrix(jun.2013[,15:ncol(jun.2013)]), na.rm = TRUE)+sum(as.matrix(jul.2013[,15:ncol(jul.2013)]), na.rm = TRUE)+sum(as.matrix(aug.2013[,15:ncol(aug.2013)]), na.rm = TRUE)+sum(as.matrix(sep.2013[,15:ncol(sep.2013)]), na.rm = TRUE)+sum(as.matrix(apr.2014[,15:ncol(apr.2014)]), na.rm = TRUE)+sum(as.matrix(may.2014[,15:ncol(may.2014)]), na.rm = TRUE)+sum(as.matrix(jun.2014[,15:ncol(jun.2014)]), na.rm = TRUE)+sum(as.matrix(jul.2014[,15:ncol(jul.2014)]), na.rm = TRUE)+sum(as.matrix(aug.2014[,15:ncol(aug.2014)]), na.rm = TRUE)+sum(as.matrix(sep.2014[,15:ncol(sep.2014)]), na.rm = TRUE)
#sum(as.matrix(out1[,15:ncol(out1)]), na.rm = TRUE)
#sum(as.matrix(out[,15:ncol(out)]), na.rm = TRUE)



## Sort columns with insect data in order by the identifier that is the column title
INSECTS.raw = names(out)[15:length(names(out))]
insects = data.frame(letter = substr(INSECTS.raw,1,1), number = as.numeric(substring(INSECTS.raw, 2, nchar(INSECTS.raw))))
insects.sorted = insects[order(insects$letter, insects$number),]
INSECTS = paste(insects.sorted$letter, insects.sorted$number, sep = "")
out.sorted = cbind(out[,1:14],out[,INSECTS])

## Replace all NAs (which were originally blanks, but read in as NAs) in the insect data (not in the identifier data) with 0's (that's waht EstimateS requires for missing samples)
insect.data = out.sorted[,15:ncol(out.sorted)]
insect.data[is.na(insect.data)] <- 0
out.sorted[,15:ncol(out.sorted)] = insect.data

## Make a few rows of BAD pitfall traps NAs
out.sorted[which(out.sorted$year==2013 & out.sorted$month==7 & out.sorted$id %in% c(23,33,41,51)), 15:ncol(out.sorted)] <- NA

## Write csv of compiled and sorted data
write.table(out.sorted, file = paste(inputDataPath, "Insect_Data_Compiled.csv", sep=""), sep = ",", row.names = FALSE)

}