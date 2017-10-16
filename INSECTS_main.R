### INSECTS MAIN
### Programmer: Kevin Wolz
### Originally Created: 20 Nov 2014
### Last Updated: 20 Nov 2014

startTime = proc.time()[3] # Start timer

## USER PARAMETERS
setwd("~/Desktop/RESEARCH/UIUC_WPP/RESEARCH/Field_Data/Biodiversity/Insects")
site = 'WPP'
yr = 2014

## Required packages
#library(gtools) #for smartbind function in compile script
library(plyr) #for rbind.fill function in compile script
library(doBy) #for stats/summarization
#library(lattice) #for plots
library(plotrix) #for plots


## File Parameters
outputDataPath = './output/data/'
outputPlotPath = './output/plots/'
inputDataPath  = './raw_data/'
outputCountPlotPath = './output/plots/count_data/' 

dir.create(outputDataPath,showWarnings=FALSE,recursive=TRUE)
dir.create(outputPlotPath,showWarnings=FALSE,recursive=TRUE)
dir.create(outputCountPlotPath,showWarnings=FALSE,recursive=TRUE)
dir.create(inputDataPath,showWarnings=FALSE,recursive=TRUE)

INSECTS.compile = 'INSECTS_compile.R'
INSECTS.library = 'INSECTS_library.R'
INSECTS.analysis = 'INSECTS_analysis.R'

## Toggle
COMPILE = FALSE #this should theoretically never be used again
LIBRARY = TRUE
ANALYSIS = TRUE

## Analyze Insect Data
source(INSECTS.compile)
source(INSECTS.library)
source(INSECTS.analysis)


elapsedTime = proc.time()[3] - startTime 
print(elapsedTime)