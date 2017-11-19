### WPP BIODIVERSITY ANALYSIS - MAIN
### Programmer: Kevin Wolz
### Originally Created: 20 Nov 2014

## Required packages
library(tidyverse)
library(lubridate)
library(DeLuciatoR)

## File Parameters
outputDataPath <- './output/data/'
outputPlotPath <- './output/plots/'
orderPlotPath  <- './output/plots/orders/'
inputDataPath  <- './raw_data/'

dir.create(outputDataPath, showWarnings = FALSE, recursive=TRUE)
dir.create(outputPlotPath, showWarnings = FALSE, recursive=TRUE)
dir.create(orderPlotPath, showWarnings = FALSE, recursive=TRUE)
dir.create(inputDataPath,  showWarnings = FALSE, recursive=TRUE)

## Run Analysis Scripts
source("WPP_Biodiversity_AERIALS.R")
source("WPP_Biodiversity_BOWLS.R")
source("WPP_Biodiversity_PITFALLS.R")
source("WPP_Biodiversity_MICROBES.R")
