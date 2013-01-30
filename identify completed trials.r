## R script for the initial analysis of the clinicaltrials.gov dataset
# ClinicalTrials.gov was searched for all interventional studies on 
# 19th Jan 2011.  Filename = "clinicaltrials.gov_search.txt. 
# This file is large (70mb).  The bottom of the file was edited to
# ensure that R reads it in correctly.

rm(list=ls(all=TRUE))
# remember to set your working directory prior to downloading the file!
data.orig <- read.delim("clinicaltrials.gov_search.txt",
                        header = T,
                        stringsAsFactors = F)
data1 <- data.orig

## Initially the dataset needs the missing values converted into 
# the format required by R
data1$Completion.Date[which(data1$Completion.Date == "")] <- NA
data1$Primary.Completion.Date[which(data1$Primary.Completion.Date == "")] <- NA
data1$Phases[which(data1$Phases == "")] <- NA
data1$Phases <- as.factor(data1$Phases)
table(data1$Phases, useNA = "ifany")

## Ensure that the dates are formatted corretly:-
data1$Primary.Completion.Date <- as.Date(data1$Primary.Completion.Date, format = "%d/%m/%Y")
data1$Completion.Date <- as.Date(data1$Completion.Date, format = "%d/%m/%Y")
# if a "primary completion date" is missing, copy the value from "Completion date"
data1$Primary.Completion.Date[which(is.na(data1$Primary.Completion.Date))] <- data1$Completion.Date[which(is.na(data1$Primary.Completion.Date))]

## sort out the ones with any completion date
# and put into a datafram called completed
completed <- data1[which(!is.na(data1$Primary.Completion.Date)), ]
completed <- completed[which(completed$Recruitment == "Completed"), ]

## sort out from these the ones which completed in 2009
# store in the dataframe completed.2009
completed.2009 <- completed[which(
            (completed$Primary.Completion.Date >= as.Date("01/01/2009", 
                                         format = "%d/%m/%Y")) &
            (completed$Primary.Completion.Date < as.Date("01/01/2010", 
                                         format = "%d/%m/%Y")) ), ]
## We need to exclude phase 0 and I studies.  
# store these in completed.2009.II 
completed.2009.II <-  completed.2009
completed.2009.II <-  completed.2009.II[-which(completed.2009.II$Phases == "Phase 0"), ]
completed.2009.II <-  completed.2009.II[-which(completed.2009.II$Phases == "Phase I"), ]
table(completed.2009.II$Phases, useNA = "ifany")
nrow(completed.2009.II)

## Also remove the studies which don't record the study phase
# store in completed.2009.II.noNA
completed.2009.II.noNA <- completed.2009.II[which(!is.na(completed.2009.II$Phases)), ]
nrow(completed.2009.II.noNA)
table(completed.2009$Phases, useNA = 'ifany')
table(completed.2009.II.noNA$Phases)

## webscrape the location of the study - warning - takes some time!!
# the motivation for this is that the FDAAA only applies to US studies
# please note that if ClinicalTrials.gov change the layout of the
# table view of each trial record that this will no-longer work
# a random sample of 20 records were manually checked to ensure that
# the script was in-fact downloading the right information for
# each trial
study.location <- rep(NA, nrow(completed.2009.II.noNA))
for (n in 3249:length(study.location)){
    newURL <- paste("http://clinicaltrials.gov/ct2/show/record/", completed.2009.II.noNA$NCT.ID[n], sep = "")
    webpage <- readLines(newURL)
    write(webpage, "page.html")
    location_line <- grep("Location Countries", webpage)
    countries <- webpage[location_line + 1]
    # the field of interest is in the web page on the line below
    # the line which contains the string "Location Countries"
    # and the string containing the list of countries is in between the 
    # < and > symbols.
    countries <- strsplit(countries, ">")[[1]][2]
    countries <- strsplit(countries, "<")[[1]][1]
    study.location[n] <- countries
    print(n)  # to update you regarding how
    flush.console()
    }

print(tail(study.location))

completed.2009.II.noNA.loc <- cbind(completed.2009.II.noNA, study.location)
## it's worthwhile to save this after the webscrape as the webscrape takes some time
write.csv(completed.2009.II.noNA.loc, "output_with_location.csv") 

## identify which studies are done in the US
USlist <- grep("United States", completed.2009.II.noNA.loc$study.location) 
# gives the row numbers of the studies
completed.2009.II.noNA.US <- completed.2009.II.noNA.loc[USlist ,]

## final thing to do is exclude trials which are not drugs, devices or biologicals
# first thing get a list of the rows with 'Drug:', 'Device:' or 'Biological:' 
# in the 'Interventions' variable
# regular expressions can be used for this
drug <- grep("Drug:", completed.2009.II.noNA.US$Interventions, ignore.case = F, invert = F)
device <- grep("Device:", completed.2009.II.noNA.US$Interventions, ignore.case = F, invert = F)
biological <- grep("Biological:", completed.2009.II.noNA.US$Interventions, ignore.case = F, invert = F)
include <- c(drug, device, biological)  # this is the list of records which are of a drug, device or biological
include <- include[order(include)]
# 1531, BUT there are some interventions which have both
# a device and a biological in them, so we just need each row once:-
include <- unique(include)
length(include)
completed.2009.II.noNA.US_include <- completed.2009.II.noNA.US[include, ]
write.csv(completed.2009.II.noNA.US_include, "included_trials.csv")
## "included_trials.csv" is the 
