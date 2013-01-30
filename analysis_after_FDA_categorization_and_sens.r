## script which performs the analyses in the paper
# "FDA_table_with_sens.csv" is the dataset of 
# all trials which are of a drug, device or biological, took place in the US,
# are phase II or higher, and completed within 2009. 
# the 1465 trials in this dataset are the output from
# the "identify completed trials.r" script.  This file was then cross referenced manually with 
# the Drugs@FDA database to give the "FDA_table_with_sens.csv" file
# the sensitivity analysis is also within this file

# The field FDA_app is 1 if we could find an approved drug, device or biological,
# which matched those used in the trial in Drugs@FDA, and that drug, device or biological
# was approved at least 2 months prior to the time of the search in Jan 2011.  

rm(list = ls())
# first import the dataset 
FDA_table <- read.csv("FDA_table_with_sens.csv", stringsAsFactors = F,
                      header = T)


######################################################################################
#  ANALYSISSTARTS HERE

# FIRST THING TO DO IS DEFINE THE PRIMARY POPULATION

FDA_table$prim_pop <- 0
length(which(FDA_table$FDA_app == 1)) # this is the 739 trials with FDA app drug
FDA_table$prim_pop[which(FDA_table$FDA_app == 1)] <- 1
# now prim_pop can be used to subgroup the 1465 trials into primary population
# or otherwise

######################################################################################
#Analysis 1 - what is the proportion of trials with results:
analysis1     <- table(FDA_table$prim_pop, FDA_table$Study.Results)
rownames(analysis1) <- c("NotFDAapp", "FDAapp")
print(analysis1)
analysis1mar  <- addmargins(analysis1)
print(analysis1mar)
analysis1mar <- cbind(analysis1mar, analysis1mar[, 1] / analysis1mar[, 3] * 100)
print(analysis1mar)

######################################################################################
#Analysis 2 - compare prim_pop = 1 with prim_pop == 0
analysis2 <- analysis1
print(analysis2)
(prop.test(analysis2)$estimate[2] - prop.test(analysis2)$estimate[1]) * 100
prop.test(analysis2)$conf.int * 100
chisq.test(analysis2)

#####################################################################################
# Analysis 3 - effect of phase on reporting (primary study population)
library(car)

# make a new dataframe of the primary study population
prim_pop <- FDA_table[which(FDA_table$prim_pop == 1), ]
table(prim_pop$Phases)
prim_pop$Phases.recoded <- prim_pop$Phases
prim_pop$Phases.recoded <-recode(prim_pop$Phases.recoded, 
                                "'Phase I|Phase II'  = 'Phase II';
                                 'Phase II|Phase III' = 'Phase III'")
table(prim_pop$Phases.recoded)

analysis3 <- table(prim_pop$Phases.recoded, prim_pop$Study.Results)
analysis3mar <- addmargins(analysis3)
analysis3mar <- cbind(analysis3mar, (analysis3mar[, 1]/ analysis3mar[, 3] * 100))
print(analysis3mar)
chisq.test(analysis3)
prop.test(analysis3)

#####################################################################################
# Analysis 4 - effect of funder on reporting (primary study population)
table(prim_pop$Funded.Bys)
prim_pop$Funded.recoded.4 <- as.character(prim_pop$Funded.Bys) # recodes into 4 catgories
prim_pop$Funded.recoded.4 <- recode(prim_pop$Funded.recoded.4,
                        "'Industry' = 'Industry'; 
                        c('NIH', 'U.S. Fed') = 'NIH/Government';
                        'Other' = 'Other';
                        else = 'Mixed'")
table(prim_pop$Funded.recoded.4)

analysis4 <- table(prim_pop$Funded.recoded.4, prim_pop$Study.Results)
analysis4mar <- addmargins(analysis4)
analysis4mar <- cbind(analysis4mar, (analysis4mar[, 1]/ analysis4mar[, 3] * 100))
print(analysis4mar)
chisq.test(analysis4)
fisher.test(analysis4)
prop.test(analysis4)


####################################################################################
# analysis 4a - effect of sole industry vs not sole industry
prim_pop$Funded.recoded.2 <- as.character(prim_pop$Funded.Bys) # recodes into 2 catgories
prim_pop$Funded.recoded.2 <- recode(prim_pop$Funded.recoded.2,
                        "'Industry' = 'Sole Industry';
                        else = 'Not Sole Industry'")
table(prim_pop$Funded.recoded.2)

analysis4a <- table(prim_pop$Funded.recoded.2, prim_pop$Study.Results)
analysis4amar <- addmargins(analysis4a)
analysis4amar <- cbind(analysis4amar, (analysis4amar[, 1]/ analysis4amar[, 3] * 100))
print(analysis4amar)
chisq.test(analysis4a)
prop.test(analysis4a)$conf.int * 100

######################################################################################
#  sensitivity analysis
table(prim_pop$Brand_name_in_trial)
sens_tab <- table(prim_pop$Brand_name_in_trial, prim_pop$Study.Results, useNA = "ifany")
rownames(sens_tab) <- c("unbranded", "branded")
sens_tab <- addmargins(sens_tab)
sens_tab <- cbind(sens_tab, sens_tab[,1] / sens_tab[, 3] * 100)
print(sens_tab)

