install.packages("tableone")
library(Gmisc)
library(dplyr)
library(tidyr)
library(tableone)

setwd("C:/Users/Jacky C/Desktop/SINGLE_TASK/chiayi")
chiayi <- read.csv(pathJoin("(v2)STROKE_VITAL_SIGN.csv"))

chiayi$CHOL <- chiayi$CHOL * 0.0259
chiayi$TG <- chiayi$TG * 0.0113
chiayi$LOC <- as.character(chiayi$LOC)
chiayi$LOC[(chiayi$LOC == "2")|(chiayi$LOC == "3")|(chiayi$LOC == "6")] <- "training set"
chiayi$LOC[chiayi$LOC == "8"] <- "test set"
chiayi$LOC[(chiayi$LOC != "training set")&(chiayi$LOC != "test set")] <- NA

chiayi_1 <- chiayi %>%
  drop_na(LOC) %>%
  select(LOC, Age, Sex, eNIHSS, HTN, DM, Dyslipidemia, AF, CAD, CHF, Cancer.before.adm, Smoking, 
         CHOL, TG, Creatinine, ALT, Mean.HR, Mean.SBP, Mean.DBP, Mean.PP, Mean.RR, HR.CV, SBP.CV, 
         DBP.CV, PPCV, RR.CV)

chiayi_1[chiayi_1 == 9999] <- NA

#chiayi_1 <- lapply(chiayi_1, numeric)
varsToFactor <- c("Sex", "HTN", "DM", "Dyslipidemia", "AF", "CAD", "CHF", "Cancer.before.adm", "Smoking")
chiayi_1[varsToFactor] <- lapply(chiayi_1[varsToFactor], factor)

cont_var <- c("Age", "eNIHSS", "CHOL", "TG", "Creatinine", "ALT", "Mean.HR", "Mean.SBP", "Mean.DBP", 
              "Mean.PP", "Mean.RR", "HR.CV", "SBP.CV", "DBP.CV", "PPCV", "RR.CV")

chiayi_tableone<- CreateTableOne(data = chiayi_1, strata = c("LOC"), addOverall = TRUE, includeNA=FALSE)
chiayi_tableone_1<- print(chiayi_tableone, smd = FALSE)
write.csv(chiayi_tableone_1, file = pathJoin("tableone.csv"))

chiayi_tableone<- CreateTableOne(data = chiayi_1, strata = c("LOC"), addOverall = TRUE, includeNA=FALSE, testNonNormal = oneway.test)
chiayi_tableone_1<- print(chiayi_tableone, smd = FALSE, nonnormal = cont_var)
write.csv(chiayi_tableone_1, file = pathJoin("tableone_iqr.csv"))
