install.packages("mice")
install.packages("VIM")
install.packages("Gmisc")
library(mice)
library(VIM)
library(Gmisc)

setwd("/Users/jackychen/OneDrive - University College London/SINGLE_TASK/STROKE_VITAL")
file_path <- pathJoin("STROKE_VITAL_SIGN_PP.csv")
sign <- read.csv(file_path)
sign[sign == 9999] = NA
sign1 <- subset(sign, select = -c(LOC, UID, Hospital_ID, admission_date, discharge_date, death_date))

nominal_features = c("Sex")

ordinal_features = c("E", "V", "M")

boolean = c("AF", "DM", "HTN", "Hyperlipidemia", "CHF", "Smoking", "Cancer.before.adm", 
            "Foley", "NG", "ICU", "Mortality", "CVDeath", "CAD", "Dyslipidemia")

continuous = c("Age", "ALT", "AST", "CHOL", "HbA1c", "TG", "Creatinine", "MPsum", "Mean.HR", 
               "MeanHR.G", "HR.SD", "HRSD.G", "HR.CV", "HRCV.G", "Mean.SBP", "Mean.SBP.G",
               "SBP.SD", "SBPSD.G", "SBP.CV", "SBPCV.G", "Mean.DBP", "MeanDBP.G", "DBP.SD", 
               "DBPSD.G", "DBP.CV", "DBPCV.G", "Mean.RR", "MeanRR.G", "RR.SD", "RRSD.G", 
               "RR.CV", "RRCV.G", "SurvivalWeeks", "eNIHSS", "BMI", "LDL", "SurvivalDays",
               "Mean.PP", "PP.SD", "PPCV")

#mice imputation

#md.pattern(sign1, plot = FALSE)
#md.pairs(sign1)
#sign_plot <- aggr(sign1,col=c('navyblue','yellow'),numbers=TRUE,sortVars=TRUE,labels=names(TSR_ALL3),cex.axis=.7,gap=3,ylab=c("Missing data","Pattern"))

sign1[nominal_features] <- lapply(sign1[nominal_features], as.factor)
sign1[ordinal_features] <- lapply(sign1[ordinal_features], as.factor)
sign1[boolean] <- lapply(sign1[boolean], as.factor)
sign1[continuous] <- lapply(sign1[continuous], as.numeric)

methods_sign <- c(Sex = "polyreg", Age = "pmm", AF = "logreg", DM = "logreg", HTN = "logreg", 
                  Hyperlipidemia = "logreg", CHF = "logreg", Smoking = "logreg", 
                  Cancer.before.adm = "logreg", ALT = "pmm", AST = "pmm", CHOL = "pmm", 
                  HbA1c = "pmm", TG = "pmm", Creatinine = "pmm", E = "polr", V = "polr", 
                  M = "polr", Foley = "logreg", NG = "logreg", ICU = "logreg", MPsum = "pmm", 
                  Mean.HR = "pmm", MeanHR.G = "pmm", HR.SD = "pmm", HRSD.G = "pmm", 
                  HR.CV = "pmm", HRCV.G = "pmm", Mean.SBP = "pmm", Mean.SBP.G = "pmm", 
                  SBP.SD = "pmm", SBPSD.G = "pmm", SBP.CV = "pmm", SBPCV.G = "pmm", 
                  Mean.DBP = "pmm", MeanDBP.G = "pmm", DBP.SD = "pmm", DBPSD.G = "pmm", 
                  DBP.CV = "pmm", DBPCV.G = "pmm", Mean.RR = "pmm", MeanRR.G = "pmm", 
                  RR.SD = "pmm", RRSD.G = "pmm", RR.CV = "pmm", RRCV.G = "pmm", 
                  Mortality = "logreg", SurvivalWeeks = "pmm", CVDeath = "logreg", 
                  CAD = "logreg", Dyslipidemia = "logreg", eNIHSS = "pmm", BMI = "pmm", 
                  LDL = "pmm", SurvivalDays = "pmm", Mean.PP = "pmm", PP.SD = "pmm", 
                  PPCV = "pmm")

sign_imp <- mice(sign1, maxit = 10, m = 1, method = methods_sign, print = TRUE, seed = 19)
#summary(sign_imp)

sign_mice <- complete(sign_imp, 1)

sign[names(sign_mice)] <- sign_mice[names(sign_mice)]

write.csv(sign, "STROKE_VITAL_SIGN_PP_MICE.csv", row.names=FALSE)

