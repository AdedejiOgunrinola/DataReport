##Data Report - Adedeji Ogunrinola

##Setting our working directory
setwd("~/Documents/R")

##We call-up packages that will be relevant to our data report.
library(tidyverse)
library(readxl)
library(readr)
library(haven)
library(dplyr)
library(expss)
library(pastecs)
library(data.table)
library(flextable)
library(survey)
library(ggthemes)
library(kableExtra)
library(pollster)
library(plm)
library(modelsummary)
library(Hmisc)

##We import our IPEDS data currently in Excel format
IPEDS <- read_excel("IPEDS.XLSX")

##Creating our analytical data set
##First, we will select data from our year of concern, that is, 2012
IPEDSN <- subset(IPEDS, YEAR %in% c(2012))

##Secondly, we will CREATE a variable AVAIDAMT. To create our data set, we need to create a new variable by joining AVPRAID1 and AVPRAID2. This will help avoid errors and an 'empty' dataset.
IPEDSN$AVPRAID1[is.na(IPEDSN$AVPRAID1)] <- 0
IPEDSN$AVPRAID2[is.na(IPEDSN$AVPRAID2)] <- 0
IPEDSN$AVPRAIDT <- IPEDSN$AVPRAID1 + IPEDSN$AVPRAID2
IPEDSN$AVPRAIDT[IPEDSN$AVPRAIDT == 0] <- NA #Now we have our new variable created successfully

##Thirdly, we will clean our data set by selecting the variables of concern for our analysis
IPEDS1 <- filter(IPEDSN, !is.na(UNITID) &  !is.na(INSTITUTION) & !is.na(YEAR) & !is.na(STATE) &  !is.na(CONTROL) &  !is.na(LEVEL) &  !is.na(TPISONC) &  !is.na(TPOSONC) &  !is.na(TPISOFC) & !is.na(TPOSOFC) & !is.na(AVAIDAMT) & !is.na(AVPRAIDT) 
                 & !is.na(UNENROLL) & !is.na(GRENROLL) &  !is.na(FTRETEN) &  !is.na(PTRETEN) & !is.na(GRADRATET) &  !is.na(GRADRATEM) &  !is.na(GRADRATEF))
IPEDS1 <- select(IPEDS1, UNITID, INSTITUTION, YEAR, STATE, CONTROL, LEVEL, TPISONC, TPOSONC, TPISOFC, TPOSOFC, AVAIDAMT, AVPRAIDT, UNENROLL, GRENROLL, FTRETEN, PTRETEN, GRADRATET, GRADRATEM, GRADRATEF)
nrow(IPEDS1)

##Fourthly, we will CREATE another variable called TOENROLL (Total Enrollment). This is the addition of UNENROLL (Undergraduate Enrollment) and GRENROLL (Graduate Enrollment).
IPEDS1$TOENROLL <- IPEDS1$UNENROLL + IPEDS1$GRENROLL

##Next, we will MERGE a new data set to IPEDS1. This data set is known as DCP, obtained from Delta Cost Project.
#We begin by importing the excel file containing the figures
DCP <- read_excel("DCP.XLSX")
#Since we have observation for multiple years, we select only the year of concern, that is, 2012
DCPN <- subset(DCP, YEAR %in% c(2012))
#We change negative values to NA
DCPN$NETTUI01[DCPN$NETTUI01 < 0] <- NA
DCPN$NETSTUDTUI[DCPN$NETSTUDTUI < 0] <- NA
DCPNN <- filter(DCPN, !is.na(UNITID) & !is.na(NETTUI01) &  !is.na(NETSTUDTUI))
#We select only the variables needed
DCPNN <- select(DCPNN, UNITID, NETTUI01, NETSTUDTUI)
#The merged data set is obtained
IPEDSmerged <- left_join(IPEDS1,DCPNN, by = "UNITID")

##We obtain our analytical data set as IPEDSMA. This is because merging IPEDS and DCP resulted in some observations with 'NA'
IPEDSMA <- filter(IPEDSmerged, !is.na(UNITID) &  !is.na(INSTITUTION) & !is.na(YEAR) & !is.na(STATE) &  !is.na(CONTROL) &  !is.na(LEVEL) &  !is.na(TPISONC) &  !is.na(TPOSONC) &  !is.na(TPISOFC) & !is.na(TPOSOFC) & !is.na(AVAIDAMT) & !is.na(AVPRAIDT) 
                  & !is.na(NETTUI01) &  !is.na(NETSTUDTUI) & !is.na(UNENROLL) & !is.na(GRENROLL) & !is.na(TOENROLL) & !is.na(FTRETEN) & !is.na(PTRETEN) & !is.na(GRADRATET) &  !is.na(GRADRATEM) &  !is.na(GRADRATEF))
IPEDSMA <- select(IPEDSMA, UNITID, INSTITUTION, YEAR, STATE, CONTROL, LEVEL, TPISONC, TPOSONC, TPISOFC, TPOSOFC, AVAIDAMT, AVPRAIDT, NETTUI01, NETSTUDTUI, UNENROLL, GRENROLL, TOENROLL, FTRETEN, PTRETEN, GRADRATET, GRADRATEM, GRADRATEF)
nrow(IPEDSMA)

##Creating a new variable from outside the dataset, and adding it to the analytical dataset
#We find the mean of average net price paid by students awarded grant or scholarship aid and then assign the mean value to all the observations
AVPRAIDT_MEAN <- mean(IPEDSMA$AVPRAIDT)
IPEDSMA$AVPRAIDT_MEAN <- AVPRAIDT_MEAN

##To make our data set efficiently readable, we will add labels to most of our variables. We use the 'Hmisc' package
label(IPEDSMA$UNITID) <- "Unit ID of Institution"
label(IPEDSMA$CONTROL) <- "Control of institution
1-Public
2-Private not-for-profit
3-Private for-profit
-3-{Not available}"
label(IPEDSMA$LEVEL) <- "Level of institution
1-Four or more years
2-At least 2 but less than 4 years
3-Less than 2 years (below associate)
-3-{Not available}"
label(IPEDSMA$TPISONC) <- "Total price for in-state students living on campus"
label(IPEDSMA$TPOSONC) <- "Total price for out-of-state students living on campus"
label(IPEDSMA$TPISOFC) <- "Total price for in-state students living off campus (not with family)"
label(IPEDSMA$TPOSOFC) <- "Total price for out-of-state students living off campus (not with family)"
label(IPEDSMA$AVAIDAMT) <- "Average amount of federal  state  local or institutional grant aid awarded"
label(IPEDSMA$AVPRAIDT) <- "Average net price-students awarded grant or scholarship aid"
label(IPEDSMA$NETTUI01) <- "Finance revenues - Tuition from student plus financial aid"
label(IPEDSMA$NETSTUDTUI) <- "Finance revenues - Tuition from student only (without financial aid)"
label(IPEDSMA$UNENROLL) <- "Undergraduate enrollment"
label(IPEDSMA$GRENROLL) <- "Graduate enrollment"
label(IPEDSMA$TOENROLL) <- "Total enrollment"
label(IPEDSMA$FTRETEN) <- "Full-time retention rate"
label(IPEDSMA$PTRETEN) <- "Part-time retention rate"
label(IPEDSMA$GRADRATET) <- "Graduation rate  total cohort"
label(IPEDSMA$GRADRATEM) <- "Graduation rate  men"
label(IPEDSMA$GRADRATEF) <- "Graduation rate  women"
label(IPEDSMA$AVPRAIDT_MEAN) <- "Mean of Average net price-students awarded grant or scholarship aid"

##Descriptive Analysis
IPEDSMADA <- subset(IPEDSMA, select = c(TPISONC, TPOSONC, TPISOFC, TPOSOFC, AVAIDAMT, AVPRAIDT, NETTUI01, NETSTUDTUI, UNENROLL, GRENROLL, TOENROLL, FTRETEN, PTRETEN, GRADRATET, GRADRATEM, GRADRATEF))
options(scipen=100)
options(digits=2)
summary(IPEDSMADA)
length(IPEDSMA$TPISONC)
length(IPEDSMA$TPOSONC)
length(IPEDSMA$TPISOFC)
length(IPEDSMA$TPOSOFC)
length(IPEDSMA$AVAIDAMT)
length(IPEDSMA$AVPRAIDT)
length(IPEDSMA$NETTUI01)
length(IPEDSMA$NETSTUDTUI)
length(IPEDSMA$UNENROLL)
length(IPEDSMA$GRENROLL)
length(IPEDSMA$TOENROLL)
length(IPEDSMA$FTRETEN)
length(IPEDSMA$PTRETEN)
length(IPEDSMA$GRADRATET)
length(IPEDSMA$GRADRATEM)
length(IPEDSMA$GRADRATEF)

##Descriptive Analysis to obtain standard deviation
options(scipen=100)
options(digits=2)
stat.desc(IPEDSMADA)

##Descriptive Analysis by control of institution
stat.desc(select(filter(IPEDSMA, IPEDSMA$CONTROL==1), TPISONC, TPOSONC, TPISOFC, TPOSOFC, AVAIDAMT, AVPRAIDT, FTRETEN, PTRETEN, GRADRATET))
stat.desc(select(filter(IPEDSMA, IPEDSMA$CONTROL==2), TPISONC, TPOSONC, TPISOFC, TPOSOFC, AVAIDAMT, AVPRAIDT, FTRETEN, PTRETEN, GRADRATET))
stat.desc(select(filter(IPEDSMA, IPEDSMA$CONTROL==3), TPISONC, TPOSONC, TPISOFC, TPOSOFC, AVAIDAMT, AVPRAIDT, FTRETEN, PTRETEN, GRADRATET))

##Descriptive Analysis by level of institution
stat.desc(select(filter(IPEDSMA, IPEDSMA$LEVEL==1), TPISONC, TPOSONC, TPISOFC, TPOSOFC, AVAIDAMT, AVPRAIDT, FTRETEN, PTRETEN, GRADRATET))
stat.desc(select(filter(IPEDSMA, IPEDSMA$LEVEL==2), TPISONC, TPOSONC, TPISOFC, TPOSOFC, AVAIDAMT, AVPRAIDT, FTRETEN, PTRETEN, GRADRATET))
##stat.desc(select(filter(IPEDSMA, IPEDSMA$LEVEL==3), TPISONC, TPOSONC, TPISOFC, TPOSOFC, AVAIDAMT, AVPRAIDT, FTRETEN, PTRETEN, GRADRATET))

##Selecting institutions with the highest and lowest values as follows
H_TPISONC <- IPEDSMA$INSTITUTION[IPEDSMA$TPISONC == 61407]
print(H_TPISONC)
L_TPISONC <- IPEDSMA$INSTITUTION[IPEDSMA$TPISONC == 8015]
print(L_TPISONC)
H_TPOSONC <- IPEDSMA$INSTITUTION[IPEDSMA$TPOSONC == 61407]
print(H_TPOSONC)
L_TPOSONC <- IPEDSMA$INSTITUTION[IPEDSMA$TPOSONC == 8015]
print(L_TPOSONC)
H_TPISOFC <- IPEDSMA$INSTITUTION[IPEDSMA$TPISOFC == 61407]
print(H_TPISOFC)
L_TPISOFC <- IPEDSMA$INSTITUTION[IPEDSMA$TPISOFC == 7950]
print(L_TPISOFC)
H_TPOSOFC <- IPEDSMA$INSTITUTION[IPEDSMA$TPOSOFC == 61407]
print(H_TPOSOFC)
L_TPOSOFC <- IPEDSMA$INSTITUTION[IPEDSMA$TPOSOFC == 8310]
print(L_TPOSOFC)
H_AVAIDAMT <- IPEDSMA$INSTITUTION[IPEDSMA$AVAIDAMT == 40836]
print(H_AVAIDAMT)
L_AVAIDAMT <- IPEDSMA$INSTITUTION[IPEDSMA$AVAIDAMT == 667]
print(L_AVAIDAMT)
H_AVPRAIDT <- IPEDSMA$INSTITUTION[IPEDSMA$AVPRAIDT == 43489]
print(H_AVPRAIDT)
L_AVPRAIDT <- IPEDSMA$INSTITUTION[IPEDSMA$AVPRAIDT == 1442]
print(L_AVPRAIDT)
H_NETTUI01 <- IPEDSMA$INSTITUTION[IPEDSMA$NETTUI01 == 1468811379]
print(H_NETTUI01)
L_NETTUI01 <- IPEDSMA$INSTITUTION[IPEDSMA$NETTUI01 == 160565]
print(L_NETTUI01)
H_NETSTUDTUI <- IPEDSMA$INSTITUTION[IPEDSMA$NETSTUDTUI == 1334386000]
print(H_NETSTUDTUI)
L_NETSTUDTUI <- IPEDSMA$INSTITUTION[IPEDSMA$NETSTUDTUI == 0]
print(L_NETSTUDTUI)
H_UNENROLL <- IPEDSMA$INSTITUTION[IPEDSMA$UNENROLL == 69380]
print(H_UNENROLL)
L_UNENROLL <- IPEDSMA$INSTITUTION[IPEDSMA$UNENROLL == 46]
print(L_UNENROLL)
H_GRENROLL <- IPEDSMA$INSTITUTION[IPEDSMA$GRENROLL == 28239]
print(H_GRENROLL)
L_GRENROLL <- IPEDSMA$INSTITUTION[IPEDSMA$GRENROLL == 0]
print(L_GRENROLL)
H_TOENROLL <- IPEDSMA$INSTITUTION[IPEDSMA$TOENROLL == 77734]
print(H_TOENROLL)
L_TOENROLL <- IPEDSMA$INSTITUTION[IPEDSMA$TOENROLL == 46]
print(L_TOENROLL)
H_FTRETEN <- IPEDSMA$INSTITUTION[IPEDSMA$FTRETEN == 100]
print(H_FTRETEN)
L_FTRETEN <- IPEDSMA$INSTITUTION[IPEDSMA$FTRETEN == 17]
print(L_FTRETEN)
H_PTRETEN <- IPEDSMA$INSTITUTION[IPEDSMA$PTRETEN == 100]
print(H_PTRETEN)
L_PTRETEN <- IPEDSMA$INSTITUTION[IPEDSMA$PTRETEN == 0]
print(L_PTRETEN)
H_GRADRATET <- IPEDSMA$INSTITUTION[IPEDSMA$GRADRATET == 95]
print(H_GRADRATET)
L_GRADRATET <- IPEDSMA$INSTITUTION[IPEDSMA$GRADRATET == 0]
print(L_GRADRATET)
H_GRADRATEM <- IPEDSMA$INSTITUTION[IPEDSMA$GRADRATEM == 100]
print(H_GRADRATEM)
L_GRADRATEM <- IPEDSMA$INSTITUTION[IPEDSMA$GRADRATEM == 0]
print(L_GRADRATEM)
H_GRADRATEF <- IPEDSMA$INSTITUTION[IPEDSMA$GRADRATEF == 100]
print(H_GRADRATEF)
L_GRADRATEF <- IPEDSMA$INSTITUTION[IPEDSMA$GRADRATEF == 0]
print(L_GRADRATEF)

##Correlation between AVPRAIDT and GRADRATET
CORAIDGRATE <- cor(IPEDSMA$AVPRAIDT, IPEDSMA$GRADRATET)
print(CORAIDGRATE)
##Scatterplot for AVPRAIDT and GRADRATET
ggplot(data = IPEDSMA) + 
  geom_point(mapping = aes(x = AVPRAIDT, y = GRADRATET, color = factor(CONTROL))) +
  scale_color_manual(values = c("1" = "red", "2" = "grey", "3" = "blue"), 
                     (labels = c("Control of Institution 
1 = Public 
2 = Private (Not-For-Profit) 
3 = Private (For-Profit)"))) +
  labs(x = "Average net price-students awarded grant or scholarship aid", y = "Graduation rate total cohort", 
       title = "FIGURE 1: Scatteplot of Average Tuition After Aid and Graduation Rate [Total Cohort]", caption = "The correlation coefficient between average price after aid and graduation rate (total cohort)  = 0.44") 

##Correlation between AVAIDAMT and FTRETEN
CORAIDFTRETN <- cor(IPEDSMA$AVAIDAMT, IPEDSMA$FTRETEN)
print(CORAIDFTRETN)
##Scatterplot for AVAIDAMT and FTRETEN
ggplot(data = IPEDSMA) + 
  geom_point(mapping = aes(x = AVAIDAMT, y = FTRETEN, color = factor(CONTROL))) +
  scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "green"), 
                     (labels = c("Control of Institution 
1 = Public 
2 = Private (Not-For-Profit) 
3 = Private (For-Profit)"))) +
  labs(x = "Average amount of federal, state, local or institutional grant aid awarded", y = "Full-Time Retention Rate", 
       title = "FIGURE 2: Scatteplot of Average Amount of Aid Awarded and Full-Time Retention Rate", caption = "The correlation coefficient between average amount of aid awarded and full-time retention rate  = 0.42") 

##Correlation between AVAIDAMT and PTRETEN
CORAIDPTRETN <- cor(IPEDSMA$AVAIDAMT, IPEDSMA$PTRETEN)
print(CORAIDPTRETN)
##Scatterplot for AVAIDAMT and PTRETEN
ggplot(data = IPEDSMA) + 
  geom_point(mapping = aes(x = AVAIDAMT, y = GRADRATET, color = factor(CONTROL))) +
  scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "black"), 
                     (labels = c("Control of Institution 
1 = Public 
2 = Private (Not-For-Profit) 
3 = Private (For-Profit)"))) +
  labs(x = "Average amount of federal, state, local or institutional grant aid awarded", y = "Part-Time Retention Rate", 
       title = "FIGURE 3: Scatteplot of Average Amount of Aid Awarded and Part-Time Retention Rate", caption = "The correlation coefficient between average amount of aid awarded and part-time retention rate  = 0.1") 

##Creating a bar graph with means of average tuition after aid by control of institution
#Mean of average tuition after aid in public, private (not-for-profit) and private (for-profit) institutions
MeanPub <- mean(select(filter(IPEDSMA, CONTROL == 1), AVPRAIDT))
MeanPriv <- mean(select(filter(IPEDSMA, CONTROL == 2), AVPRAIDT))
MeanPrivPro <- mean(select(filter(IPEDSMA, CONTROL == 3), AVPRAIDT))

#Table for mean incomes
MEAN_AVPRAIDT <- data.frame(CONTROL = c(1, 2, 3),
                            AVPRAIDT = c(MeanPub, MeanPriv, MeanPrivPro))

#Creating bar chart for mean incomes
ggplot(data = MEAN_AVPRAIDT) +
  geom_bar(mapping = aes(x = factor(CONTROL), y = AVPRAIDT), color = "red", stat="identity") +
  geom_text(mapping = aes(x = factor(CONTROL), y = AVPRAIDT, label = round(AVPRAIDT, digits=0)), vjust=0) +
  scale_x_discrete(labels = c("Public", "Private (Not-For-Profit)", "Private (For-Profit)")) +
  labs(title = "FIGURE 4: Average Tuition after Aid by Control of Institution", x = "Control of Institution", y = "Average Tuition after Aid") +
  theme_minimal()

##Simple Regression 1
RegressionA <- list(
  "Base"     = lm(GRADRATET ~ AVPRAIDT, data = IPEDSMA),
  "Adding Controls"    = lm(GRADRATET ~ AVPRAIDT + AVAIDAMT + FTRETEN + PTRETEN, data = IPEDSMA)
)

modelsummary(RegressionA, stars = c('*' = .1, '**' = .05, '***' = .01))

##Simple Regression 2
RegressionB <- list(
  "Base"     = lm(NETSTUDTUI ~ TOENROLL, data = IPEDSMA)
)

modelsummary(RegressionA, stars = c('*' = .1, '**' = .05, '***' = .01))
