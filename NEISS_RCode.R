rm(list = ls())

# ================================================================
# Author  : Moneka Bommasani
# Date    : May 21, 2017
# Purpose : NEISS Data Analysis
# ================================================================

# **********************************************************************************************

# 1) Load the libraries #

library(dplyr)

# **********************************************************************************************

# 2) Import the data #

NEISS2014 <- read.csv("C:/Users/Moneka Bommasani/Desktop/BI_Developer_HW/NEISS2014.csv")
attach(NEISS2014)       #Attach the dataset

BodyParts <- read.csv("C:/Users/Moneka Bommasani/Desktop/BI_Developer_HW/BodyParts.csv")

DiagnosisCodes <- read.csv("C:/Users/Moneka Bommasani/Desktop/BI_Developer_HW/DiagnosisCodes.csv")

Disposition <- read.csv("C:/Users/Moneka Bommasani/Desktop/BI_Developer_HW/Disposition.csv")


# **********************************************************************************************

# QUESTION 1 #

bodyparts <- left_join(NEISS2014, BodyParts, by = c("body_part" = "Code"))
tail(names(sort(table(bodyparts$BodyPart))),3)
head(names(sort(table(bodyparts$BodyPart))),5)

#To Check
transform(as.data.frame(table(bodyparts$BodyPart)),percentage_column=Freq/nrow(bodyparts)*100)

# **********************************************************************************************

# QUESTION 2 #

#Approach1
nrow(data.frame(grep("skateboard", NEISS2014$narrative, ignore.case = TRUE )))

subset <- NEISS2014[grep("skateboard", NEISS2014$narrative, ignore.case = TRUE), ]
options(digits=4)
transform(as.data.frame(table(subset$sex)),percentage_column=Freq/nrow(subset)*100)

+(subset$age > 200) #To check if any entry is greater than 200

mean(subset$age)
summary(subset$age)

#Approach2
skate <- subset(NEISS2014 , NEISS2014$prod1 == 1333 | NEISS2014$prod2 == 1333)
nrow(skate)

transform(as.data.frame(table(skate$sex)),percentage_column=Freq/nrow(skate)*100)

+(subset$age > 200) #To check if any entry is greater than 200

mean(skate$age)
summary(skate$age)

# **********************************************************************************************

# QUESTION 3 #

Diagno <- left_join(NEISS2014, DiagnosisCodes, by = c("diag" = "Code"))
hosp <- subset(Diagno , Diagno$disposition == 4)
tail(names(sort(table(hosp$Diagnosis))),1)

#To Check
p <- transform(as.data.frame(table(hosp$Diagnosis)),percentage_column=Freq/nrow(hosp)*100)


leave <- subset(Diagno , Diagno$disposition == 6)
tail(names(sort(table(leave$Diagnosis))),2)
tail(names(sort(table(leave$diag_other))),2)

#To Check
q <- transform(as.data.frame(table(leave$Diagnosis)),percentage_column=Freq/nrow(leave)*100)
r <- transform(as.data.frame(table(leave$diag_other)),percentage_column=Freq/nrow(leave)*100)

# **********************************************************************************************

# QUESTION 4 #

hist(NEISS2014$age, col="orange", main = "Relationship between Age and Injuries" , breaks = 500, xlab = "Age", ylab = "Number of Injuries")
