# Healthcare cost analysis

setwd("D:/Business Intelligence/Simplilearn Material/Course 2 - Data Science with R/Healthcare Cost Analysis")
getwd()

# Reading the data by loading package for excel file.
install.packages("readxl")
library("readxl")

HospitalCost <- read_excel("Hospitalcosts.xlsx")

# Viewing the Hospital dataset

View(HospitalCost)
head(HospitalCost)

# 1. To record the patient statistics, the agency wants to find the age category of people who frequent the hospital
# and has the maximum expenditure.
summary(HospitalCost)
head(HospitalCost$AGE)
summary(HospitalCost$AGE)
table(HospitalCost$AGE)
hist(HospitalCost$AGE)
summary(as.factor(HospitalCost$AGE))
max(table(HospitalCost$AGE))
max(summary(as.factor(HospitalCost$AGE)))
which.max(table(HospitalCost$AGE))
age <- aggregate(TOTCHG ~ AGE, data = HospitalCost, sum)
max(age)

# 2. In order of severity of the diagnosis and treatments and to find out the expensive treatments, 
# the agency wants to find the diagnosis related group that has maximum hospitalization and expenditure.
Diagnosis <- table(HospitalCost$APRDRG)
df <- as.data.frame(Diagnosis)
names(df)[1] = 'Diagnosis Group'
df
which.max(table(HospitalCost$APRDRG))
which.max(Diagnosis)
which.max(df)          
res <- aggregate(TOTCHG ~ APRDRG, data = HospitalCost, sum)
res
which.max(res$TOTCHG)
res[which.max(res$TOTCHG),]

# 3. To make sure that there is no malpractice, the agency needs to analyze if the race of the patient is related 
# to the hospitalization costs
table(HospitalCost$RACE)
HospitalCost$RACE <- as.factor(HospitalCost$RACE)
NoMal <- lm(TOTCHG ~ RACE,data=HospitalCost)
NoMal
summary(NoMal)
NoMal1 <- aov(TOTCHG ~ RACE,data=HospitalCost)
summary(NoMal1)
HospitalCost <- na.omit(HospitalCost)

# 4. To properly utilize the costs, the agency has to analyze the severity of the hospital costs by age and gender 
# for proper allocation of resources.
table(HospitalCost$FEMALE)
Allocation <- aov(TOTCHG ~ AGE+FEMALE,data=HospitalCost)
summary(Allocation)
Allocation1 <- lm(TOTCHG ~ AGE+FEMALE,data=HospitalCost)
summary(Allocation1)

# 5. Since the length of stay is the crucial factor for inpatients, the agency wants to find if the length of stay 
# can be predicted from age, gender, and race.
table(HospitalCost$LOS)
AGR <- aov(LOS ~ AGE+FEMALE+RACE,data=HospitalCost)
summary(AGR)
AGR1 <- lm(LOS ~ AGE+FEMALE+RACE,data=HospitalCost)
summary(AGR1)

# 6. To perform a complete analysis, the agency wants to find the variable that mainly affects the hospital costs.
aov(TOTCHG ~.,data=HospitalCost)
modl <- lm(TOTCHG ~ .,data=HospitalCost)
summary(modl)



