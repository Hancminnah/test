# To create extract medication features
# Features to extract: 1) conj oestrogen
#                      2) premarin / premarin vaginal
#                      3) vagifem
#                      4) oestradiol vaginal
#                      5) warfarin
library(data.table)
setwd('/data/volume/MinMin/GeneralComplication/Deidentified_Extracted_Data')
medicationorder <- fread('./case_medicationorder_mapped.csv')
premarin_index <- which(medicationorder$DrugName == "PREMARIN VAGINAL"| medicationorder$DrugName=="PREMARIN")
warfarin_index <- which(medicationorder$DrugName == "WARFARIN")
vagifem_index <- which(medicationorder$DrugName == "VAGIFEM")
oestradiol_index <- which(medicationorder$DrugName == "OESTRADIOL" | medicationorder$DrugName == "OESTRADIOL VAGINAL")
conj_oestrogen_index <- which(medicationorder$DrugName == "CONJ OESTROGEN")

# Check Data
medicationorder[warfarin_index,.(DeIdentifiedNRIC,CaseIdentificationNumber,DrugCode,DrugName,DrugStrengthAmount,DrugStrengthDescription)]
#medicationorder[warfarin_index,.(OrderDateTime,TransactionDateTime)]
medicationorder[warfarin_index,.(DeIdentifiedNRIC,CaseIdentificationNumber,DrugFormCode,DrugFormDescription)]
medicationorder[warfarin_index,.(DeIdentifiedNRIC,CaseIdentificationNumber,OrderFrequencyCode,OrderFrequencyDescription,OrderDurationCode,OrderDurationCodeDescription)]
medicationorder[warfarin_index,.(DeIdentifiedNRIC,CaseIdentificationNumber,DispenseDosageAmount,FindingCode)]

tmp <- medicationorder[c(warfarin_index,premarin_index,vagifem_index,oestradiol_index,conj_oestrogen_index),.(DeIdentifiedNRIC,CaseIdentificationNumber,OrderFrequencyCode,OrderFrequencyDescription,OrderDurationCode,OrderDurationCodeDescription)]

#OrderFrequencyCode     OrderFrequencyDescription
#1:                0                 EVERY MORNING
#2:                  D                         DAILY
#3:
#4:                  1                   EVERY NIGHT
#5:                  X                4 TIMES A WEEK
#6:                  W                3 TIMES A WEEK
#7:                  A             ON ALTERNATE DAYS
#8:                  V                2 TIMES A WEEK
#9:                  Y                5 TIMES A WEEK
#10:                  Z                6 TIMES A WEEK
#11:                  U                   ONCE WEEKLY
#12:                  9                 EVERY EVENING
#13:             OM1357 EVERY MORNING MON,WED,FRI,SUN
#14:              OM246   EVERY MORNING TUE,THURS,SAT
#15:                  I                   IMMEDIATELY
#16:                  2                 2 TIMES A DAY
#17:                 DM                             0
#18:                 PD                 BEFORE DINNER
#19:                 OT                        OTHERS

# Once a day: EVERY MORNING, DAILY, EVERY NIGHT, EVERY EVENING, BEFORE DINNER [0, D, 1, 9, PD] ==> 7 times a week
# Twice a day: 2 TIMES A DAY [2] ==> 14 times a week
# ONCE WEEKLY    ==> 1 time a week
# 2 TIMES A WEEK ==> 2 times a week
# 3 TIMES A WEEK ==> 3 times a week
# 4 TIMES A WEEK ==> 4 times a week
# 5 TIMES A WEEK ==> 5 times a week
# 6 TIMES A WEEK ==> 6 times a week
# ON ALTERNATE DAYS ==> 3 times a week (Assuming tue, thur, sat) [A]
# EVERY MORNING MON,WED,FRI,SUN ==> 4 times a week [OM1357]
# EVERY MORNING TUE,THURS,SAT ==> 3 times a week [OM246]

unique(tmp[,.(OrderFrequencyCode,OrderFrequencyDescription)])
# Now computing the standard equivalent dosage
#medicationorder$SED <- medicationorder$DispenseDosageAmount * medicationorder$