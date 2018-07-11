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

Table4 <- medicationorder[c(warfarin_index,premarin_index,vagifem_index,oestradiol_index,conj_oestrogen_index),.(DeIdentifiedNRIC,CaseIdentificationNumber,OrderFrequencyCode,OrderFrequencyDescription,OrderDurationCode,OrderDurationCodeDescription)]

unique(Table4[,.(OrderFrequencyCode,OrderFrequencyDescription)])
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
# ONCE WEEKLY    ==> 1 time a week [U]
# 2 TIMES A WEEK ==> 2 times a week [V]
# 3 TIMES A WEEK ==> 3 times a week [W]
# 4 TIMES A WEEK ==> 4 times a week [X]
# 5 TIMES A WEEK ==> 5 times a week [Y]
# 6 TIMES A WEEK ==> 6 times a week [Z]
# ON ALTERNATE DAYS ==> 3 times a week (Assuming tue, thur, sat) [A]
# EVERY MORNING MON,WED,FRI,SUN ==> 4 times a week [OM1357]
# EVERY MORNING TUE,THURS,SAT ==> 3 times a week [OM246]

unique(tmp[,.(OrderFrequencyCode,OrderFrequencyDescription,OrderDurationCode,OrderDurationCodeDescription)])
#OrderDurationCode OrderDurationCodeDescription
#1:                M1                      1 MONTH
#2:                10                      10 DAYS
#3:                W1                       1 WEEK
#4:                W2                      2 WEEKS
#5:                M2                     2 MONTHS
#6:                W6                      6 WEEKS
#7:                M3                     3 MONTHS
#8:                 1                        1 DAY
#9:                 9                       9 DAYS
#10:                 8                       8 DAYS
#11:                 3                       3 DAYS
#12:                 7                       7 DAYS
#13:                 2                       2 DAYS
#14:
#15:                 4                       4 DAYS
#16:                W3                      3 WEEKS
#17:                X0                     10 WEEKS
#18:                W4                      4 WEEKS
#19:                12                      12 DAYS
#20:                 -
#21:                 6                       6 DAYS
#22:                 5                       5 DAYS
#23:                M4                     4 MONTHS
#24:                W5                      5 WEEKS
#25:                W8                      8 WEEKS
#26:                11                      11 DAYS
#27:                M6                     6 MONTHS
#28:                M5                     5 MONTHS
#29:                14                      14 DAYS
#30:                X6                     16 WEEKS
#31:                15                      15 DAYS
#32:     EVERY MORNING                           W4
#33:     EVERY MORNING                           M1

# 1 DAY, 2 DAYS, 3 DAYS, 4 DAYS, 5 DAYS, 6 DAYS, 7 DAYS, 8 DAYS, 9 DAYS, 11 DAYS, 12 DAYS, 14 DAYS, 15 DAYS [1,2,3,4,5,6,7,8,9,11,12,14,15]
# 1 WEEK, 2 WEEKS, 3 WEEKS, 4 WEEKS, 5 WEEKS, 6 WEEKS, 8 WEEKS, 10 WEEKS, 16 WEEKS [W1, W2, W3, W4, W5, W6, W8, X0, X6]
# 1 MONTH, 2 MONTHS, 3 MONTHS, 4 MONTHS, 5 MONTHS, 6 MONTHS [M1, M2, M3, M4, M5, M6]

# Need to convert Frequency into similar format in terms of how many times a day


# Now computing the standard equivalent dosage
dur1_index <- which(Table4$OrderDurationCode=="1")
dur2_index <- which(Table4$OrderDurationCode=="2")
dur3_index <- which(Table4$OrderDurationCode=="3")
dur4_index <- which(Table4$OrderDurationCode=="4")
dur5_index <- which(Table4$OrderDurationCode=="5")
dur6_index <- which(Table4$OrderDurationCode=="6")
dur7_index <- which(Table4$OrderDurationCode=="7")
dur8_index <- which(Table4$OrderDurationCode=="8")
dur9_index <- which(Table4$OrderDurationCode=="9")
dur11_index <- which(Table4$OrderDurationCode=="11")
dur12_index <- which(Table4$OrderDurationCode=="12")
dur14_index <- which(Table4$OrderDurationCode=="14")
dur15_index <- which(Table4$OrderDurationCode=="15")

durw1_index <- which(Table4$OrderDurationCode=="W1")
durw2_index <- which(Table4$OrderDurationCode=="W2")
durw3_index <- which(Table4$OrderDurationCode=="W3")
durw4_index <- which(Table4$OrderDurationCode=="W4")
durw5_index <- which(Table4$OrderDurationCode=="W5")
durw6_index <- which(Table4$OrderDurationCode=="W6")
durw8_index <- which(Table4$OrderDurationCode=="W8")
durX0_index <- which(Table4$OrderDurationCode=="X0")
durX6_index <- which(Table4$OrderDurationCode=="X6")

durm1_index <- which(Table4$OrderDurationCode=="M1")
durm2_index <- which(Table4$OrderDurationCode=="M2")
durm3_index <- which(Table4$OrderDurationCode=="M3")
durm4_index <- which(Table4$OrderDurationCode=="M4")
durm5_index <- which(Table4$OrderDurationCode=="M5")
durm6_index <- which(Table4$OrderDurationCode=="M6")

# Convert all duration format to days
Table4[dur1_index,]$Duration <- 1
Table4[dur2_index,]$Duration <- 2
Table4[dur3_index,]$Duration <- 3
Table4[dur4_index,]$Duration <- 4
Table4[dur5_index,]$Duration <- 5
Table4[dur6_index,]$Duration <- 6
Table4[dur7_index,]$Duration <- 7
Table4[dur8_index,]$Duration <- 8
Table4[dur9_index,]$Duration <- 9
Table4[dur11_index,]$Duration <- 11
Table4[dur12_index,]$Duration <- 12
Table4[dur14_index,]$Duration <- 14
Table4[dur15_index,]$Duration <- 15
Table4[durw1_index,]$Duration <- 7
Table4[durw2_index,]$Duration <- 14
Table4[durw3_index,]$Duration <- 21
Table4[durw4_index,]$Duration <- 28
Table4[durw5_index,]$Duration <- 35
Table4[durw6_index,]$Duration <- 42
Table4[durw8_index,]$Duration <- 56
Table4[durX0_index,]$Duration <- 70
Table4[durX6_index,]$Duration <- 112
Table4[durm1_index,]$Duration <- 28
Table4[durm2_index,]$Duration <- 56
Table4[durm3_index,]$Duration <- 84
Table4[durm4_index,]$Duration <- 112
Table4[durm5_index,]$Duration <- 140
Table4[durm6_index,]$Duration <- 168

# Convert frequency format to how many times a day
dailystr <- c("0", "D", "1", "9", "PD")
daily_index <- which(Table4$OrderFrequencyCode %in% dailystr)
twice_a_day_index <- which(Table4$OrderFrequencyCode == "2")
onceweekly_index <- which(Table4$OrderFrequencyCode == "U")
twiceweekly_index <- which(Table4$OrderFrequencyCode == "V")
thriceweekly_index <- which(Table4$OrderFrequencyCode == "W")
four_times_a_week_index <- which(Table4$OrderFrequencyCode == "X")
five_times_a_week_index <- which(Table4$OrderFrequencyCode == "Y")
six_times_a_week_index <- which(Table4$OrderFrequencyCode == "Z")
alternate_index <- which(Table4$OrderFrequencyCode == "A")
om1357_index <- which(Table4$OrderFrequencyCode == "OM1357")
om246_index <- which(Table4$OrderFrequencyCode == "OM246")

Table4[daily_index,]$Frequency <- 1 # Once a day
Table4[twice_a_day_index,]$Frequency <- 2 # Once a day
Table4[onceweekly_index,]$Frequency <- 1/7 # Once a day
Table4[twiceweekly_index,]$Frequency <- 2/7 # Once a day
Table4[thriceweekly_index,]$Frequency <- 3/7 # Once a day
Table4[four_times_a_week_index,]$Frequency <- 4/7 # Once a day
Table4[five_times_a_week_index,]$Frequency <- 5/7 # Once a day
Table4[six_times_a_week_index,]$Frequency <- 6/7 # Once a day
Table4[alternate_index,]$Frequency <- 3/7 # Once a day
Table4[om1357_index,]$Frequency <- 4/7 # Once a day
Table4[om246_index,]$Frequency <- 3/7 # Once a day

Table4$SED <- Table4$DispenseDosageAmount * Table4$Frequency * Table4$Duration