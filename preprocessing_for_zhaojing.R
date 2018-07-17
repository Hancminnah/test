rm(list=ls())
library(data.table)
library(gdata)
setwd('C:/Users/mmchan/Desktop/MinMin/GeneralComplication/')

# Flags
fsave = 0

# Extract all cases, irregardless of whether patient went thru surgical procedures before
dvtindex <-fread("./Deidentified_Extracted_Data/cmm_dvt.csv")
CASE_PATIENT_ID = dvtindex
setnames(CASE_PATIENT_ID, old = c("DeIdentifiedNRIC") , new = c("IDNumber"))
mismatch_desc = c("MVGNMSXHHVx574","IJAOGGXDHOx412","LXANLUXHKTx362","JCWOMBWMFVx231","NTCPIQXLMNx290","ONHONNXEIUx393","NPCOPMXJKRx386","KFHOJFXFKNx338","IAZNCYWIHUx413") # Patient IDs where allindex's DIAGNOSIS_DESC does not match ds_diag's DiagnosisDescription [Refer to "ds_diag_DiagnosisDescription_allindex_DIAGNOSISDESC_mismatch.xlsx"]
mismatch_desc_idx = match(mismatch_desc,CASE_PATIENT_ID$IDNumber)
mismatch_desc_idx<- na.omit(mismatch_desc_idx)
CASE_PATIENT_ID<- CASE_PATIENT_ID[-mismatch_desc_idx,] # Only DVT data, but some dvt patients are also hap and bedsore patients

ds_diag <- fread("./Deidentified_Extracted_Data/case_ds_diag.csv")
ds_diag <- subset(ds_diag, DeIdentifiedNRIC %in% CASE_PATIENT_ID$IDNumber)
ds_diag <- unique(ds_diag)
length(unique(ds_diag$CaseIdentificationNumber))

dsbrief <- fread("./Deidentified_Extracted_Data/case_dsbrief_v4.csv")
dsbrief <- subset(dsbrief, DeIdentifiedNRIC %in% CASE_PATIENT_ID$IDNumber)
dsbrief[which(dsbrief$complicationType=="hap"),]$complicationType =""
dsbrief[which(dsbrief$complicationType=="bedscore"),]$complicationType =""
dsbrief <- unique(dsbrief)
length(unique(dsbrief$CaseIdentificationNumber))

prescribedmedication <- fread("./Deidentified_Extracted_Data/case_prescribedmedication_mapped.csv")
prescribedmedication <- subset(prescribedmedication, DeIdentifiedNRIC %in% CASE_PATIENT_ID$IDNumber)
prescribedmedication <- unique(prescribedmedication)
length(unique(prescribedmedication$CaseIdentificationNumber))


#dsnew <- read.xls("C:/Users/mmchan/Desktop/MinMin/GeneralComplication/Deidentified_Extracted_Data/akidata/case_dsnew_mapped.xlsx")
#setDT(dsnew)
#dsnew <- subset(dsnew, DeIdentifiedNRIC %in% CASE_PATIENT_ID$IDNumber)
#dsnew <- unique(dsnew)
#length(unique(dsnew$CaseIdentificationNumber))

# Add Label to Dsbrief
dsbrief[,("Id"):=NULL]

#dupCASEID <- which(duplicated(birthdate_genderdesc[, .(DeIdentifiedNRIC, BirthDateTime)])) # some nric are duplicated, with genderdesc as missing field
# Find all duplicatedIDs with and without genderdesc, then remove those without genderdesc
length(unique(dsbrief[which(dsbrief$DIAGNOSIS_DESC!=""),]$DeIdentifiedNRIC))
birthdate_genderdesc <- prescribedmedication[,.(DeIdentifiedNRIC,BirthDateTime,GenderDesc)]
birthdate_genderdesc <- unique(birthdate_genderdesc)
dupfield <- duplicated(birthdate_genderdesc[, .(DeIdentifiedNRIC, BirthDateTime)]) | duplicated(birthdate_genderdesc[, .(DeIdentifiedNRIC, BirthDateTime)], fromLast=TRUE)
dupfield_idx_TRUE <- which(dupfield==TRUE)
dupfield_idx_FALSE <- which(dupfield==FALSE)
dupfield_idx_to_remove <- which(birthdate_genderdesc[dupfield_idx_TRUE]$GenderDesc=="")
birthdate_genderdesc_final <- birthdate_genderdesc[c(dupfield_idx_TRUE[-dupfield_idx_to_remove],dupfield_idx_FALSE)]
dupfield_blah <- duplicated(birthdate_genderdesc_final[, .(DeIdentifiedNRIC, BirthDateTime)]) | duplicated(birthdate_genderdesc_final[, .(DeIdentifiedNRIC, BirthDateTime)], fromLast=TRUE) # Check if there are any duplicates again, shoul be don't have
birthdate_genderdesc_final

# Merge BirthDateTime, GenderDesc to dsbrief
# Obtain Main Table
Table1 <- merge(x = dsbrief, y = birthdate_genderdesc_final, by = "DeIdentifiedNRIC", all.x = TRUE)
#Table1 <- unique(subset(Table1, CaseIdentificationNumber %in% ds_diag$CaseIdentificationNumber)) # Make sure that CaseIDs in dsbrief and dsdiag is the same
Table1 <- Table1[order(Table1[,1],Table1[,3]),] # Order by DeIdentifiedNRIC then AdmitDateTime. Note that it is important that AdmitDateTime is ordered
Table1 <- unique(Table1)

Table1$AgeAtTimeOfVisit <- ""
ind_bdt <- which(nchar(Table1$BirthDateTime)>0)
ind_adt <- which(nchar(Table1$AdmitDateTime)>0)
ind_bdt <- ind_bdt[which(is.na(match(ind_bdt,ind_adt))==FALSE)]
Table1[ind_bdt,]$AgeAtTimeOfVisit <- floor(age_calc(as.Date(Table1[ind_bdt,]$BirthDateTime),as.Date(Table1[ind_bdt,]$AdmitDateTime),units = "years"))

if (fsave == 1) {
  fwrite(Table1,"C:/Users/mmchan/Desktop/MinMin/GeneralComplication/Preprocessing/data_for_zhaojing/maintable.csv")
}

# Note that data only contains dvt complicationType labels

# Obtain Table 2 with CaseIDs and Diagnosis Descriptions
load("C:/Users/mmchan/Desktop/MinMin/GeneralComplication/WorkFlow/FillICDCodes/FillICDCode_function.rda")
Table2 <- unique(ds_diag[,.(DeIdentifiedNRIC, CaseIdentificationNumber, DiagnosisDescription)])
Table2 <- Table2[order(Table2[,1]),]
Table2 <- FillICDCode(Table2)
if (fsave == 1) {
  fwrite(Table2,"C:/Users/mmchan/Desktop/MinMin/GeneralComplication/Preprocessing/data_for_zhaojing/diag_table.csv")
}


# Obtain Table 3 with CaseIDs and procedurecodes
procedure <- fread("./Deidentified_Extracted_Data/case_procedure_mapped.csv")
Table3 <- unique(subset(procedure, DeIdentifiedNRIC %in% CASE_PATIENT_ID$IDNumber)) # Good to check whether the number of NRIC in procedure is the same as dsbrief/dsdiag
Table3 <- Table3[,.(DeIdentifiedNRIC, CaseIdentificationNumber, ProcedureCatalogCode, ProcedureCode, ProcedureBeginDateTime, ProcedureEndDateTime)]
Table3 <- Table3[order(Table3[,1]),]
Table3 <- unique(Table3)
if (fsave == 1) {
  fwrite(Table3,"C:/Users/mmchan/Desktop/MinMin/GeneralComplication/Preprocessing/data_for_zhaojing/procedure_table.csv")
}


load("C:/Users/mmchan/Desktop/MinMin/GeneralComplication/WorkFlow/Procedures/FillProcedureClass_function.rda")
procedure_table<- fread("C:/Users/mmchan/Desktop/MinMin/GeneralComplication/Preprocessing/codes_to_preprocess_data_for_zhaojing/procedure_table.csv")
procedure_table_control<- fread("C:/Users/mmchan/Desktop/MinMin/GeneralComplication/Preprocessing/codes_to_preprocess_data_for_zhaojing/procedure_table_control.csv")
output_procedure <- FillProcedureClass(procedure_table,procedure_table_control)
procedure_table <- output_procedure[[1]]
procedure_table_control <- output_procedure[[2]]
procedure_table_final <- rbind(procedure_table,procedure_table_control)

if (fsave == 1) {
  fwrite(procedure_table,"C:/Users/mmchan/Desktop/MinMin/GeneralComplication/Preprocessing/codes_to_preprocess_data_for_zhaojing/procedure_table.csv")
  fwrite(procedure_table_control,"C:/Users/mmchan/Desktop/MinMin/GeneralComplication/Preprocessing/codes_to_preprocess_data_for_zhaojing/procedure_table_control.csv")
  fwrite(procedure_table_final,"C:/Users/mmchan/Desktop/MinMin/GeneralComplication/Preprocessing/codes_to_preprocess_data_for_zhaojing/procedure_table_final.csv")
}


# Preprocessing Medication Features
medicationorder <- fread('./Deidentified_Extracted_Data/case_medicationorder_mapped.csv')
curate_index <- c(which(medicationorder$CaseIdentificationNumber=="20042333216Z" & medicationorder$DrugName == "WARFARIN"),which(medicationorder$CaseIdentificationNumber=="1540450245" & medicationorder$DrugName == "WARFARIN"))
medicationorder[curate_index,19:24]<- medicationorder[curate_index,20:25]
medicationorder[curate_index,]$FindingCode <- ""

premarin_index <- which(medicationorder$DrugName == "PREMARIN VAGINAL"| medicationorder$DrugName=="PREMARIN")
warfarin_index <- which(medicationorder$DrugName == "WARFARIN")
vagifem_index <- which(medicationorder$DrugName == "VAGIFEM")
oestradiol_index <- which(medicationorder$DrugName == "OESTRADIOL" | medicationorder$DrugName == "OESTRADIOL VAGINAL")
conj_oestrogen_index <- which(medicationorder$DrugName == "CONJ OESTROGEN")

# Also need to think whether we need to convert premarin, vagifem, oestradiol and conj_oestrogen to standard equivalent dosage
medicationorder[vagifem_index,]$DispenseDosageAmount <- medicationorder[vagifem_index,]$DrugStrengthAmount # Since vagifem comes in tablet form in an applicator, i am assuming that the dispense dosage amount reflects the drug strength amount
medicationorder[oestradiol_index,]$DispenseDosageAmount <- medicationorder[oestradiol_index,]$DrugStrengthAmount # Since oestradiol/oestradiol vaginal tablet comes in tablet form in an applicator, i am assuming that the dispense dosage amount reflects the drug strength amount
medicationorder[premarin_index,]$DispenseDosageAmount <- "2G"# When it says "apply thinly",  I am assuming the premarin cream dispense dosage is 2G.
medicationorder[conj_oestrogen_index,]$DispenseDosageAmount <- "2G"# Dispense dosage amount is 2G as it is.


Table4 <- medicationorder[c(warfarin_index,premarin_index,vagifem_index,oestradiol_index,conj_oestrogen_index),
                          .(DeIdentifiedNRIC, CaseIdentificationNumber,
                            DrugCode, DrugName, DrugStrengthAmount, DrugStrengthDescription,
                            DrugFormCode,DrugFormDescription,
                            OrderFrequencyCode,OrderFrequencyDescription,OrderDurationCode,OrderDurationCodeDescription,
                            DispenseDosageAmount,FindingCode)]



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
dur10_index <- which(Table4$OrderDurationCode=="10")
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
Table4$DayDuration <- ""
Table4[dur1_index,]$DayDuration <- 1
Table4[dur2_index,]$DayDuration <- 2
Table4[dur3_index,]$DayDuration <- 3
Table4[dur4_index,]$DayDuration <- 4
Table4[dur5_index,]$DayDuration <- 5
Table4[dur6_index,]$DayDuration <- 6
Table4[dur7_index,]$DayDuration <- 7
Table4[dur8_index,]$DayDuration <- 8
Table4[dur9_index,]$DayDuration <- 9
Table4[dur10_index,]$DayDuration <- 10
Table4[dur11_index,]$DayDuration <- 11
Table4[dur12_index,]$DayDuration <- 12
Table4[dur14_index,]$DayDuration <- 14
Table4[dur15_index,]$DayDuration <- 15
Table4[durw1_index,]$DayDuration <- 7
Table4[durw2_index,]$DayDuration <- 14
Table4[durw3_index,]$DayDuration <- 21
Table4[durw4_index,]$DayDuration <- 28
Table4[durw5_index,]$DayDuration <- 35
Table4[durw6_index,]$DayDuration <- 42
Table4[durw8_index,]$DayDuration <- 56
Table4[durX0_index,]$DayDuration <- 70
Table4[durX6_index,]$DayDuration <- 112
Table4[durm1_index,]$DayDuration <- 28
Table4[durm2_index,]$DayDuration <- 56
Table4[durm3_index,]$DayDuration <- 84
Table4[durm4_index,]$DayDuration <- 112
Table4[durm5_index,]$DayDuration <- 140
Table4[durm6_index,]$DayDuration <- 168

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

Table4$Frequency <- ""
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

premarin_index <- which(Table4$DrugName == "PREMARIN VAGINAL"| Table4$DrugName=="PREMARIN")
warfarin_index <- which(Table4$DrugName == "WARFARIN")
vagifem_index <- which(Table4$DrugName == "VAGIFEM")
oestradiol_index <- which(Table4$DrugName == "OESTRADIOL" | Table4$DrugName == "OESTRADIOL VAGINAL")
conj_oestrogen_index <- which(Table4$DrugName == "CONJ OESTROGEN")

Table4$DosageAmt <- ""
# Preprocessing for DispenseDosageAmount
for (ii in 1:dim(Table4)[1]){
  dispense_dosage_amt = Table4[ii,]$DispenseDosageAmount
  
  amt <- strsplit(gsub("[^0-9\\.]"," ",dispense_dosage_amt)," ")
  amt <- as.numeric(lapply(amt, function(y){y[!y ==""]})[[1]][1])
  out <- strsplit(gsub("[[:punct:]]|[[:digit:]]"," ",dispense_dosage_amt)," ")
  out_unit <- lapply(out, function(y){y[!y ==""]})[[1]][1]
  if (is.na(amt) | is.na(out_unit)) {next} 
  Table4[ii,]$DosageAmt <- amt
}

Table4$DosageAmount <- as.numeric(Table4$DosageAmt)
Table4$Frequency <- as.numeric(Table4$Frequency)
Table4$DayDuration <- as.numeric(Table4$DayDuration)
Table4$SED <- Table4$DosageAmount * Table4$Frequency * Table4$DayDuration

# Remove those fields that is NA
Table4 <- Table4[-which(is.na(Table4$SED)),]
if (fsave == 1) {
  fwrite(Table4,"C:/Users/mmchan/Desktop/MinMin/GeneralComplication/Preprocessing/codes_to_preprocess_data_for_zhaojing/medication_table.csv")
}

## Preprocessing Labtest Features
labtest <- fread('./Deidentified_Extracted_Data/case_labtest_observation.csv')

# Maybe good to check if there are other labtest descriptions with the same procedure code
# Checked. Seems like ProcedureCodeDescription and ProcedureCode is unique to each other (One-to-One mapping)
dimer_index <- which(labtest$ProcedureCodeDescription == "D-Dimer")
albumin_index <- which(labtest$ProcedureCodeDescription == "Albumin")
antithrombin3_index <- which(labtest$ProcedureCodeDescription == "Anti-Thrombin III")
protein_c_index <- which(labtest$ProcedureCodeDescription == "Protein C Activity")
protein_s_index <- which(labtest$ProcedureCodeDescription == "Protein S Activity")
c_reactive_proc_index <- which(labtest$ProcedureCodeDescription == "C-Reactive Protein")
prothrombin_index <- which(labtest$ProcedureCodeDescription == "Prothrombin 20210A G")
esr_index <- which(labtest$ProcedureCodeDescription == "ESR")
factor_v_leiden_index <- which(labtest$ProcedureCodeDescription == "Factor V Leiden Gene" | labtest$ProcedureCodeDescription == "Factor V Leiden")
homocystein_index <- which(labtest$ProcedureCodeDescription == "Homocysteine, P")

Table5 <- labtest[c(dimer_index,albumin_index,antithrombin3_index,protein_c_index,protein_s_index,c_reactive_proc_index,prothrombin_index,esr_index,factor_v_leiden_index,homocystein_index),.(DeIdentifiedNRIC, CaseIdentificationNumber, ProcedureCode, ProcedureCodeDescription, ProcedureExtentDescription, FindingAmount, FindingAmountUnitCode, FindingComments)]

dimer_index <- which(Table5$ProcedureCodeDescription == "D-Dimer")
albumin_index <- which(Table5$ProcedureCodeDescription == "Albumin")
antithrombin3_index <- which(Table5$ProcedureCodeDescription == "Anti-Thrombin III")
protein_c_index <- which(Table5$ProcedureCodeDescription == "Protein C Activity")
protein_s_index <- which(Table5$ProcedureCodeDescription == "Protein S Activity")
c_reactive_proc_index <- which(Table5$ProcedureCodeDescription == "C-Reactive Protein")
prothrombin_index <- which(Table5$ProcedureCodeDescription == "Prothrombin 20210A G")
esr_index <- which(Table5$ProcedureCodeDescription == "ESR")
factor_v_leiden_index <- which(Table5$ProcedureCodeDescription == "Factor V Leiden Gene" | Table5$ProcedureCodeDescription == "Factor V Leiden")
homocystein_index <- which(Table5$ProcedureCodeDescription == "Homocysteine, P")

Table5$LabSym <- ""
Table5$LabValue <- ""
Table5$LabUnit <- ""

for (k in 1:dim(Table5)[1]) {
  lab_obs <- Table5[k,]$FindingAmount
  lab_unit <- Table5[k,]$FindingAmountUnitCode
  amt <- strsplit(gsub('[^0-9\\.]'," ",lab_obs)," ")
  amt <- as.numeric(lapply(amt, function(y){y[!y ==""]})[[1]][1])
  sym <- strsplit(gsub("[^[:^punct:];<&>]|[[:digit:]]", " ", lab_obs, perl = TRUE)," ")
  sym <- lapply(sym, function(y){y[!y ==""]})[[1]][1]
  if (is.na(amt) & is.na(sym) & lab_obs == ".") {
    amt <-"Not Normal"
    sym <- ""}
  if (is.na(amt) & is.na(sym) & lab_obs != ".") {next}
  if (is.na(amt) & sym == "INV") {next}
  if (is.na(amt) & sym == "Normal") {
    amt <-"Normal"
    sym <- ""}
  if (lab_unit == "ng/mL") {
    amt <- amt/1000
    lab_unit <- "ug/mL"}
  if (is.na(sym)==FALSE) {
    if (sym==">" | sym == "&gt") { amt <-  amt + 1}
    if (sym=="<" | sym == "&lt") { amt <-  amt - 1}
  }
  Table5[k,]$LabSym <- sym
  Table5[k,]$LabValue <- amt
  Table5[k,]$LabUnit <- lab_unit
}



# Is it possible that there is case number that is in procedure but not in ds_diag? Yes #haha = match(unique(procedure$CaseIdentificationNumber),unique(Table2$CaseIdentificationNumber))
# Only 937 out of 1891 NRICs has procedure data available,
# Out of 937 NRICs, only 188 NRICs truely has 281 procedure codes available, what do the other NRIC data contain?
# Even with the 188 NRICs that contain procedure codes right, they have Case numbers which is not found in ds_diag & vice versa

# NRICprocedure <- unique(Table3[which(Table3$ProcedureCode != ""),]$DeIdentifiedNRIC)
# sampleTable3<- subset(Table3, DeIdentifiedNRIC %in% NRICprocedure)
# ds_diag_sample <- subset(ds_diag,DeIdentifiedNRIC %in% NRICprocedure)

# Need to do Procedure Class and Major Minor Surgery


# Case IDs for prescribedmedication > dsbrief> dsdiag > dsnew
# NRIC for dsnew < prescribedmedication < dsbrief = dsdiag


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


D-Dimer Levels
unique(Table5[dimer_index,]$FindingAmount)
[1] "3.4"      "2"        "3.9"      "3.1"      "&gt; 4.0" "2.4"
[7] "3.2"      "311.4"    "> 2000.0" "1.5"      "> 4.0"    "3.8"
[13] "2.1"      "1.7"      "1.6"      "0.9"      "2.2"      "2.6"
[19] "1.9"      "0.7"      "0.8"      "&lt; 0.5" "1"        "3.5"
[25] "2.8"      "2.5"      "3"        "1.1"      "1.4"      "1.3"
[31] "1.2"      "2.7"      "3.6"
> unique(Table5[dimer_index,]$FindingAmountUnitCode)
[1] "ug/mL" "ng/mL"

Albumin Levels
unique(Table5[albumin_index,]$FindingAmount)
[1] "34"  "38"  "35"  "42"  "41"  "39"  "44"  "37"  "36"  "28"  "22"  "23"
[13] "21"  "29"  "25"  "26"  "32"  "24"  "27"  "30"  "12"  "31"  "40"  "33"
[25] "20"  "17"  "43"  "18"  "19"  "15"  "47"  "60"  "45"  "46"  "48"  "14"
[37] "16"  "INV" "50"  "52"  "13"  "51"  "49"
unique(Table5[albumin_index,]$FindingAmountUnitCode)
[1] "g/L" ""

Antithrombin levels
> unique(Table5[antithrombin3_index,]$FindingAmountUnitCode)
[1] "%"
> unique(Table5[antithrombin3_index,]$FindingAmount)
[1] "102" "86"  "104" "84"  "85"  "99"  "103" "93"  "92"  "87"  "91"  "100"
[13] ""    "109" "98"  "97"  "94"  "72"  "116" "88"  "111" "61"  "115" "96"


Protein C Levels
> unique(Table5[protein_c_index,]$FindingAmount)
[1] "47"       "118"      "&gt; 150" "78"       "42"       "25"
[7] "93"       "107"      "79"       "82"       "81"       "122"
[13] "95"       "124"      "117"      ""         "88"       "111"
[19] "92"       "134"      "56"       "80"       "57"       "132"
[25] "146"      "50"       "74"       "87"       "58"
> unique(Table5[protein_c_index,]$FindingAmountUnitCode)
[1] "%"

Protein S Levels
> unique(Table5[protein_s_index,]$FindingAmount)
[1] "42"  "122" "997" "58"  "36"  "43"  "57"  "76"  "72"  "92"  "84"  "65"
[13] "87"  "110" ""    "102" "75"  "129" "91"  "61"  "93"  "86"  "28"  "71"
[25] "13"
> unique(Table5[protein_s_index,]$FindingAmountUnitCode)
[1] "%"

C Reactive Protein Levels
> unique(Table5[c_reactive_proc_index,]$FindingAmountUnitCode)
[1] "mg/L" ""

ProThrombin Levels
> unique(Table5[prothrombin_index,]$FindingAmountUnitCode)
[1] ""
> unique(Table5[prothrombin_index,]$FindingAmount)
[1] "."      "Normal"

ESR Levels
> unique(Table5[esr_index,]$FindingAmountUnitCode)
[1] "mm/hr"
> unique(Table5[esr_index,]$FindingAmount)
[1] "79"       "45"       "103"      "116"      "47"       "80"
[7] "99"       "73"       "72"       "58"       "54"       "44"
[13] "82"       "74"       "59"       "49"       "81"       "119"
[19] "50"       "15"       "48"       "18"       "115"      "124"
[25] "125"      "126"      "92"       "98"       "106"      "32"
[31] "69"       "62"       "39"       "29"       "56"       "31"
[37] "65"       "77"       "101"      "97"       "22"       "36"
[43] "53"       "43"       "19"       "23"       "52"       "107"
[49] "94"       "78"       "108"      "121"      "10"       "8"
[55] "51"       "120"      "136"      "64"       "109"      "57"
[61] "104"      "63"       "88"       "100"      "90"       "68"
[67] "83"       "93"       "41"       "42"       "70"       "37"
[73] "33"       "16"       "85"       "25"       "91"       "14"
[79] "11"       "17"       "38"       "6"        "9"        "24"
[85] "4"        "7"        "1"        "84"       "13"       "5"
[91] "3"        "2"        "132"      "123"      "61"       "87"
[97] "86"       "75"       "89"       "102"      "114"      "110"
[103] "112"      "105"      "131"      "95"       "67"       "76"
[109] "55"       "40"       "128"      "117"      "96"       "34"
[115] "66"       "26"       "60"       "30"       "27"       "122"
[121] "&gt; 145" "46"       "35"       "129"      "71"

Factor V Leiden Levels
> unique(Table5[factor_v_leiden_index,]$FindingAmount)
[1] "."      "Normal"

> unique(Table5[factor_v_leiden_index,]$FindingAmountUnitCode)
[1] ""


Homocystein Levels
> unique(Table5[homocystein_index,]$FindingAmount)
[1] "21.2" "26.9" "14.2" "17.6" "11.5" "10.1" "9.4"  "8.8"  "12.6" "14"
[11] "8.1"  "10.9" "9.5"  "35.5" "15.9" "21.3" "26.7" "18.3" "20.8" "12.1"
[21] "14.5" "9"    "11.8" "19"   "7.4"  "26.5" "5.6"  "4.9"  "7.9"  "8.2"
[31] "11.3" "15"
> unique(Table5[homocystein_index,]$FindingAmountUnitCode)
[1] "umol/L"
