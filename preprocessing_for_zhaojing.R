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
factor_v_leiden_index <- which(Table5$ProcedureCodeDescription == "Factor V Leiden Gene" | labtest$ProcedureCodeDescription == "Factor V Leiden")
homocystein_index <- which(Table5$ProcedureCodeDescription == "Homocysteine, P")


amt <- strsplit(gsub('[^0-9\\.]'," ","> 2000.0")," ")
amt <- as.numeric(lapply(amt, function(y){y[!y ==""]})[[1]][1])
sym <- strsplit(gsub("[^[:^punct:]<&>]|[[:digit:]]", " ", "< 2000.0", perl = TRUE)," ")
sym <- lapply(sym, function(y){y[!y ==""]})[[1]][1]
if (is.na(amt) | is.na(sym)) {next}

if (sym == "ng/mL") {
  amt <- amt/1000
  sym <- "ug/mL"}
if (sym==">" | sym == "&gt") { amt <-  amt + 1}
if (sym=="<" | sym == "&lt") { amt <-  amt - 1}
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