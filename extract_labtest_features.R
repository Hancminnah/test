# To create extract medication features
# Features to extract: 1) conj oestrogen
#                      2) premarin / premarin vaginal
#                      3) vagifem
#                      4) oestradiol vaginal
#                      5) warfarin
library(data.table)
setwd('/data/volume/MinMin/GeneralComplication/Deidentified_Extracted_Data')
labtest <- fread('./case_labtest_observation.csv')

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

# Check Data
labtest[dimer_index,.(DeIdentifiedNRIC, CaseIdentificationNumber, ProcedureCode, ProcedureCodeDescription, ProcedureExtentDescription, FindingAmount, FindingAmountUnitCode, FindingComments)]
labtest[albumin_index,.(DeIdentifiedNRIC, CaseIdentificationNumber, ProcedureCode, ProcedureCodeDescription, ProcedureExtentDescription, FindingAmount, FindingAmountUnitCode, FindingComments)]
labtest[antithrombin3_index,.(DeIdentifiedNRIC, CaseIdentificationNumber, ProcedureCode, ProcedureCodeDescription, ProcedureExtentDescription, FindingAmount, FindingAmountUnitCode, FindingComments)]
labtest[protein_c_index,.(DeIdentifiedNRIC, CaseIdentificationNumber, ProcedureCode, ProcedureCodeDescription, ProcedureExtentDescription, FindingAmount, FindingAmountUnitCode, FindingComments)]
labtest[protein_s_index,.(DeIdentifiedNRIC, CaseIdentificationNumber, ProcedureCode, ProcedureCodeDescription, ProcedureExtentDescription, FindingAmount, FindingAmountUnitCode, FindingComments)]
labtest[c_reactive_proc_index,.(DeIdentifiedNRIC, CaseIdentificationNumber, ProcedureCode, ProcedureCodeDescription, ProcedureExtentDescription, FindingAmount, FindingAmountUnitCode, FindingComments)]
labtest[prothrombin_index,.(DeIdentifiedNRIC, CaseIdentificationNumber, ProcedureCode, ProcedureCodeDescription, ProcedureExtentDescription, FindingAmount, FindingAmountUnitCode, FindingComments)]
labtest[esr_index,.(DeIdentifiedNRIC, CaseIdentificationNumber, ProcedureCode, ProcedureCodeDescription, ProcedureExtentDescription, FindingAmount, FindingAmountUnitCode, FindingComments)]
labtest[factor_v_leiden_index,.(DeIdentifiedNRIC, CaseIdentificationNumber, ProcedureCode, ProcedureCodeDescription, ProcedureExtentDescription, FindingAmount, FindingAmountUnitCode, FindingComments)]
labtest[homocystein_index,.(DeIdentifiedNRIC, CaseIdentificationNumber, ProcedureCode, ProcedureCodeDescription, ProcedureExtentDescription, FindingAmount, FindingAmountUnitCode, FindingComments)]

Table5 <- medicationorder[c(dimer_index,albumin_index,antithrombin3_index,protein_c_index,protein_s_index,c_reactive_proc_index,prothrombin_index,esr_index,factor_v_leiden_index,homocystein_index),.(DeIdentifiedNRIC, CaseIdentificationNumber, ProcedureCode, ProcedureCodeDescription, ProcedureExtentDescription, FindingAmount, FindingAmountUnitCode, FindingComments)]

