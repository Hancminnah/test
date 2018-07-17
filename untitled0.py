# -*- coding: utf-8 -*-
"""
Created on Mon Jul 16 16:58:16 2018

@author: AdminCOOP
References: http://pbpython.com/pandas-pivot-table-explained.html
          : https://stackoverflow.com/questions/39229005/pivot-table-no-numeric-types-to-aggregate
          : https://stackoverflow.com/questions/41463763/merge-2-dataframes-with-same-values-in-a-column
          : https://stackoverflow.com/questions/44937462/outer-merging-two-data-frames-in-place-in-pandas
"""

import pandas as pd
tempdata = pd.read_csv('C:/Users/AdminCOOP/Desktop/temp/temp.csv')

tempdata1 = tempdata.loc[tempdata['ProcedureCodeDescription'].isin(['Prothrombin 20210A G','Factor V Leiden'])]
tempdata2 = tempdata.loc[tempdata['ProcedureCodeDescription'].isin(['D-Dimer', 'Albumin', 'Anti-Thrombin III', 'Protein C Activity', 'Protein S Activity', 'C-Reactive Protein', 'ESR', 'Homocysteine,  P'])]
tempdata2['LabValue'] = pd.to_numeric(tempdata2['LabValue'])
tempdata1_pivot = tempdata1.pivot_table(index = 'CaseIdentificationNumber',
               values = 'LabValue',
               columns = 'ProcedureCodeDescription',aggfunc='first') # Note that there can be more than 1 labtest observation values in a case visit. I took the mean here. Zhaojing mentions majority voting or split it up into High, Normal Low.
tempdata1_pivot = tempdata1_pivot.reset_index()

tempdata2_pivot = tempdata2.pivot_table(index = 'CaseIdentificationNumber',
               values = 'LabValue',
               columns = 'ProcedureCodeDescription',aggfunc='mean') # Note that there can be more than 1 labtest observation values in a case visit. I took the mean here. Zhaojing mentions majority voting or split it up into High, Normal Low.
tempdata2_pivot = tempdata2_pivot.reset_index()

tempdata1_pivot.to_csv('C:/Users/AdminCOOP/Desktop/temp/pivoted_tablev1.csv')
tempdata2_pivot.to_csv('C:/Users/AdminCOOP/Desktop/temp/pivoted_tablev2.csv')

result = pd.merge(tempdata1_pivot, tempdata2_pivot, how='outer')

result.to_csv('C:/Users/AdminCOOP/Desktop/temp/pivoted_table_final.csv')

medication_table = pd.read_csv('/data/volume/MinMin/GeneralComplication/Preprocessing/temp_folder/medication_table.csv')
medication_table2 = medication_table[['DeIdentifiedNRIC','CaseIdentificationNumber','DrugName','SED']]
medication_table2['SED'] = pd.to_numeric(medication_table2['SED'])
medication_table_pivot = medication_table2.pivot_table(index = ['DeIdentifiedNRIC','CaseIdentificationNumber'], values = 'SED', columns = 'DrugName',aggfunc='mean',fill_value=0)

