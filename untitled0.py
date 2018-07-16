# -*- coding: utf-8 -*-
"""
Created on Mon Jul 16 16:58:16 2018

@author: AdminCOOP
References: http://pbpython.com/pandas-pivot-table-explained.html
          : https://stackoverflow.com/questions/39229005/pivot-table-no-numeric-types-to-aggregate
"""

import pandas as pd
tempdata = pd.read_csv('C:/Users/AdminCOOP/Desktop/temp/temp.csv')

tempdata1 = tempdata.loc[tempdata['ProcedureCodeDescription'].isin(['Prothrombin 20210A G','Factor V Leiden'])]
tempdata2 = tempdata.loc[tempdata['ProcedureCodeDescription'].isin(['D-Dimer', 'Albumin', 'Anti-Thrombin III', 'Protein C Activity', 'Protein S Activity', 'C-Reactive Protein', 'ESR', 'Homocysteine,  P'])]
tempdata2['LabValue'] = pd.to_numeric(tempdata2['LabValue'])
tempdata1_pivot = tempdata1.pivot_table(index = 'CaseIdentificationNumber',
               values = 'LabValue',
               columns = 'ProcedureCodeDescription',aggfunc='first') # Note that there can be more than 1 labtest observation values in a case visit. I took the mean here. Zhaojing mentions majority voting or split it up into High, Normal Low.
tempdata2_pivot = tempdata2.pivot_table(index = 'CaseIdentificationNumber',
               values = 'LabValue',
               columns = 'ProcedureCodeDescription',aggfunc='mean') # Note that there can be more than 1 labtest observation values in a case visit. I took the mean here. Zhaojing mentions majority voting or split it up into High, Normal Low.

tempdata1_pivot.to_csv('C:/Users/AdminCOOP/Desktop/temp/pivoted_tablev1.csv')
tempdata2_pivot.to_csv('C:/Users/AdminCOOP/Desktop/temp/pivoted_tablev2.csv')

dsmed = bmcpiv.merge(cdrugs,on=['DeIdentifiedNRIC'])
