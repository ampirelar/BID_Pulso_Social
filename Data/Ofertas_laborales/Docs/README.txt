---------------------------------
Labor Discrimination/02_Elempleo_covid

Created: 	2020/03/18
Last Updated: 	2020/07/28
---------------------------------

This folder contains 

-- 00_Gen_DataSet.R: Process raw webscrapped data from elempleo.com created with Scrapping_web_cluster.py. The output are csv files with cleaned strings and additional information, as dates and municipality ids

-- 04_analysis_economic_sectors.R: This scripts is used specifically for statistics and graphs used for analyzing how the economic sectors offers' changed during different stages of the pandemic (Blog 1)

-- 05_geih_unemployment.R: this datasets uses GEIH information in order to obtain collapsed datasets with statistics as monthly unemployment rate, etc. The output is saved in a cleaned rds file called geih.rds

-- 06_geih_vs_elempleo.R: This script uses geih.rds and elempleo.rds to compare statistics from job offers and the labour market.

-- 07_wage_contract.R: This scripts is used for statistics and graphs used for analyzing how the job offers' changed by education level, contract and wage range during different stages of the pandemic (Blog 2)

-- 08_statistics_regressions.R: This script is used for regressions and statistics of interest, such as average job offers per week.



!!!!

---------------------------------

ORDER FOR ANALYZING ELEMPLEO JOB OFFERS: 
1- Scrapping_web_cluster.py
2- 00_Gen_DataSet.R runned by 00_Run.sh
3- 02_gen_datasets.R
4- 03_analysis.R
5- 05_geih_unemployment.R

---------------------------------