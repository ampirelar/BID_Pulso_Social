---------------------------------
Labor Discrimination/02_Elempleo_covid

Created: 	2020/03/18
Last Updated: 	2020/07/28
---------------------------------

This folder contains 

-- 00_Gen_DataSet.R: Process raw webscrapped data from elempleo.com created with Scrapping_web_cluster.py. The output are csv files with cleaned strings and additional information, as dates and municipality ids-- 00_Run.sh: This script iterates over csv elempleo datasets. It runs 00_Gen_DataSet.R-- 01_scrapping_computrabajo_complete.R: Webscrapp data using urls from computrabajo.com, that the script Scrapping_computrabajo.py retrieves.-- 02_gen_datasets.R: Organize datasets from elempleo and computrabajo into clean datasets. Factor variables are standardized between both engines. In this script we obtain cleaned datasets from computrabajo and Elempleo ready to analyze.-- 03_analysis.R: Creates graphs and statistics about elempleo and computrabajo.

-- 04_analysis_economic_sectors.R: This scripts is used specifically for statistics and graphs used for analyzing how the economic sectors offers' changed during different stages of the pandemic (Blog 1)

-- 05_geih_unemployment.R: this datasets uses GEIH information in order to obtain collapsed datasets with statistics as monthly unemployment rate, etc. The output is saved in a cleaned rds file called geih.rds

-- 06_geih_vs_elempleo.R: This script uses geih.rds and elempleo.rds to compare statistics from job offers and the labour market.-- Scrapping_computrabajo.py: retrieve  urls of job offers from computrabajo.com-- Scrapping_web_cluster.py: Webscrapp raw information from job offers in elempleo

-- 07_wage_contract.R: This scripts is used for statistics and graphs used for analyzing how the job offers' changed by education level, contract and wage range during different stages of the pandemic (Blog 2)

-- 08_statistics_regressions.R: This script is used for regressions and statistics of interest, such as average job offers per week.
-- 09_analysis_wages.R: This script graphs and estimates the job wage distribution in Colombia between 2019-2020 (pre-pandemic) using GEIH dataset. The information is used for Blog#2-- 10_scrapping_applicants.R: This script webscrapps data from Elempleo job applicants. Up until this last version the dataset hasn't been used.-- 11_analysis_applicants.R: This script analyses data from Elempleo job applicants. Up until this last version the graphs haven't been used.
-- 12_text_analysis.R: This script analyses how the requirements and expected abilities in the job offers from Elempleo have changed after the pandemic (it uses many many packages). Its output is used for constructing Blog#3

!!!!

---------------------------------

ORDER FOR ANALYZING ELEMPLEO JOB OFFERS: 
1- Scrapping_web_cluster.py
2- 00_Gen_DataSet.R runned by 00_Run.sh
3- 02_gen_datasets.R
4- 03_analysis.R
5- 05_geih_unemployment.R

---------------------------------