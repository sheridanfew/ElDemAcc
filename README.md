# ElDemAcc
Electricity Demand in an Energy Access Context

This repository contains scripts used to:
(1) simulate and analyse data in an electricity access context
(2) optimise a microgrid system to meet these loads aceoss scenarios

## Overview 

- Run specific scripts to generate CREST data, and process MEshpower, Gram Oorja, and CREST data, as oulined in separate secton
- Generate CLOVER profiles with load processing script (also generates input lines defining ranges considered in CLOVER optimistion based on max daily load by context)
- Analyse and produce plots based upon this data and results of simulations/optimisations
- Home pa
- NB. Names were changed for climates (’temperate’ -> tropical savannah’, ‘extreme’ -> ’humid substropical’) and rurality conditions (‘accessibility’ -> ’rurality’, ‘acessible’ -> ‘peri-urban’, ‘remote’ -> ‘rural’) relatively late, therefore while plots use the revised terminology, many variables use the old terminology.

## CREST Demand model

- An adapted version of the CREST demand model is used to simulate electricity demand for 25 households across:
	- five energy access tiers,
	- two climatic conditions
	- week day and weekedns
	- each month of the year

- These data are combined to give an annual hourly load profile across climatic conditions.

Steps to generate and analyse load profiles for new locations (NB. All scripts will need update of root directory to reflect that on the local computer):

In excel (Goal here was minimise effort in excel and carry out most processing using other more user-friendly tools):
- Add PV and temperature data for your location by exporting with metadata for your location from the renewables.ninja website. Add this to the excel workbook as a sheet entitled (Location)_ninja.
- If desired edit other inputs such as appliance ownership/technical characteristics by tier (2006 - 2026 appliance inputs used to reflect tiers 1 - 5) / household size distribution / other inputs to reflect the local context.
- Change ‘City’, ‘Local Time Meridian’, Latitude and longditude in main sheet to reflect your location (and number of dwellings and weekday/weekend if desired). In this case, Bhinjpur and Gitaraga represent extreme and temperate climates.
- Update the ‘Output path’ cell in the Main Sheet to reflect the location of the ElDemAcc/CREST/Excel_Outputs’ folder on your local computer
- Run the “Iterate and Export” macro to generate outputs for one day of each month 1 to 12 at each tier 1 - 5.

Post-excel:
- xls_to_csvs.sh: Shell script to update directory in second line to that of ‘Excel_Outputs’ on local dir
- Run shell script xls_to_csvs.sh to convert excel outputs to csvs and rename them
- Run process_CREST_excel_outputs.R to generate hourly load profiles across 2019 across business types and households
- This script also characterised constitutent loads according to a range of standard metrics


## Gram Oorja data

- Seasonal data on rice mills, water pumps, and fans are taken from Gram Oorja estimates
- These are processed into hourly load for an entire year by device, in a similar format to that used for CREST data, using the process_GO_data.R script
- This script also characterises constitutent loads according to a range of standard metrics

## Meshpower data

- In conducting this study, real measued data from domestic and business customers connected to the Gitaraga microgrid was analysed and used to inform design of a microgrid system.
- In order to maintain confidentiality, raw data is not included in this repository, only aggregate properties and load data for the microgrid as a whole. Please get in touch if you are interested in this data as I have developed some tools to download this from servers and convert it into a useable form (with Meshpower's permission)
- Data here have already undergone a process to obtain hourly loads per customer from the meshpower database (detailed elsewhere)
- process_Mespower_loads.R was used to generate processed loads from raw data, but will not work here as raw data is excluded (processed data is included to facilitate running of later scripts)

## Data processing

- Scripts to process, and produce plots based upon simulated and measured load data are included in the "Data_Processing" directory
- These lead to outputs in the "Outputs" and "Plots" directories


## CLOVER Microgrid Simulation and Optimisation (see also: CLOVER-hpc respository)

- CLOVER readable load profiles are generated in the "Outputs/" dir using the "make_and_plot_CLOVER_load_profiles.R" script
- These are included in CLOVER location files in the CLOVER directory.

- The script "make_demand_paper_jobs.sh" is to be run on the HPC to generate job files to optimise the system based upon each of the generated microgrid load profiles. (Instructions on steps to carry out before running this script are outlined in the header to the script).
- These are launched using commands in launch_demand_paper_CLOVER_jobs_hpc.sh (which will need to be edited for any new set of jobs, as described in the header to this script)
- Results should be copied back from the hpc into the "CLOVER/Optimisation_Results/" folder
- Results can be analysed using CLOVER_run_info_demand_paper.sh, which will need editing to reflect local paths and choice of scenarios. This generates a file "CLOVER_run_info_summarised.csv" in Outputs which is used for the next stage of processing
- CLOVER results are then copied back from HPC, analysed, and plots produced using the "Process_CLOVER_Outputs.R" script.
