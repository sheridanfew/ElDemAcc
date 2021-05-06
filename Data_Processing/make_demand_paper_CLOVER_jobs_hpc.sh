#!/bin/bash

# Generate CLOVER input files (max daily load used to calibrate limits for optimisation - values here are copied directly from R output)

cat <<EOF

This script is to be run on the HPC to generate job files

- This is to be run after process_CREST_excel_outputs.R and make_and_plot_CLOVER_load_profiles.R

- Before running, CLOVER load files should be manually be copied from Demand/Outputs/CLOVER_Loads to appropriate subdir of CLOVER location folder on HPC. eg. /home/spf310/CLOVER-hpc/Locations/DemandPaper/Load/Device\ load/"

- Entries to the max_daily_load array (below this message in this script)  should be replaced with those generated in the text output to make_and_plot_CLOVER_load_profiles.R

- CLOVER_dir variable may also need updating.

EOF

CLOVER_dir="/rds/general/user/${USER}/home/CLOVER-hpc/"

declare -A max_daily_load

max_daily_load[slow_growth_TSav_climate_accessible_total_load]=49363.6
max_daily_load[slow_growth_TSav_climate_remote_total_load]=74875.02
max_daily_load[slow_growth_HSub_climate_accessible_total_load]=79251.58
max_daily_load[slow_growth_HSub_climate_remote_total_load]=89272.78
max_daily_load[slowplus_growth_TSav_climate_accessible_total_load]=73241.94
max_daily_load[slowplus_growth_TSav_climate_remote_total_load]=91966.39
max_daily_load[slowplus_growth_HSub_climate_accessible_total_load]=177127.31
max_daily_load[slowplus_growth_HSub_climate_remote_total_load]=187148.52
max_daily_load[medium_growth_TSav_climate_accessible_total_load]=110163.53
max_daily_load[medium_growth_TSav_climate_remote_total_load]=119441.44
max_daily_load[medium_growth_HSub_climate_accessible_total_load]=276180.37
max_daily_load[medium_growth_HSub_climate_remote_total_load]=286201.58
max_daily_load[medplus_growth_TSav_climate_accessible_total_load]=126656.45
max_daily_load[medplus_growth_TSav_climate_remote_total_load]=135934.37
max_daily_load[medplus_growth_HSub_climate_accessible_total_load]=310780.83
max_daily_load[medplus_growth_HSub_climate_remote_total_load]=320802.03
max_daily_load[fast_growth_TSav_climate_accessible_total_load]=169303.3
max_daily_load[fast_growth_TSav_climate_remote_total_load]=178860.77
max_daily_load[fast_growth_HSub_climate_accessible_total_load]=335304.3
max_daily_load[fast_growth_HSub_climate_remote_total_load]=345325.5
max_daily_load[fastplus_growth_TSav_climate_accessible_total_load]=287901.15
max_daily_load[fastplus_growth_TSav_climate_remote_total_load]=297179.06
max_daily_load[fastplus_growth_HSub_climate_accessible_total_load]=990570.5
max_daily_load[fastplus_growth_HSub_climate_remote_total_load]=1000591.7
max_daily_load[faster_growth_TSav_climate_accessible_total_load]=358958.99
max_daily_load[faster_growth_TSav_climate_remote_total_load]=368236.91
max_daily_load[faster_growth_HSub_climate_accessible_total_load]=1496000.71
max_daily_load[faster_growth_HSub_climate_remote_total_load]=1506021.91

for load_scenario in "${!max_daily_load[@]}"; do
    echo "load_scenario: ${load_scenario}"
    echo "max_daily_load: ${max_daily_load[${load_scenario}]}"

    yearly_load_file=$(echo "${load_scenario}" | sed 's/_total_load/_yearly_load_stats/g')

    echo "yearly_load_file: $yearly_load_file"


    mkdir ${CLOVER_dir}/Jobs/DemandPaper_${load_scenario}

    cat > ${CLOVER_dir}/Jobs/DemandPaper_${load_scenario}/DemandPaper_${load_scenario}.py << EOF
# Import Packages
import time
import os

# Max daily load (calculated in R script externally, used to define limits of optimisation space)

Max_daily_load_Wh = ${max_daily_load[${load_scenario}]}
Max_daily_load_kWh = Max_daily_load_Wh/1000

location='DemandPaper'

# Go to CLOVER directory
#cd('/Users/Shez/Google Drive/Grantham/CLOVER/CLOVER-master/')

# Import scripts
print('Importing Scripts...')
exec(open("Scripts/Conversion_scripts/Conversion.py").read())
exec(open("Scripts/Generation_scripts/Diesel.py").read())
exec(open("Scripts/Generation_scripts/Grid.py").read())
exec(open("Scripts/Generation_scripts/Solar.py").read())
exec(open("Scripts/Load_scripts/Load.py").read())
exec(open("Scripts/Simulation_scripts/Energy_System.py").read())
exec(open("Scripts/Optimisation_scripts/Optimisation.py").read())

# Run optimisation - search ranges based on previous experience of optimal system sizes (but CLOVER will explore outside of this range if it doesn't find an optimal solution here.)
opt=Optimisation(location='DemandPaper',
optimisation_inputs=[
['PV size (min)',0],['PV size (max)',Max_daily_load_kWh/5],['PV size (step)',Max_daily_load_kWh/5/40],['PV size (increase)',Max_daily_load_kWh/5],
['Storage size (min)',0],['Storage size (max)',Max_daily_load_kWh],['Storage size (step)',Max_daily_load_kWh/40],['Storage size (increase)',Max_daily_load_kWh]],
load_override='${load_scenario}.csv',
yearly_load_stats_override='${yearly_load_file}.csv'
).multiple_optimisation_step()


Optimisation(location='DemandPaper').save_optimisation(opt,filename='${load_scenario}')

print('Done!')

EOF

done

