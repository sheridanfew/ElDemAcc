wd=$(pwd)

# This file extracts key pieces of information from the log files.
# Will need to replace "/Users/Shez/Google\ Drive/Grantham/ElDemAcc/" with path to local main directory throughout this file before running on other machines.
# The script defines scenario columns based on names of the files, this process would need amending for a different scenario basis.
# I found this script worked only if I selected "paste slowly" into the terminal

cd /Users/Shez/Google\ Drive/Grantham/ElDemAcc/CLOVER/Optimisation_Results/

# Write header to run_info file
echo 'Name,Growth Rate,Accessibility,Climate,PV size start (kW),PV size end (kW),Storage size first (kWh),Storage size first - post deg (kWh),Storage size last (kWh),Storage size last - post deg(kWh),LCUE ($/kWh),Initial Capital ($),Emissions Instensity (gCO2e/kWh),Self Consumption (%),Blackouts (%),Unmet Energy (%)' > /Users/Shez/Google\ Drive/Grantham/ElDemAcc/Outputs/CLOVER_run_info.csv

# Loop over all csvs in the directory
for file in *csv
do
	# Extract info from optimisation output file
	name=$(echo $file | sed 's/.csv//g')

	# Get first and last lines, break down into array by column
	first_line=$(head -n 2 $file | tail -n 1)
	first_line_arr=($(echo "$first_line" | sed 's/,/ /g'))
	last_line=$(tail -n 1 $file)
	last_line_arr=($(echo "$last_line" | sed 's/,/ /g'))
	
	# Get key numbers based on column number in oprimisation output file (would need amending if info coming out of an optimisation changed)
	PV_size_first=${first_line_arr[4-1]}
	PV_size_first_post_degrad=${first_line_arr[6-1]}
	PV_size_last=${last_line_arr[4-1]}
	PV_size_last_post_degrad=${last_line_arr[6-1]}
	Stor_size_first=${first_line_arr[5-1]}
	Stor_size_first_post_degrad=${first_line_arr[7-1]}
	Stor_size_last=${last_line_arr[5-1]}
	Stor_size_last_post_degrad=${last_line_arr[7-1]}
	LCUE=${last_line_arr[17-1]}
	LCSE=${last_line_arr[18-1]}
	Initial_capital=${first_line_arr[9-1]}
	Em_intens=${last_line_arr[19-1]}
	Unmet_energy=${last_line_arr[21-1]}
	Self_cons=${last_line_arr[22-1]}
	Blackouts=${last_line_arr[20-1]}

	# Identify scenario based on filename (growth rate, accessibility, climate)
	if [[ $name == *"NoGrowth"* ]]; then
		growth='None'
	elif [[ $name == *"slow_growth"* ]]; then
		growth='Slow'
	elif [[ $name == *"slowplus_growth"* ]]; then
		growth='SlowPlus'
	elif [[ $name == *"medium_growth"* ]]; then
		growth='Medium'
	elif [[ $name == *"medplus_growth"* ]]; then
		growth='Medplus'
	elif [[ $name == *"fast_growth"* ]]; then
		growth='Fast'
	elif [[ $name == *"faster_growth"* ]]; then
		growth='Faster'
	elif [[ $name == *"fastplus_growth"* ]]; then
		growth='FastPlus'
	fi

	if [[ $name == *"remote"* ]]; then
		accessibility='Rural'
	elif [[ $name == *"accessible"* ]]; then
		accessibility='Peri-urban'
	fi
	
	if [[ $name == *"HSub"* ]]; then
		climate='HSub'
	elif [[ $name == *"TSav"* ]]; then
		climate='TSav'
	fi

	# Output row in run_info file with relevant quantities
	echo "${name},${growth},${accessibility},${climate},${PV_size_first},${PV_size_last},${Stor_size_first},${Stor_size_first_post_degrad},${Stor_size_last},${Stor_size_last_post_degrad},${LCUE},${Initial_capital},${Em_intens},${Self_cons},${Blackouts},${Unmet_energy}" >> /Users/Shez/Google\ Drive/Grantham/ElDemAcc/Outputs/CLOVER_run_info.csv
done
