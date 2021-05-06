# Script to process Gram Oorja data and generate annual hourly load for CLOVER input and analysis in later scripts
# Sheridan Few, Oct 2020
# See also readme file

### PACKAGES

library(data.table)
library(zoo)
#library(plyr)
library(dplyr)
library(lubridate)
library(timeDate)

### PATH DEFINITION

root_path <- '/Users/Shez/Google\ Drive/Grantham/ElDemAcc/'
input_path <- paste(root_path,'GramOorja/Input_data/',sep='')
output_path <- paste(root_path,'GramOorja/Processed_Load/',sep='')

climate_data_input_path <- paste(root_path,'Climate_data/',sep='')


### INPUT DATA
# SEASONAL DATA FOR BHINJPUR (based on GO estimates as defined in their microgrid sizing excel model)
Bhinjpur_Seasonal_Load_input <- paste(input_path,'Bhinjpur_Seasonal_Load.csv',sep='')

### INPUT VARIABLES

community='Bhinjpur'

# Devices to consider
devices<-c('Fan','Rice Mill','Irrigation Pump')

# Number of fans in Bhinjpur (used to normalise)

Nfans = 60

### FUNCTIONS

# DO STUFF

### 1. IMPORT AND CLEAN DATA

Bhinjpur_Seasonal_Load_df = read.csv(Bhinjpur_Seasonal_Load_input)

# Scale fan load to one fan (data is for all the fans in Bhinjpur in input data)

Bhinjpur_Seasonal_Load_df$hourly_usage_wh[which(Bhinjpur_Seasonal_Load_df$Device=="Fan")] <- Bhinjpur_Seasonal_Load_df$hourly_usage_wh[which(Bhinjpur_Seasonal_Load_df$Device=="Fan")] / Nfans

### 2. GENERATE ANNUAL (365 day) HOURLY LOAD PROFILE FOR EACH DEVICE IN BHINJPUR (based on GO estimates as defined in their microgrid sizing excel model)

# Generate df with a years worth of hours to add load data alongside

start_date <- dmy_hms("01/01/2019 00:00:00 AM")
end_date <- dmy_hms("31/12/2019 23:00:00 PM")

time_sequence <- seq(start_date,end_date, by = '1 hour')

time_data<-cbind(month=month(time_sequence),day=day(time_sequence),day_of_year=yday(time_sequence),weekday_status=isWeekday(time_sequence),hour_in_day=hour(time_sequence))

# Add hourly load data for each load (based on hour of day, month) and export

annual_hourly_load_by_device<-lapply(devices, function(device){
	# Get load for every hour of the year based on month & device
	hour_load<-apply(time_data, 1, function(x){
		month<-as.character(x[1])
		weekday_status<-x[3]
		hour<-x[['hour_in_day']]

		if(month <= 1 | month >= 10)
		{
			season = 'Oct - Jan'
		}
		if(month <= 5 & month >= 2)
		{
			season = 'Feb - May'
		}
		if(month <= 9 & month >= 6)
		{
			season = 'Jun - Sep'
		}

		return(Bhinjpur_Seasonal_Load_df$hourly_usage_wh[which(Bhinjpur_Seasonal_Load_df$Device==device & Bhinjpur_Seasonal_Load_df$Season==season)][hour+1])
	})
	df<-as.data.frame(cbind(time_data,hour_load))
	colnames(df)<-c('month','day','day_of_year','weekday_status','hour','load')
	write.table(df, paste(output_path,'Annual_Hourly_Load_By_Source/',gsub(' ','_',device),'_annual_hourly.csv',sep=''), sep=",", row.names=FALSE)
	return(df)
})	

names(annual_hourly_load_by_device)<-devices

### 3. CHARACTERISE LOADS

### 3a. AVERAGE DAILY ENERGY

daily_energy<-lapply(devices,function(device){
	return(sum(annual_hourly_load_by_device[[device]]$load)/365)
})

daily_energy_stdev_across_days<-lapply(devices,function(device){
	mean_by_day <- aggregate(annual_hourly_load_by_device[[device]][c('day_of_year','load')], by=list(annual_hourly_load_by_device[[device]]$day_of_year),FUN=sum)
	return(sd(mean_by_day$load))
})


### 3b. PEAK LOAD

peak_load<-lapply(devices,function(device){
	return(max(annual_hourly_load_by_device[[device]]$load))
})

### 3c.  LOAD FACTOR
# Average load over peak load

load_factor <- as.list((as.numeric(daily_energy)/24)/as.numeric(peak_load))

### 3d. COINCIDENCE FACTOR

# Not defined by user type - not enough instances

### 3e. PV OVERLAP FACTOR

# PV data:
PV_data_input<-paste(climate_data_input_path,community,'_climate.csv',sep='')
PV_data<-read.csv(PV_data_input,skip = 3, header = T)
PV_data['local_time'] <- lapply(PV_data['local_time'], strptime, "%m/%d/%Y %H:%M")
PV_data$hour_tot<-as.integer(as.numeric(PV_data$local_time)/3600) # track hours (absolute, ie each day adds 24)
PV_data$hour_in_day=PV_data$hour_tot%%24
PV_mean<-aggregate(electricity ~ hour_in_day, PV_data, mean)
PV_mean_normalised<-as.data.frame(cbind(hour_in_day=PV_mean$hour_in_day,electricity=PV_mean$electricity/sum(PV_mean$electricity)))

# Get overlap w PV data by hour for plotting purposes
hourly_pv_overlap_by_device<-lapply(devices,function(device){
	load_data_mean_day<-aggregate(load ~ hour, annual_hourly_load_by_device[[device]], mean)
	load_data_mean_day_normalised<-as.data.frame(cbind(hour=load_data_mean_day$hour,load=load_data_mean_day$load/sum(load_data_mean_day$load)))
	overlap=pmin(PV_mean_normalised$electricity,load_data_mean_day_normalised$load)
	return(overlap)
})

pv_overlap_by_device <- lapply(hourly_pv_overlap_by_device, sum)



# 3f. SEASONAL CONSISTENCY

# Quantify seasonal consistency (min(mean load over month) / max(mean load over month))

monthly_load_by_device<-lapply(devices,function(device){
	device_mean_by_month <- aggregate(annual_hourly_load_by_device[[device]][c('month','load')], by=list(annual_hourly_load_by_device[[device]]$month),FUN=mean)
	return(device_mean_by_month)
})

names(monthly_load_by_device)<-devices

seasonal_consistency_by_device<-lapply(devices,function(device){
	ordered_load<-monthly_load_by_device[[device]][order(monthly_load_by_device[[device]]$load),]
	return(sum(ordered_load[1:6,]$load)/sum(ordered_load[7:12,]$load))
})
names(seasonal_consistency_by_device)<-devices


### 3g. COMBINE AND EXPORT LOAD CHARACTERISATION METRICS

Load_metrics <-as.data.frame(cbind(end_user=devices,
										    daily_energy, 
										    daily_energy_stdev_across_days,
										    daily_energy_stdev_across_users=NA,
								   			daily_energy_stdev_across_days_and_users=NA,
											peak_load,
											peak_load_stdev_across_users=NA,
											load_factor,
											load_factor_stdev_across_users=NA,
											pv_overlap=pv_overlap_by_device,
											pv_overlap_stdev_across_users=NA,
											seasonal_consistency=seasonal_consistency_by_device,
											seasonal_consistency_stdev_across_users=NA,
											coincidence_factor=NA
											))


Load_metrics <- as.data.frame(lapply(Load_metrics,unlist))

write.csv(Load_metrics, paste(output_path,'Load_Metrics_by_device.csv',sep=''), row.names=FALSE)




