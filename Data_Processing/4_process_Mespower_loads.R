# Script to process excel outputs from CREST demand model and generate cleaner csv files for analysis in later scripts
# Sheridan Few, Oct 2020
# NB. xlsx_to_csvs.sh should be run between using CREST model and this script to convert file format. (Alternatively, this script could be adapted to read xlsx directly, and rename files)
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
input_path <- paste(root_path,'Meshpower/Input_data/',sep='')
output_path <- paste(root_path,'Meshpower/Processed_Load/',sep='')

climate_data_input_path <- paste(root_path,'Climate_data/',sep='')


### RANGES TO CONSIDER
community='Gitaraga'

bar<-c('Bar_1', 'Bar_2', 'Bar_3', 'Bar_4', 'Bar_7')
cinema<-c('Cinema_1')
hairdresser<-c('Hair_1', 'Hair_2')
mosque<-c('Mosque')
shop<-c('Shop_1')
tailor<-c('Tailoring_1', 'Tailoring_2')
workshop<-c('Workshop')

nondoms<-c(bar,cinema,hairdresser,mosque,shop,tailor,workshop)

nondom_types<-c('bar','cinema','hairdresser','mosque','shop','tailor','workshop')

# Aggregate into list for later use
nondoms_by_type<-list()
nondoms_by_type[['bar']]<-bar
nondoms_by_type[['cinema']]<-cinema
nondoms_by_type[['hairdresser']]<-hairdresser
nondoms_by_type[['mosque']]<-mosque
nondoms_by_type[['shop']]<-shop
nondoms_by_type[['tailor']]<-tailor
nondoms_by_type[['workshop']]<-workshop

### FUNCTIONS

# DO STUFF

### 1. IMPORT LOAD DATA BY NONDOM USER

load_data_by_nondom<-lapply(nondoms,function(nondom){
	input_file<-paste(input_path,"AC_Customers_Hourly_Load/",nondom,"_hourly_load.csv",sep='')
	df = read.csv(input_file)
	return(df)
})

names(load_data_by_nondom)<-nondoms

### 2. GENERATE ANNUAL (365 day) HOURLY LOAD PROFILE FOR EACH NONDOM CUSTOMER (and replace missing Cinema data from Jan 2019 with Jan 2020 data)

# Generate df with a years worth of hours to add load data alongside (this is required as there are some missing values in the Meshpower data which I assume should be zeroes)

start_date <- dmy_hms("01/01/2019 00:00:00 AM")
end_date <- dmy_hms("31/12/2019 23:00:00 PM")

time_sequence <- seq(start_date,end_date, by = '1 hour')

# Hours since start hour for hour_tot in Meshpower data (as standard for time data, something like midnight on 1 Jan 1980?)
time_data<-cbind(month=month(time_sequence),day=day(time_sequence),day_of_year=yday(time_sequence),weekday_status=isWeekday(time_sequence),hour_in_day=hour(time_sequence),hour_tot=c(429528:438287))

# Trim to a year of data (no filling of NAs - used to see how good data is)
load_data_w_NAs_one_year_by_nondom_user<-lapply(nondoms,function(nondom){
	nondom_data<-load_data_by_nondom[[nondom]][c('hour_tot','hourly_usage_wh')]
	# approximately month, actually year divided into 12 equal slices, just used to analyse distribution of data
	nondom_data$month_approx<-as.integer((nondom_data$hour_tot-429528)*12/24/365 + 1)
	# Which are in 2019?
	year_data<-nondom_data[which(nondom_data$hour_tot >= 429528 & nondom_data$hour_tot <= 438287),]
	return(year_data)
})

# Trim to a year of data (NAs assumed zero)
load_data_one_year_by_nondom_user<-lapply(nondoms,function(nondom){
	nondom_data<-load_data_by_nondom[[nondom]][c('hour_tot','hourly_usage_wh')]
	# Which are in 2019?
	year_data<-nondom_data[which(nondom_data$hour_tot >= 429528 & nondom_data$hour_tot <= 438287),]

	# For cinema, 1 - 18 Jan 2019, use equivalent data from 7 - 24 Jan 2020 for the period before the cinema was installed (both start on a Tuesday)
	if(nondom == 'Cinema_1'){
		# Select relevant data from 2020
		replacement_data<-nondom_data[which(nondom_data$hour_tot >= 438288 + (6*24) & nondom_data$hour_tot <= 438288 + (24*24)),]
		# Adjust hour_tot (used in determining date in later stages) so this represents 1-18 Jan
		replacement_data$hour_tot=replacement_data$hour_tot-(365*24)-(6*24)
		# Get rid of partial overlap of data (for half of 18 Jan 2019)
		year_data<-year_data[which(year_data$hour_tot >= 429528 + (24*24)),]
		# Combine data
		year_data<-rbind(replacement_data,year_data)
	}

	# There seems to be a chunk of missing data for some businesses for Nov and Dec 2019, replacing with 2018 data for these
	if(nondom %in% c('Bar_3','Bar_7','Mosque','Tailoring_1')){
		# Select relevant data from 2020
		replacement_data<-nondom_data[which(nondom_data$hour_tot > 429527 - (61*24)  & nondom_data$hour_tot <= 429527),]
		# Adjust hour_tot (used in determining date in later stages) so this represents 1-18 Jan
		replacement_data$hour_tot=replacement_data$hour_tot+(365*24)
		# Get rid of partial overlap of data (for half of 18 Jan 2019)
		year_data<-year_data[which(year_data$hour_tot <= 438287 - (61*24)),]
		# Combine data
		year_data<-rbind(replacement_data,year_data)
	}

	# Merge to give full year
	df<-merge(time_data,year_data,by='hour_tot',all=TRUE)
	# Rename and select variables for consistent format with data from other sources (Gram Oorja, CREST)
	df$hour<-df$hour_in_day
	df$load<-df$hourly_usage_wh
	df$nondom<-nondom

	df[is.na(df)] <- 0
	df<-df[c('nondom','hour_tot','month','day','day_of_year','weekday_status','hour','load')]

	# Export Data
	write.table(df[c('month','day','weekday_status','hour','load')], paste(output_path,'Annual_Hourly_Load_By_Nondom_User/',nondom,'_annual_hourly.csv',sep=''), sep=",", row.names=FALSE)

	return(df)
})

names(load_data_one_year_by_nondom_user)<-nondoms

### 2. GENERATE AVERAGE ANNUAL (365 day) HOURLY LOAD PROFILE FOR EACH NONDOM CUSTOMER TYPE


load_data_one_year_by_nondom_type<-lapply(nondom_types,function(nondom_type){
	# Bind data fitting into this nondom category - this seems a bit clunky tbh
	nondom_type_data<-do.call(rbind,load_data_one_year_by_nondom_user[nondoms_by_type[[nondom_type]]])
	nondom_type_mean<-aggregate(nondom_type_data, by=list(nondom_type_data$hour_tot),FUN=mean)
	df<-nondom_type_mean[c('nondom','hour_tot','month','day','day_of_year','weekday_status','hour','load')]

	# Export Data
	write.table(df[c('month','day','weekday_status','hour','load')], paste(output_path,'Annual_Hourly_Load_By_Nondom_Type/',nondom_type,'_annual_hourly.csv',sep=''), sep=",", row.names=FALSE)

	return(df)
})

names(load_data_one_year_by_nondom_type)<-nondom_types

### 3. IMPORT LOAD DATA BY DOM USER

 dom_files <- list.files(path=paste(input_path,"DC_Customers_Hourly_Load/",sep=''), pattern="*.csv", full.names=TRUE, recursive=TRUE) # List of files containing usage data. These were downloaded from the Meshpower server based upon an SQL query to find mac_address names which have at some point been associated with Gitaraga


load_data_by_dom<-lapply(dom_files,function(dom_file){
	df = read.csv(dom_file)
	return(df)
})

### 4. GENERATE AVERAGE ANNUAL (365 day) HOURLY LOAD PROFILE FOR DOM USERS

# Trim to a year of data
load_data_one_year_by_dom<-lapply(load_data_by_dom,function(dom_load_df){
	# Check if any entries before of Jan or after end of Dec - if not, assume system installed later than start of 2019/disconnected before end 2019 and ignore this dwelling
	if (min(dom_load_df$hour_tot) > 429528 || max(dom_load_df$hour_tot) < 429528){
		return(NULL)
	}

	# Which are in 2019?
	year_data<-dom_load_df[which(dom_load_df$hour_tot >= 429528 & dom_load_df$hour_tot <= 438287),]

	# Remove entries with no energy usage
	if (sum(year_data$hourly_usage_wh)==0){
		return(NULL)
	}

	return(year_data)
})

# Remove null values for meters installed after 1 Jan 2019, or decomissioned before 31 Dec 2020

load_data_one_year_by_dom <- load_data_one_year_by_dom[!sapply(load_data_one_year_by_dom, is.null)]

# Split by those achieving and not achieving tier 1. Not currently used beyond here.

load_data_one_year_by_dom_tier<-list()

load_data_one_year_by_dom_tier_0 <- load_data_one_year_by_dom[sapply(load_data_one_year_by_dom,function(df){sum(df$hourly_usage_wh) < 12 * 365})]
load_data_one_year_by_dom_tier_1 <- load_data_one_year_by_dom[sapply(load_data_one_year_by_dom,function(df){sum(df$hourly_usage_wh) > 12 * 365})]

# Fill empties and export year of load for each dom dwelling with data

n_dom <- length(load_data_one_year_by_dom)

dom_indices<-c(1:n_dom)

load_data_one_year_by_dom_clean<-lapply(dom_indices,function(i){
	dom_data<-load_data_one_year_by_dom[[i]][c('hour_tot','hourly_usage_wh')]
	# Which are in 2019?
	year_data<-dom_data[which(dom_data$hour_tot >= 429528 & dom_data$hour_tot <= 438287),]

	# Merge to give full year
	df<-merge(time_data,year_data,by='hour_tot',all=TRUE)
	# Rename and select variables for consistent format with data from other sources (Gram Oorja, CREST)
	df$hour<-df$hour_in_day
	df$load<-df$hourly_usage_wh
	df[is.na(df)] <- 0
	df$dom_id<-i
	df<-df[c('dom_id','hour_tot','month','day','day_of_year','weekday_status','hour','load')]

	# Export Data
	write.table(df[c('month','day','weekday_status','hour','load')], paste(output_path,'Annual_Hourly_Load_By_Dom_User/','Dwelling_',i,'_annual_hourly.csv',sep=''), sep=",", row.names=FALSE)

	return(df)
})

# Fill empties and export year of load for each dom dwelling with data


# Export average across all dwellings across year (although note there's a lot of data missing)

load_data_one_year_bound_doms<-do.call(rbind,load_data_one_year_by_dom_clean)
load_data_one_year_dom<-aggregate(load_data_one_year_bound_doms, by=list(load_data_one_year_bound_doms$hour_tot),FUN=mean)

write.table(load_data_one_year_dom[c('month','day','weekday_status','hour','load')], paste(output_path,'Annual_Hourly_Load_By_Dom_Type/','DC_domestic_mean_annual_hourly.csv',sep=''), sep=",", row.names=FALSE)


### 7. CHARACTERISE NONDOM LOADS

### 7a. AVERAGE DAILY ENERGY

daily_energy<-lapply(nondom_types,function(nondom_type){
	return(sum(load_data_one_year_by_nondom_type[[nondom_type]]$load)/365)
})

daily_energy_stdev_across_days<-lapply(nondom_types,function(nondom_type){
	nondom_type_mean_by_day <- aggregate(load_data_one_year_by_nondom_type[[nondom_type]][c('day_of_year','load')], by=list(load_data_one_year_by_nondom_type[[nondom_type]]$day_of_year),FUN=sum)
	return(sd(nondom_type_mean_by_day$load))
})

daily_energy_stdev_across_users<-lapply(nondom_types,function(nondom_type){
	nondom_type_data<-do.call(rbind,load_data_one_year_by_nondom_user[nondoms_by_type[[nondom_type]]])
	nondom_mean_annual_by_user <- aggregate(. ~ nondom, nondom_type_data[c('nondom','load')], sum)
	nondom_mean_daily_by_user <- cbind(nondom_mean_annual_by_user['nondom'],nondom_mean_annual_by_user['load']/365)
	return(sd(nondom_mean_daily_by_user$load))
})

daily_energy_stdev_across_days_and_users<-lapply(nondom_types,function(nondom_type){
	nondom_type_data<-do.call(rbind,load_data_one_year_by_nondom_user[nondoms_by_type[[nondom_type]]])
	nondom_type_data$nondom_day<-paste(nondom_type_data$nondom, nondom_type_data$day_of_year)
	nondom_mean_annual_by_day_and_user <- aggregate(. ~ nondom_day, nondom_type_data[c('nondom_day','load')], sum)
	return(sd(nondom_mean_annual_by_day_and_user$load))
})

### 7b. PEAK LOAD

peak_load<-lapply(nondom_types,function(nondom_type){
	return(max(load_data_one_year_by_nondom_type[[nondom_type]]$load))
})

peak_load_stdev_across_users <-lapply(nondom_types,function(nondom_type){
	peak_load_by_nondom_user<-lapply(nondoms_by_type[[nondom_type]],function(nondom_user){
	return(max(load_data_one_year_by_nondom_user[[nondom_user]]$load))
	})
	return(sd(as.numeric(peak_load_by_nondom_user)))
})

### 7c. LOAD FACTOR
# Average load over peak load

load_factor <- as.list((as.numeric(daily_energy)/24)/as.numeric(peak_load))

load_factor_stdev_across_users <-lapply(nondom_types,function(nondom_type){
	load_factor_by_nondom_user<-lapply(nondoms_by_type[[nondom_type]],function(nondom_user){
	return(mean(load_data_one_year_by_nondom_user[[nondom_user]]$load)/max(load_data_one_year_by_nondom_user[[nondom_user]]$load))
	})
	return(sd(as.numeric(load_factor_by_nondom_user)))
})

### 7d. COINCIDENCE FACTOR

# Not defined by user type - not enough instances

### 7e. PV OVERLAP FACTOR

# PV data:
PV_data_input<-paste(climate_data_input_path,community,'_climate.csv',sep='')
PV_data<-read.csv(PV_data_input,skip = 3, header = T)
PV_data['local_time'] <- lapply(PV_data['local_time'], strptime, "%m/%d/%Y %H:%M")
PV_data$hour_tot<-as.integer(as.numeric(PV_data$local_time)/3600) # track hours (absolute, ie each day adds 24)
PV_data$hour_in_day=PV_data$hour_tot%%24
PV_mean<-aggregate(electricity ~ hour_in_day, PV_data, mean)
PV_mean_normalised<-as.data.frame(cbind(hour_in_day=PV_mean$hour_in_day,electricity=PV_mean$electricity/sum(PV_mean$electricity)))

# Get overlap w PV data by hour for plotting purposes
hourly_pv_overlap_by_nondom_type<-lapply(nondom_types,function(nondom_type){
	load_data_mean_day<-aggregate(load ~ hour, load_data_one_year_by_nondom_type[[nondom_type]], mean)
	load_data_mean_day_normalised<-as.data.frame(cbind(hour=load_data_mean_day$hour,load=load_data_mean_day$load/sum(load_data_mean_day$load)))
	overlap=pmin(PV_mean_normalised$electricity,load_data_mean_day_normalised$load)
	return(overlap)
})

pv_overlap_by_nondom_type <- lapply(hourly_pv_overlap_by_nondom_type, sum)

pv_overlap_by_nondom_type_stdev_across_users <-lapply(nondom_types,function(nondom_type){
	pv_overlap_by_nondom_user<-lapply(nondoms_by_type[[nondom_type]],function(nondom_user){
		load_data_mean_day<-aggregate(load ~ hour, load_data_one_year_by_nondom_user[[nondom_user]], mean)
		load_data_mean_day_normalised<-as.data.frame(cbind(hour=load_data_mean_day$hour,load=load_data_mean_day$load/sum(load_data_mean_day$load)))
		overlap=pmin(PV_mean_normalised$electricity,load_data_mean_day_normalised$load)
		return(sum(overlap))
	})
	return(sd(as.numeric(pv_overlap_by_nondom_user)))
})

# 7f. SEASONAL CONSISTENCY

# Quantify seasonal consistency (min(mean load over month) / max(mean load over month))

monthly_load_by_nondom_type<-lapply(nondom_types,function(nondom_type){
	nondom_type_mean_by_month <- aggregate(load_data_one_year_by_nondom_type[[nondom_type]][c('month','load')], by=list(load_data_one_year_by_nondom_type[[nondom_type]]$month),FUN=mean)
	return(nondom_type_mean_by_month)
})

names(monthly_load_by_nondom_type)<-nondom_types

seasonal_consistency_by_nondom_type<-lapply(nondom_types,function(nondom_type){
	ordered_load<-monthly_load_by_nondom_type[[nondom_type]][order(monthly_load_by_nondom_type[[nondom_type]]$load),]
	return(sum(ordered_load[1:6,]$load)/sum(ordered_load[7:12,]$load))
})
names(seasonal_consistency_by_nondom_type)<-nondom_types

seasonal_consistency_by_nondom_type_stdev_across_users <-lapply(nondom_types,function(nondom_type){
	seasonal_consistency_by_nondom_user<-lapply(nondoms_by_type[[nondom_type]],function(nondom_user){
		nondom_user_mean_by_month <- aggregate(load_data_one_year_by_nondom_user[[nondom_user]][c('month','load')], by=list(load_data_one_year_by_nondom_user[[nondom_user]]$month),FUN=mean)
		ordered_load<-nondom_user_mean_by_month[order(nondom_user_mean_by_month$load),]
		return(sum(ordered_load[1:6,]$load)/sum(ordered_load[7:12,]$load))
	})
	return(sd(as.numeric(seasonal_consistency_by_nondom_user)))
})


seasonal_consistency_measure_2_by_nondom_type<-lapply(nondom_types,function(nondom_type){
	nondom_type_mean_by_month <- aggregate(load_data_one_year_by_nondom_type[[nondom_type]][c('month','load')], by=list(load_data_one_year_by_nondom_type[[nondom_type]]$month),FUN=mean)
	return(min(nondom_type_mean_by_month$load)/max(nondom_type_mean_by_month$load))
})


### 7g. COMBINE AND EXPORT LOAD CHARACTERISATION METRICS

Nondom_load_metrics <-as.data.frame(cbind( end_user=nondom_types,
									daily_energy, 
									daily_energy_stdev_across_days,
									daily_energy_stdev_across_users,
									daily_energy_stdev_across_days_and_users,
									peak_load,
									peak_load_stdev_across_users,
									load_factor,
									load_factor_stdev_across_users,
									pv_overlap=pv_overlap_by_nondom_type,
									pv_overlap_stdev_across_users=pv_overlap_by_nondom_type_stdev_across_users,
									seasonal_consistency=seasonal_consistency_by_nondom_type,
									seasonal_consistency_stdev_across_users=seasonal_consistency_by_nondom_type_stdev_across_users,
									coincidence_factor=NA
									))

Nondom_load_metrics <- as.data.frame(lapply(Nondom_load_metrics,unlist))

write.csv(Nondom_load_metrics, paste(output_path,'Load_Metrics_nondom.csv',sep=''), row.names=FALSE)


# 8 CHARACTERISE DOM LOADS (simpler as only one type) (in hindsight would probably have been easier to find a way to integrate domestic into the above commands)

### 8a. AVERAGE DAILY ENERGY


dom_daily_energy<-sum(load_data_one_year_dom$load)/365

dom_daily_energy_stdev_across_days <- sd(aggregate(load_data_one_year_dom[c('day_of_year','load')], by=list(load_data_one_year_dom$day_of_year),FUN=sum)$load)

dom_mean_annual_by_user <- aggregate(. ~ dom_id, load_data_one_year_bound_doms[c('dom_id','load')], sum)
dom_mean_daily_by_user <- cbind(dom_mean_annual_by_user['dom_id'],dom_mean_annual_by_user['load']/365)
dom_daily_energy_stdev_across_users <-sd(dom_mean_daily_by_user$load)

load_data_one_year_bound_doms$dom_day<-paste(load_data_one_year_bound_doms$dom_id, load_data_one_year_bound_doms$day_of_year)
load_data_one_year_aggregated <- aggregate(. ~ dom_day, load_data_one_year_bound_doms[c('dom_day','load')], sum)
dom_daily_energy_stdev_across_days_and_users<-(sd(load_data_one_year_aggregated$load))


### 8b. PEAK LOAD

dom_peak_load<-max(load_data_one_year_dom$load)

peak_load_by_dom_user<-sapply(dom_indices,function(i){
return(max(load_data_one_year_by_dom_clean[[i]]$load))
})

dom_peak_load_stdev_across_users<-sd(peak_load_by_dom_user)

### 8c. LOAD FACTOR
# Average load over peak load

dom_load_factor <- dom_daily_energy/24/dom_peak_load

load_factor_by_dom_user<-sapply(dom_indices,function(i){
return(mean(load_data_one_year_by_dom_clean[[i]]$load)/max(load_data_one_year_by_dom_clean[[i]]$load))
})

dom_load_factor_stdev_across_users<-sd(load_factor_by_dom_user)

### 8d. COINCIDENCE FACTOR

dom_coincidence_factor<-length(dom_indices)*dom_peak_load/sum(peak_load_by_dom_user)


# Not defined by user type - not enough instances

### 8e. PV OVERLAP FACTOR

# Get overlap w PV data by hour for plotting purposes

load_data_mean_day<-aggregate(load ~ hour, load_data_one_year_dom, mean)
load_data_mean_day_normalised<-as.data.frame(cbind(hour=load_data_mean_day$hour,load=load_data_mean_day$load/sum(load_data_mean_day$load)))
dom_hourly_pv_overlap=pmin(PV_mean_normalised$electricity,load_data_mean_day_normalised$load)

dom_pv_overlap <- sum(dom_hourly_pv_overlap)

pv_overlap_by_dom_user<-sapply(dom_indices,function(i){
	load_data_mean_day<-aggregate(load ~ hour, load_data_one_year_by_dom_clean[[i]], mean)
	load_data_mean_day_normalised<-as.data.frame(cbind(hour=load_data_mean_day$hour,load=load_data_mean_day$load/sum(load_data_mean_day$load)))
	overlap=pmin(PV_mean_normalised$electricity,load_data_mean_day_normalised$load)
return(sum(overlap))})

dom_pv_overlap_stdev_across_users <- sd(as.numeric(pv_overlap_by_dom_user))

# 8f. SEASONAL CONSISTENCY

# Quantify seasonal consistency (min(mean load over month) / max(mean load over month))

dom_mean_by_month <- aggregate(load_data_one_year_dom[c('month','load')], by=list(load_data_one_year_dom$month),FUN=mean)
ordered_load<-dom_mean_by_month[order(dom_mean_by_month$load),]
dom_seasonal_consistency<-sum(ordered_load[1:6,]$load)/sum(ordered_load[7:12,]$load)

seasonal_consistency_by_dom_user<-sapply(dom_indices,function(i){
	dom_mean_by_month <- aggregate(load_data_one_year_by_dom_clean[[i]][c('month','load')], by=list(load_data_one_year_by_dom_clean[[i]]$month),FUN=mean)
	ordered_load<-dom_mean_by_month[order(dom_mean_by_month$load),]
	dom_seasonal_consistency<-sum(ordered_load[1:6,]$load)/sum(ordered_load[7:12,]$load)
	return(dom_seasonal_consistency)
})

dom_seasonal_consistency_stdev_across_users <- sd(seasonal_consistency_by_dom_user)


### 8g. COMBINE AND EXPORT LOAD CHARACTERISATION METRICS

Dom_load_metrics <-as.data.frame(cbind( end_user='domestic',
								   dom_daily_energy, 
								   dom_daily_energy_stdev_across_days, 
								   dom_daily_energy_stdev_across_users,
								   dom_daily_energy_stdev_across_days_and_users,
								   dom_peak_load,
								   dom_peak_load_stdev_across_users,
								   dom_load_factor,
								   dom_load_factor_stdev_across_users,
								   dom_pv_overlap,
								   dom_pv_overlap_stdev_across_users,
								   dom_seasonal_consistency,
								   dom_seasonal_consistency_stdev_across_users,
								   dom_coincidence_factor))

Dom_load_metrics <- as.data.frame(lapply(Dom_load_metrics,unlist))

write.csv(Dom_load_metrics, paste(output_path,'Load_Metrics_dom.csv',sep=''), row.names=FALSE)













### 9. BIT OF DATA ANALYSIS - HOW PATCHY IS THE DATA? WHAT IS AVERAGE LOAD? (not strictly processing, but seemed to fit here) -> Suggests domestic data is too patchy to be of much use without a lot of effort

# How many hours of 2019 do we have data for for each dom meter?

n_hours_with_load_data_dom_2019 <- do.call(rbind,lapply(load_data_one_year_by_dom,nrow))

# What proportion of hours of 2019 do we have data for for each dom meter?

proportion_hours_with_load_data_dom_2019 <- n_hours_with_load_data_dom_2019 / (365*24)

# What is the distribution of proportion of hours with data across all dom meters in the sample?

quantile(proportion_hours_with_load_data_dom_2019,c(0:10)/10)

# Output (indicates around 40% of meters have load data recorded for less than 50% of hours)
#          0%         10%         20%         30%         40%         50%         60%         70%         80%         90%        100% 
# 0.008789954 0.026369863 0.105479452 0.315296804 0.505936073 0.584589041 0.709703196 0.806735160 0.882191781 0.935730594 0.999086758 

# What is typical daily energy usage?

# Average daily usage based on assumption that all hours with no data also have no energy usage. This probably gives aggregate values close to the truth, as I think data isn't sent when there's no mobile signal, but this usage will be recorded eventually (perhaps at the wrong hour).
daily_usage_wh <-lapply(load_data_one_year_by_dom,function(df){
	sum(df$hourly_usage_wh)/365
	})

daily_usage_wh_list<-do.call(rbind,daily_usage_wh)


quantile(daily_usage_wh_list,c(0:10)/10)

# Output: indicates ~60% at tier 1 or above, none at tier 2
  #        0%         10%         20%         30%         40%         50%         60%         70%         80%         90%        100% 
  # 0.0000000   0.5906977   3.6007136   8.7224743  11.4856750  16.7188531  24.9214702  38.0673034  52.1995726  61.3711447 171.3503653 
