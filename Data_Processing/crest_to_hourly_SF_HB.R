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
library(ggplot2)
library(RColorBrewer)


### PATH DEFINITION

root_path <- '/Volumes/Hamish_ext/'
input_path <- paste(root_path,'CSV/',sep='')
processed_load_path <- paste(root_path,'Processed_Load/',sep='')
output_path<-paste(root_path,'Outputs/',sep='')
plot_path <- paste(root_path,'Plots/',sep='')

#updated

# Climate data (used in determining PV overlap factor)
climate_data_input_path <- paste(root_path,'Climate_data/',sep='')

# Create necessary directories if they don't exist
dir.create(processed_load_path, showWarnings = FALSE)
dir.create(output_path, showWarnings = FALSE)
dir.create(plot_path, showWarnings = FALSE)

dir.create(paste(processed_load_path,'Full_Demand_Data/',sep=''), showWarnings = FALSE)
# dir.create(paste(processed_load_path,'Annual_Hourly_Load_By_Village_Tier/'), showWarnings = FALSE)
dir.create(paste(processed_load_path,'Annual_Hourly_Load_By_Village_Tier_Dwelling/',sep=''), showWarnings = FALSE)

dir.create(paste(processed_load_path,'Total_Annual_Device_Load_By_Village_Tier/',sep=''), showWarnings = FALSE)
dir.create(paste(processed_load_path,'Load_By_Tier/',sep=''), showWarnings = FALSE)


### SET VALUES

### CORRECTION FACTORS

# Washing machine appliance inputs in CREST model are not used, but overriden by minute by minute load data specified in the "appliances" module based upon "data from personal communication with a major washing maching manufacturer". These are now ou of date as washing machines have become more efficient. This script therefore multiplies washing machine demand by a correcitive scaling factor.
Wash_machine_expected_cycle_Wh<-52.57*138/60

# Washing machine as modelled in CREST model uses 926.8 Wh -> correction factor
Wash_machine_factor<-Wash_machine_expected_cycle_Wh/926.8

# Fraction of modelled CREST fridge consumption excpected at different temperatures based on Verasol (formerly EST) data. This allows fridge consumption to be scaled up/down based on internal temperature. CHasn't had a large impact so far, but cpuld make a big difference with hot climate and ineffective cooling.
fridge_consumption_factor_temp_fitline<-lowess(c(4,32,43),c(0.0,1.0,1.89))

### RANGES TO CONSIDER

example_village='Angola' # Should replace with name of arbitrary location from which non-climatic appliance load data should be taken

# Full range
#villages<-c('Angola', 'Bangladesh', 'Benin', 'Botswana', 'Burkina Faso', 'Burundi', 'Cambodia', 'Cameroon', 'Central African Republic', 'Chad', "Cote d'Ivoire", 'DRC', 'Eritrea', 'Ethiopia', 'Gambia', 'Ghana', 'Guatemala', 'Guinea', 'Guinea-Bissau', 'Haiti', 'Honduras', 'India', 'Indonesia', 'Kenya', 'Lesotho', 'Liberia', 'Madagascar', 'Malawi', 'Mali', 'Mauritania', 'Mozambique', 'Myanmar', 'Namibia', 'Nepal', 'Niger', 'Nigeria', 'Peru', 'Philippines', 'Republic of Congo', 'Rwanda', 'Senegal', 'Sierra Leone', 'Somalia', 'South Africa', 'Sudan', 'Tanzania', 'Togo', 'Uganda', 'Yemen', 'Zambia', 'Zimbabwe', 'India_1', 'India_2', 'India_3', 'India_4', 'Indonesia_1', 'Indonesia_2', 'Indonesia_3')
# climates<-c('HSub','TSav')
villages<-c('Angola')
tiers<-c(1:5)
daytypes<-c('wd','we')
months<-c(1:12)
N_households<-25
dwelling_indices<-c(1:N_households)

# Test range
#input_path <- paste(root_path,'CREST/Excel_Outputs/',sep='')
villages<-c('Angola')
tiers<-c(5)
daytypes<-c('wd','we')
months<-c(1:12)
N_households<-25
dwelling_indices<-c(1:N_households)
# 
# village_to_climate<-function(village){
#   climate<-gsub(village, pattern = "Gitaraga", replacement = "TSav")
#   climate<-gsub(climate, pattern = "Bhinjpur", replacement = "HSub") 
#   return(climate)
# }

# Set devices by tier and define color palettes to use for plotting (Heating not used)

devices_by_tier<-list()
cooling_by_tier<-list()
heating_by_tier<-list()
colors_by_tier<-list()

cooling_by_tier['1']<-list()
cooling_by_tier['2']<-list(c("Fan"))
cooling_by_tier['3']<-list(c("Air Cooler"))
cooling_by_tier['4']<-list(c("Air Cooler"))
cooling_by_tier['5']<-list(c("Air Conditioning"))

heating_by_tier['1']<-list()
heating_by_tier['2']<-list()
heating_by_tier['3']<-list()
heating_by_tier['4']<-list()
heating_by_tier['5']<-list(c("Electric Water Heater"))

colors_by_tier['1']<-'Reds'
colors_by_tier['2']<-'Blues'
colors_by_tier['3']<-'Greens'
colors_by_tier['4']<-'Purples'
colors_by_tier['5']<-'Oranges'

cooling_techs<-c("Fan","Air Cooler","Air Conditioning")
heating_techs<-c("Electric Water Heater")

devices_by_tier['1']<-list(c("Lighting","Mobile Phone"))
devices_by_tier['2']<-list(c("Laptop","TV","Fan"))
devices_by_tier['3']<-list(c("Fridge freezer","Air Cooler"))
devices_by_tier['4']<-list(c("Iron", "Microwave", 'Washing machine'))
devices_by_tier['5']<-list(c("Hob","Vacuum","Air Conditioning"))

all_techs<-c(devices_by_tier[['1']],
             devices_by_tier[['2']],
             devices_by_tier[['3']],
             devices_by_tier[['4']],
             devices_by_tier[['5']],
             'Appliance_Load')

myColors<-c(rev(brewer.pal(length(devices_by_tier[['1']]),colors_by_tier[['1']]))[1:length(devices_by_tier[['1']])],
            rev(brewer.pal(length(devices_by_tier[['2']]),colors_by_tier[['2']]))[1:length(devices_by_tier[['2']])],
            rev(brewer.pal(length(devices_by_tier[['3']]),colors_by_tier[['3']]))[1:length(devices_by_tier[['3']])],
            rev(brewer.pal(length(devices_by_tier[['4']]),colors_by_tier[['4']]))[1:length(devices_by_tier[['4']])],
            rev(brewer.pal(length(devices_by_tier[['5']]),colors_by_tier[['5']]))[1:length(devices_by_tier[['5']])],
            'grey')
names(myColors) <-c(all_techs)




### FUNCTIONS

# Split load data by dwelling index (also fixes data type of some columns
split_load_CREST <- function (x,data){
  load_data<-subset(data,data['Dwelling index']==x) # Subset by dwelling index
  load_data<-cbind(load_data[3],load_data[1],load_data[,-c(1:3)])
  load_data_numeric <- mutate_all(load_data[-1], function(x) as.numeric(as.character(x))) # Convert data to numeric (R doesn't recogines it as such yet - probs because input data has no decimal point)
  load_data_out<-cbind(Time=load_data[['Time']],load_data_numeric)
  return(load_data_out)
}

# Convert minute by minute data to hourly mean. Would also work for other sub-hour temporal resolutions
convert_to_hourly <- function(x){
  data<-aggregate(x, list(cut(as.POSIXct(x$Time,tz='Africa/Kigali'), "1 hour")), mean)
  out<-data[-1]
  out$Time<-format(as.POSIXct(data[[1]],tz='Africa/Kigali'), "%H")
  #out$Time<-as.POSIXct(data[[1]],tz='Africa/Kigali')
  return(out)
}

convert_to_hourly_peak <- function(x){
  data<-aggregate(x, list(cut(as.POSIXct(x$Time,tz='Africa/Kigali'), "1 hour")), max)
  out<-data[-1]
  out$Time<-format(as.POSIXct(data[[1]],tz='Africa/Kigali'), "%H")
  #out$Time<-as.POSIXct(data[[1]],tz='Africa/Kigali')
  return(out)
}


# DO STUFF

### 1. AMALGAMATE EXCEL OUTPUTS INTO ONE CLEAN SHEET CONTAINING APPLIANCE, HEATING, COOLING, & LIGHTING DATA - REVISE FRIDGE LOAD BASED ON INTERNAL TEMP

print('STEP 1. AMALGAMATE EXCEL OUTPUTS INTO ONE CLEAN SHEET CONTAINING APPLIANCE, HEATING, COOLING, & LIGHTING DATA - REVISE FRIDGE LOAD BASED ON INTERNAL TEMP')

print('Step 1 begun at:')
Sys.time()
Sys.sleep(1)

# Loop over each combination of the variables below

# Test variables for troubleshooting - shouldn't have an impact alone as all the work is done in "lapply" functions, but best if commented out to be safe
#village='Gitaraga'
#tier=5
#month=1
#daytype='we'

load_data_by_tier_daytype<-lapply(tiers, function(tier){
  load_data_by_daytype<-lapply(daytypes, function(daytype){
    month=1 # Months identical wrt appliance demand
    name_abbrev<-paste(example_village,"_Tier",tier,"_Month",month,"_",daytype,sep='')
    
    print(paste('Importing appliance data for ',name_abbrev,sep=''))
    print('non climate dependent appliance data from this location and month will be used for all locations and months')
    
    # GET AND RESHAPE DATA
    
    # Get names of input files
    input_file<-paste(input_path,"Load_By_Device_",example_village,"_Tier",tier,"_Month",month,"_",daytype,".csv",sep='')
    
    # Deal with messiness in imported files
    headers = read.csv(input_file, skip = 3, header = F, nrows = 1, as.is = T) 
    df = read.csv(input_file, skip = 6, header = F)
    colnames(df)= headers
    df <- head(df, -2)[1:33] 
    
    
    # Rename appliances where appropriate
    names(df)[names(df) == 'Cordless telephone'] <- 'Mobile Phone'
    names(df)[names(df) == 'TV 1'] <- 'TV'
    names(df)[names(df) == 'Personal computer'] <- 'Laptop'
    
    # Correct washing machine load (see head of file)
    df[['Washing machine']]<-Wash_machine_factor*as.numeric(as.character(df[['Washing machine']]))
    
    # Drop columns with all zero values - not currently used
    #df<-df[, colSums(df != 0) > 0]
    
    # Get R to recognise time column as times
    #df['Time'] <- lapply(df['Time'], strptime, "%I:%M:%S %p")
    df['Time'] <- lapply(df['Time'], strptime, "%H:%M:%S")
    print('df')
    # Export complete set of loads by tier
    write.table(df, paste(processed_load_path,'Full_Demand_Data/',name_abbrev,'_all_loads.csv',sep=''), sep=",", row.names=FALSE)
    
    # Export df to list
    return(df)
  })
  names(load_data_by_daytype)<-daytypes
  return(load_data_by_daytype)
})	
names(load_data_by_tier_daytype)<-tiers

# Get heating/cooling load (depends on tier, but not on climatic condition)

climatic_load_data_by_village_tier_month_daytype<-lapply(villages, function(village){
  climatic_load_data_by_tier<-lapply(tiers, function(tier){
    climatic_load_data_by_month<-lapply(months, function(month){
      climatic_load_data_by_daytype<-lapply(daytypes, function(daytype){
        name_abbrev<-paste(village,"_Tier",tier,"_Month",month,"_",daytype,sep='')
        
        print(paste('Importing cooling, heating, lighting data for ',name_abbrev,sep=''))
        
        # GET AND RESHAPE DATA
        
        # Get names of input files
        d_input_file<-paste(input_path,"Res_Disagg_",village,"_Tier",tier,"_Month",month,"_",daytype,".csv",sep='')
        
        # Deal with messiness in imported files
        d_headers = read.csv(d_input_file, skip = 3, header = F, nrows = 1, as.is = T)
        d_df = read.csv(d_input_file, skip = 6, header = F)
        colnames(d_df)= d_headers
        
        # Subset relevant columns
        df <- cbind(d_df[1:3],Cooling=d_df[['Electricity used by cooling system']],Heating=d_df[['Electricity used by heating system']],Lighting=d_df[['Lighting demand']],Internal_Temp=d_df[['Internal building node temperature']]) 
        
        # Rename/drop heating & cooling by tier as appropriate
        
        if(tier == 1)
        {
          df[['Cooling']] <- NULL
          df[['Heating']] <- NULL
        }
        if(tier == 2)
        {
          names(df)[names(df) == 'Cooling'] <- 'Fan'
          df[['Heating']] <- NULL
        }
        if(tier == 3)
        {
          names(df)[names(df) == 'Cooling'] <- 'Air Cooler'
          df[['Heating']] <- NULL
        }
        if(tier == 4)
        {
          names(df)[names(df) == 'Cooling'] <- 'Air Cooler'
          df[['Heating']] <- NULL
        }
        if(tier == 5)
        {
          names(df)[names(df) == 'Cooling'] <- 'Air Conditioning'
          print('Removing heating load')
          df = subset(df, select = -c(Heating) )
          #names(df)[names(df) == 'Heating'] <- 'Electric Water Heater'
        }
        
        df = subset(df, select = -c(Internal_Temp) )
        
        # Drop columns with all zero values - not currently used
        #df<-df[, colSums(df != 0) > 0]
        
        # Get R to recognise time column as times
        #df['Time'] <- lapply(df['Time'], strptime, "%I:%M:%S %p")
        df['Time'] <- lapply(df['Time'], strptime, "%H:%M:%S")
        
        # Export complete set of loads by tier
        write.table(df, paste(processed_load_path,'Full_Demand_Data/',name_abbrev,'_all_loads.csv',sep=''), sep=",", row.names=FALSE)
        
        # Export df to list
        return(df)
      })
      names(climatic_load_data_by_daytype)<-daytypes
      return(climatic_load_data_by_daytype)
    })
    names(climatic_load_data_by_month)<-months
    return(climatic_load_data_by_month)
  })	
  names(climatic_load_data_by_tier)<-tiers
  return(climatic_load_data_by_tier)
})
names(climatic_load_data_by_village_tier_month_daytype)<-villages



# Example command to access the first few lines of a day's data:
# head(load_data_by_village_tier_month_daytype[['Bhinjpur']][['1']][['6']][['wd']])

### 2. GENERATE HOURLY LOAD BY DAY FOR EACH COMBINATION OF VILLAGE, TIER, MONTH, DAYTYPE

print('STEP 2. GENERATE HOURLY LOAD BY DAY FOR EACH COMBINATION OF VILLAGE, TIER, MONTH, DAYTYPE')

print('Step 2 begun at:')
Sys.time()
Sys.sleep(1)


# Generate appliance data disaggregated across across N dwellings

hourly_load_data_by_tier_daytype_dwelling_index<-lapply(tiers, function(tier){
  print(paste('Converting data to hourly for tier ',tier,sep=''))
  load_data_by_daytype<-lapply(daytypes, function(daytype){
    df<-load_data_by_tier_daytype[[as.character(tier)]][[daytype]]
    customer_numbers <- unique(df[["Dwelling index"]])
    df_by_customer <- lapply(customer_numbers, split_load_CREST, data=df )
    
    # Convert data into hourly (mean load across each minute in each hour)
    hourly_data <- lapply(df_by_customer, convert_to_hourly)
    hourly_load <- lapply(hourly_data,function(df){
      return(rowSums(df[-1][-1]))
    })
    return(hourly_data)
  })
  names(load_data_by_daytype)<-daytypes
  return(load_data_by_daytype)
})

names(hourly_load_data_by_tier_daytype_dwelling_index)<-tiers

# Aggregrate across appliances

aggregated_hourly_load_data_by_tier_daytype_dwelling_index<-lapply(tiers, function(tier){
  print(paste('Converting data to hourly for tier ',tier,sep=''))
  load_data_by_daytype<-lapply(daytypes, function(daytype){
    agg_hourly_data<-lapply(hourly_load_data_by_tier_daytype_dwelling_index[[as.character(tier)]][[daytype]],function(df){
      return(cbind(df[1:2],Appliance_Load=rowSums(df[-1][-1])))
    })
    return(agg_hourly_data)
  })
  names(load_data_by_daytype)<-daytypes
  return(load_data_by_daytype)
})

names(aggregated_hourly_load_data_by_tier_daytype_dwelling_index)<-tiers



# Generate climate dependent load data disaggregated across across N dwellings

hourly_climatic_load_data_by_village_tier_month_daytype_dwelling_index<-lapply(villages, function(village){
  climatic_load_data_by_tier<-lapply(tiers, function(tier){
    print(paste('Converting data to hourly for ',village,' tier ',tier,sep=''))
    climatic_load_data_by_month<-lapply(months, function(month){
      climatic_load_data_by_daytype<-lapply(daytypes, function(daytype){
        df<-climatic_load_data_by_village_tier_month_daytype[[village]][[as.character(tier)]][[as.character(month)]][[daytype]]
        customer_numbers <- unique(df[["Dwelling index"]])
        df_by_customer <- lapply(customer_numbers, split_load_CREST, data=df )
        
        # Convert data into hourly (mean load across each minute in each hour)
        hourly_data <- lapply(df_by_customer, convert_to_hourly)
        hourly_load <- lapply(hourly_data,function(df){
          return(rowSums(df[-1][-1]))
        })
        return(hourly_data)
      })
      names(climatic_load_data_by_daytype)<-daytypes
      return(climatic_load_data_by_daytype)
    })
    names(climatic_load_data_by_month)<-months
    return(climatic_load_data_by_month)
  })	
  names(climatic_load_data_by_tier)<-tiers
  return(climatic_load_data_by_tier)
})
names(hourly_climatic_load_data_by_village_tier_month_daytype_dwelling_index)<-villages


# Max load data disaggregated across across N dwellings (required for some load characterisation stat , not currently implemented)


# max_hourly_load_data_by_village_tier_month_daytype_dwelling_index<-lapply(villages, function(village){
# 	load_data_by_tier<-lapply(tiers, function(tier){
# 		print(paste('Getting peak loads for ',village,' tier ',tier,sep=''))
# 		load_data_by_month<-lapply(months, function(month){
# 			load_data_by_daytype<-lapply(daytypes, function(daytype){
# 				df<-load_data_by_village_tier_month_daytype[[village]][[as.character(tier)]][[as.character(month)]][[daytype]]
# 				customer_numbers <- unique(df[["Dwelling index"]])
# 				df_by_customer <- lapply(customer_numbers, split_load_CREST, data=df )

# 				# Convert data into hourly (mean load across each minute in each hour)
# 				total_load_by_customer <- lapply(df_by_customer,function(df){
# 					return(rowSums(df[-1][-1]))
# 				})
# 				peak_load_by_customer <- lapply(total_load_by_customer, max)
# 				return(peak_load_by_customer)
# 			})
# 			names(load_data_by_daytype)<-daytypes
# 			return(load_data_by_daytype)
# 		})
# 		names(load_data_by_month)<-months
# 		return(load_data_by_month)
# 	})	
# 	names(load_data_by_tier)<-tiers
# 	return(load_data_by_tier)
# })
# names(hourly_load_data_by_village_tier_month_daytype_dwelling_index)<-villages

# Example command to access dwelling 24 hour profile:
# hourly_load_data_by_village_tier_month_daytype_dwelling_index[['Bhinjpur']][['1']][['7']][['wd']][[1]]

# # Generate mean across N dwellings
# # Commented out as currently not used

# mean_hourly_load_data_by_village_tier_month_daytype<-lapply(villages, function(village){
# 	load_data_by_tier<-lapply(tiers, function(tier){
# 		load_data_by_month<-lapply(months, function(month){
# 			load_data_by_daytype<-lapply(daytypes, function(daytype){
# 				print(paste('Generating mean across dwellings: ',village,"_Tier",tier,"_Month",month,"_",daytype,sep=''))
# 				# Split dataframe into a list of dataframes representing each dwelling, indexed by the Dwelling index
# 				df<-load_data_by_village_tier_month_daytype[[village]][[as.character(tier)]][[as.character(month)]][[daytype]]
# 				customer_numbers <- unique(df[["Dwelling index"]])
# 				df_by_customer <- lapply(customer_numbers, split_load_CREST, data=df )

# 				# Convert data into hourly (mean load across each minute in each hour)
# 				hourly_data <- lapply(df_by_customer, convert_to_hourly)

# 				# Convert data into hourly (maximum load across each minute in each hour)
# 				#hourly_peak_data <- lapply(df_by_customer, convert_to_hourly_peak)

# 				# Combine all dwellings again (bit of redundancy here)
# 				total_hourly_df=do.call(rbind,hourly_data)
# 				#total_hourly_peak_df=do.call(rbind,hourly_peak_data)

# 				# Get mean load across dwellings by hour
# 				mean_hourly=aggregate(total_hourly_df[-1][-1],by=list(total_hourly_df$Time), mean)
# 				names(mean_hourly)[1] <- "Hour"
# 				rownames(mean_hourly) <- mean_hourly$Hour
# 				mean_hourly <- mean_hourly[-1]

# 				return(rowSums(mean_hourly))
# 				# Export df to list
# 				#return(df)
# 			})
# 			names(load_data_by_daytype)<-daytypes
# 			return(load_data_by_daytype)
# 		})
# 		names(load_data_by_month)<-months
# 		return(load_data_by_month)
# 	})	
# 	names(load_data_by_tier)<-tiers
# 	return(load_data_by_tier)
# })
# names(mean_hourly_load_data_by_village_tier_month_daytype)<-villages


# Example command to access the first few lines of a day's data:
# head(mean_hourly_load_data_by_village_tier_month_daytype[['Bhinjpur']][['1']][['6']][['wd']])

### 3. GENERATE ANNUAL (365 day) HOURLY LOAD PROFILE EACH COMBINATION OF VILLAGE, TIER, MONTH, DAYTYPE

print('STEP 3. GENERATE ANNUAL (365 day) HOURLY LOAD PROFILE EACH COMBINATION OF VILLAGE, TIER, MONTH, DAYTYPE')

print('Step 3 begun at:')
Sys.time()
Sys.sleep(1)

# Generate df with a years worth of hours to add load data alongside

start_date <- dmy_hms("01/01/2019 00:00:00 AM")
end_date <- dmy_hms("31/12/2019 00:00:00 AM")

time_sequence <- seq(start_date,end_date, by = '1 day')

time_data<-cbind(month=month(time_sequence),weekday_status=isWeekday(time_sequence))

hourly_start_date <- dmy_hms("01/01/2019 00:00:00 AM")
hourly_end_date <- dmy_hms("31/12/2019 23:00:00 PM")
hourly_time_sequence <- seq(start_date,end_date, by = '1 hour')
hourly_time_data<-cbind(hour_tot=c(1:8760),month=month(hourly_time_sequence),day=day(hourly_time_sequence),weekday_status=isWeekday(hourly_time_sequence),hour_in_day=hour(hourly_time_sequence))


# # Add hourly load data for each village and tier (based on hour of day, month, and wd/we status) and export
# # Commented out as currently not used

# annual_hourly_load_by_village_tier<-lapply(villages, function(village){
#   load_data_by_tier<-lapply(tiers, function(tier){
#     print(paste('Generating mean hourly load over year across dweliings for ',village,' tier ',tier,sep=''))
#     hour_load<-apply(time_data, 1, function(x){
#       # Get load for every hour of the year based on village, month, wd/we status
#       month<-as.character(x[2])
#       weekday_status<-x[4]
#       hour<-x[5]

#       if(weekday_status == 1)
#       {
#         daytype = 'wd'
#       } else {
#         daytype = 'we'
#       }

#       return(mean_hourly_load_data_by_village_tier_month_daytype[[village]][[as.character(tier)]][[as.character(month)]][[daytype]][hour+1])
#     })
#     df<-cbind(time_data,hour_load)
#     colnames(df)<-c('hour_tot','month','day','weekday_status','hour','load')
#     write.table(df, paste(processed_load_path,'Annual_Hourly_Load_By_Village_Tier/',village,'_Tier',tier,'_annual_hourly.csv',sep=''), sep=",", row.names=FALSE)
#     return(df)
#   })  
#   names(load_data_by_tier)<-tiers
#   return(load_data_by_tier)
# })
# names(annual_hourly_load_by_village_tier)<-villages

# head(annual_hourly_load_by_village_tier[['Bhinjpur']][['1']])


# Add hourly load data for each village, tier, dwelling (based on hour of day, month, and wd/we status). Export aggregated load

annual_hourly_load_by_village_tier_dwelling<-lapply(villages, function(village){
  load_data_by_tier<-lapply(tiers, function(tier){
    print(paste('Generating hourly load across year by dwelling for ',village,' tier ',tier,sep=''))
    load_data_by_dwelling<-lapply(dwelling_indices, function(dwelling){
      load_by_day<-apply(time_data, 1, function(day_info){
        month=day_info[1]
        daytype<-gsub(day_info[2], pattern = 1, replacement = "wd")
        daytype<-gsub(daytype, pattern = 0, replacement = "we")

        return(cbind(hourly_climatic_load_data_by_village_tier_month_daytype_dwelling_index[[village]][[as.character(tier)]][[as.character(month)]][[daytype]][[dwelling]][-1][-1],
              Appliance_Load=aggregated_hourly_load_data_by_tier_daytype_dwelling_index[[as.character(tier)]][[daytype]][[dwelling]][-1][-1]))
      })

      # Reshape data into one df
      year_data<-do.call(rbind,load_by_day)


      # Export aggregated load to csv 
      aggregated_load<-rowSums(year_data)
      aggregated_load_w_time<-cbind(hourly_time_data,aggregated_load)
      colnames(aggregated_load_w_time)<-c('hour_tot','month','day','weekday_status','hour','load')
      write.table(aggregated_load_w_time, paste(processed_load_path,'Annual_Hourly_Load_By_Village_Tier_Dwelling/',village,'_Tier',tier,'_dwelling',dwelling,'_annual_hourly.csv',sep=''), sep=",", row.names=FALSE)

      df<-cbind(hourly_time_data,year_data)
      return(df)
    })  
    return(load_data_by_dwelling)
  })  
  names(load_data_by_tier)<-tiers
  return(load_data_by_tier)
})
names(annual_hourly_load_by_village_tier_dwelling)<-villages

head(annual_hourly_load_by_village_tier_dwelling[[village]][[as.character(tier)]][[1]])



### 4. LOAD CHARACTERISATION

print('STEP 4. LOAD CHARACTERISATION')

print('Step 4 begun at:')
Sys.time()
Sys.sleep(1)

load_stats_by_village_tier<-lapply(villages, function(village){
  # PV data:
  PV_data_input<-paste(climate_data_input_path,village,'_climate.csv',sep='')
  PV_data<-read.csv(PV_data_input,skip = 3, header = T)
  PV_data['local_time'] <- lapply(PV_data['local_time'], strptime, "%m/%d/%Y %H:%M")
  PV_data$hour_tot<-as.integer(as.numeric(PV_data$local_time)/3600) # track hours (absolute, ie each day adds 24)
  PV_data$hour_in_day=PV_data$hour_tot%%24

  PV_data$electricity_normalised<-PV_data$electricity/sum(PV_data$electricity)


  PV_mean<-aggregate(electricity ~ hour_tot, PV_data, mean)
  PV_mean_normalised<-as.data.frame(cbind(hour_tot=PV_mean$hour_tot,electricity=PV_mean$electricity/sum(PV_mean$electricity)))

  load_stats_daily_load_by_tier<-lapply(tiers, function(tier){
    # select load data for this village and tier
    data<-annual_hourly_load_by_village_tier_dwelling[[village]][[as.character(tier)]]
    data_combined<-do.call(rbind,data)

    # Add column for total load
    data_combined$total_load<-rowSums(subset(data_combined,select=-c(hour_tot,month,day,weekday_status,hour_in_day)))

    #data_combined$cooling_load<-rowSums(subset(data_combined,select=-c(hour_tot,month,day,weekday_status,hour_in_day))[[cooling_techs,]])

    # Add ID columns for stats below
    data_combined$month_day<-paste(data_combined$month,data_combined$day)
    data_combined$dwelling_index<-rep(dwelling_indices,each=8760)
    data_combined$dwelling_index_month_day<-paste(data_combined$dwelling_index,data_combined$month_day)

    #NB. INCOMPLETE - get peak load from mimute by minute data
    #data_peak_load<-annual_hourly_load_by_village_tier_dwelling[[village]][[as.character(tier)]]
    #data_combined<-do.call(rbind,data)

    # DAILY ENERGY
    # Mean
    daily_energy<-sum(data_combined$total_load)/365/N_households

    # stdev across users
    daily_energy_by_user <- aggregate(. ~ dwelling_index, data_combined[c('dwelling_index','total_load')], sum)/365
    daily_energy_stdev_across_users <- sd(daily_energy_by_user$total_load)

    # stdev across days
    daily_energy_by_day <- aggregate(. ~ month_day, data_combined[c('month_day','total_load')], sum)
    daily_energy_stdev_across_days <- sd(daily_energy_by_day$total_load)/N_households

    # stdev across users and days
    daily_energy_by_user_day <- aggregate(. ~ dwelling_index_month_day, data_combined[c('dwelling_index_month_day','total_load')], sum)
    daily_energy_stdev_across_days_and_users <- sd(daily_energy_by_user_day$total_load)


    # PEAK LOAD
    peak_load_by_user <- aggregate(. ~ dwelling_index, data_combined[c('dwelling_index','total_load')], max)

    # Mean
    peak_load<-mean(peak_load_by_user$total_load)

    # stdev across users
    peak_load_stdev_across_users <- sd(peak_load_by_user$total_load)

    # stdev across days
    peak_load_by_day <- aggregate(. ~ month_day, data_combined[c('month_day','total_load')], max)
    peak_load_stdev_across_days <- sd(peak_load_by_day$total_load)

    # stdev across users and days
    peak_load_by_user_day <- aggregate(. ~ dwelling_index_month_day, data_combined[c('dwelling_index_month_day','total_load')], max)
    peak_load_stdev_across_days_and_users <- sd(daily_energy_by_user_day$total_load)


    # LOAD FACTOR
    # Average load over peak load

    # Mean
    load_factor <-daily_energy/peak_load/24

    # stdev across users
    load_factor_by_user<-daily_energy_by_user$total_load/peak_load_by_user$total_load
    load_factor_stdev_across_users <-sd(load_factor_by_user)

    # COINCIDENCE FACTOR
    # Max of aggregated load over sum of peak loads
    daily_energy_by_hour_tot <- aggregate(. ~ hour_tot, data_combined[c('hour_tot','total_load')], sum)
    max_aggregate_load<-max(daily_energy_by_hour_tot$total_load)
    coincidence_factor<-max_aggregate_load/sum(peak_load_by_user)

    # PV OVERLAP FACTOR
    # NB. Done this across whole year - need to change Meshpower & Gram Oorja assessments accordingly and check definition in paper
    mean_load_by_hour <- aggregate(. ~ hour_tot, data_combined[c('hour_tot','total_load')], mean)
    mean_load_by_hour$load_normalised<-mean_load_by_hour$total_load/sum(mean_load_by_hour$total_load)
    data_combined$total_load_normalised<-data_combined$total_load/sum(data_combined$total_load)
    
    # Get overlap (note ranges slightly restricted in order to account for different range of data - PV data starts at midnight UCT, and load data at midnight local time)
    PV_overlap=sum(pmin(PV_data$electricity_normalised[1:(8760-PV_data$hour_in_day[1])],mean_load_by_hour$load_normalised[(PV_data$hour_in_day[1]+1):8760]))
    
    PV_overlap_by_dwelling<-sapply(dwelling_indices,function(dwelling){
      data_combined_dwelling<-data_combined[which(data_combined$dwelling_index == dwelling),]
      data_combined_dwelling$load_normalised<-data_combined_dwelling$total_load/sum(data_combined_dwelling$total_load)
      PV_overlap=sum(pmin(PV_data$electricity_normalised[1:(8760-PV_data$hour_in_day[1])],data_combined_dwelling$load_normalised[(PV_data$hour_in_day[1]+1):8760]))
    })

    PV_overlap_stdev_across_users <- sd(PV_overlap_by_dwelling)

    # SEASONAL CONSISTENCY FACTOR

    seasonal_consistency_by_dwelling<-sapply(dwelling_indices,function(dwelling){
      data_combined_dwelling<-data_combined[which(data_combined$dwelling_index == dwelling),]
      mean_load_by_month<-aggregate(. ~ month, data_combined_dwelling[c('month','total_load')], mean)
      ordered_load<-mean_load_by_month[order(mean_load_by_month$total_load),]
      return(sum(ordered_load[1:6,]$total_load)/sum(ordered_load[7:12,]$total_load))
    })

    seasonal_consistency<-mean(seasonal_consistency_by_dwelling)

    seasonal_consistency_stdev_across_users<-sd(seasonal_consistency_by_dwelling)

    characteristics<- c(daily_energy=daily_energy,
                daily_energy_stdev_across_users=daily_energy_stdev_across_users,
                daily_energy_stdev_across_days=daily_energy_stdev_across_days,
                daily_energy_stdev_across_days_and_users=daily_energy_stdev_across_days_and_users,
                peak_load=peak_load,
                peak_load_stdev_across_users=peak_load_stdev_across_users,
                peak_load_stdev_across_days=peak_load_stdev_across_days,
                peak_load_stdev_across_days_and_users=peak_load_stdev_across_days_and_users,
                load_factor=load_factor,
                load_factor_stdev_across_users=load_factor_stdev_across_users,
                coincidence_factor=coincidence_factor,
              PV_overlap=PV_overlap,
              PV_overlap_stdev_across_users=PV_overlap_stdev_across_users,
              seasonal_consistency=seasonal_consistency,
              seasonal_consistency_stdev_across_users=seasonal_consistency_stdev_across_users)
    return(characteristics)
  })
  stats<-do.call(rbind,load_stats_daily_load_by_tier)
  stats<-cbind(tier=tiers,stats)
  return(stats)
})

load_stats_df<-as.data.frame(do.call(rbind,load_stats_by_village_tier))

load_stats_df<-cbind(climate=c(rep(villages,each=length(tiers))),load_stats_df)

load_stats_df$climate<-village_to_climate(load_stats_df$climate)

load_stats_df<-load_stats_df[order(load_stats_df$tier),]

write.csv(load_stats_df, paste(output_path,'Load_Metrics_By_Tier_Climate.csv',sep=''), row.names=FALSE)

### 5. Plot load profiles across tiers and climatic conditions


print('STEP 5. MAKE PLOTS AND EXPORT ANNUAL LOAD PROPERTIES BY DEVICE')


# Plot Mean load across day
lapply(villages, function(village){
  climate=village_to_climate(village)
  lapply(tiers, function(tier){
    # select load data for this village and tier
    data<-annual_hourly_load_by_village_tier_dwelling[[village]][[as.character(tier)]]
    # get mean across dwellings
    data_combined<-do.call(rbind,data)

    # Drop columns with all zero values (previously carried out earlier, but this caused issued with rbind as some months had different numbers of appliances from others)
    data_combined<-data_combined[, colSums(data_combined != 0) > 0]
    # Get mean for each hour in day (had planned to do this in two stages, first average across users, then across hours of the day, but actually it's as easy to do this in one go)
    data_processed<-aggregate(data_combined,by=list(data_combined$hour_in_day), mean)
    data_processed<-subset(data_processed,select=-c(Group.1,hour_tot,month,day,weekday_status))

    data_processed<-data_processed[c('hour_in_day',intersect(rev(all_techs),names(data_processed)))]

    # Export annual load by device
    total_annual_load_by_device<-colSums(data_processed)[-1]*365/1000

    write.table(total_annual_load_by_device, paste(processed_load_path,'Total_Annual_Device_Load_By_Village_Tier/',village,"_Tier",tier,'_Total_Annual_Device_Load_kWh.csv',sep=''), sep=",", row.names=TRUE)


    melted_data <- melt(data_processed, id.var='hour_in_day')
    names(melted_data)[names(melted_data)=="hour_in_day"] <- "Hour"
    names(melted_data)[names(melted_data)=="value"] <- "Load"
    names(melted_data)[names(melted_data)=="variable"] <- "Device"
    # Capitalise and replace underscores with spaces
    # Make Device a factor so ggplot doesn't rearrange order
    melted_data$Device <- factor(melted_data$Device, levels = unique(melted_data$Device))

    # Define pallettes (colorful mix for prod use, grey for domestic)
    #myColors<-c(brewer.pal(length(nondoms_by_context[[context]]$nondom_type),palletes_by_context[[context]])[1:length(nondoms_by_context[[context]]$nondom_type)],'grey')
    #names(myColors) <- unique(melted_data$Device)

    p<-ggplot(melted_data, aes(fill=Device, y=Load, x=Hour)) + 
      geom_bar(position="stack", stat="identity") +   
      coord_cartesian(expand=FALSE) +
      theme_bw() +
      theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
      ggtitle(paste('Tier ',tier,', ',climate,' climate', sep='')) +
      ylab('Mean Load (W)') +
      scale_x_continuous(breaks = seq(0, 23, 3)) +
      scale_fill_manual(name = "Device",values = myColors)

    ggsave(paste(plot_path,'Load_By_Tier/mean_daily_load_tier_',tier,'_',village,'.png',sep=''),plot=p,width=8,height=4)

  })
})

# Plot Mean load across months
lapply(villages, function(village){
  climate=village_to_climate(village)
  lapply(tiers, function(tier){
    # select load data for this village and tier
    data<-annual_hourly_load_by_village_tier_dwelling[[village]][[as.character(tier)]]
    # get mean across dwellings
    data_combined<-do.call(rbind,data)
    # Drop columns with all zero values (previously carried out earlier, but this caused issued with rbind as some months had different numbers of appliances from others)
    data_combined<-data_combined[, colSums(data_combined != 0) > 0]
    # Get mean for each hour in day (had planned to do this in two stages, first average across users, then across hours of the day, but actually it's as easy to do this in one go)
    data_processed<-aggregate(data_combined,by=list(data_combined$month), mean)
    data_processed<-subset(data_processed,select=-c(Group.1,hour_tot,hour_in_day,day,weekday_status))

    data_processed<-data_processed[c('month',intersect(rev(all_techs),names(data_processed)))]

    melted_data <- melt(data_processed, id.var='month')
    names(melted_data)[names(melted_data)=="month"] <- "Month"
    names(melted_data)[names(melted_data)=="value"] <- "Load"
    names(melted_data)[names(melted_data)=="variable"] <- "Device"
    # Capitalise and replace underscores with spaces
    # Make Device a factor so ggplot doesn't rearrange order
    melted_data$Device <- factor(melted_data$Device, levels = unique(melted_data$Device))

    # Define pallettes (colorful mix for prod use, grey for domestic)
    #myColors<-c(brewer.pal(length(nondoms_by_context[[context]]$nondom_type),palletes_by_context[[context]])[1:length(nondoms_by_context[[context]]$nondom_type)],'grey')
    #names(myColors) <- unique(melted_data$Device)

    p<-ggplot(melted_data, aes(fill=Device, y=Load, x=Month)) + 
      geom_bar(position="stack", stat="identity") +   
      coord_cartesian(expand=FALSE) +
      theme_bw() +
      theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
      ggtitle(paste('Tier ',tier,', ',climate,' climate', sep='')) +
      ylab('Mean Load (W)') +
      scale_x_continuous(breaks = seq(1, 12, 1)) +
      scale_fill_manual(name = "Device",values = myColors)

    ggsave(paste(plot_path,'Load_By_Tier/mean_monthly_load_tier_',tier,'_',village,'.png',sep=''),plot=p,width=8,height=4)

  })
})


# GENERATE DATA & PLOT SEASONAL LOAD COMPARISON

seasonal_load_stats_by_village_tier<-lapply(villages, function(village){
  climate=village_to_climate(village)
  seasonal_load_stats_daily_load_by_tier<-lapply(c(2:5), function(tier){
    # select load data for this village and tier
    data<-annual_hourly_load_by_village_tier_dwelling[[village]][[as.character(tier)]]
    data_combined<-do.call(rbind,data)

    # Add column for specific loads load
    data_combined$total_load<-rowSums(subset(data_combined,select=-c(hour_tot,month,day,weekday_status,hour_in_day)))
    data_combined$appliance_load<-data_combined$total_load - data_combined[['Fridge freezer']] - data_combined[[cooling_by_tier[[as.character(tier)]]]]

    # Add column for unique day IDs
    data_combined$month_day<-paste(data_combined$month,data_combined$day)

    daily_appliance_energy<-sum(data_combined$appliance_load)/365/N_households
    daily_fridge_energy<-sum(data_combined[['Fridge freezer']])/365/N_households
    daily_cooling_energy<-sum(data_combined[[cooling_by_tier[[as.character(tier)]]]])/365/N_households

    # stdev across days
    daily_energy_by_day <- aggregate(. ~ month_day, data_combined[c('month_day','total_load')], sum)
    daily_energy_stdev_across_days <- sd(daily_energy_by_day$total_load)/N_households

    # stdev across days
    seasonal_load_stats<-c(daily_appliance_energy,daily_fridge_energy,daily_cooling_energy)
    names(seasonal_load_stats)<-c('Other Appliances','Fridge freezer',cooling_by_tier[[as.character(tier)]])
    melted_seasonal_load_stats<-melt(seasonal_load_stats)


    return(cbind(variable=names(seasonal_load_stats),climate=climate,tier=paste('T',tier,sep=''),melted_seasonal_load_stats))
  })
  stats<-as.data.frame(do.call(bind_rows,seasonal_load_stats_daily_load_by_tier))
  return(stats)
})
seasonal_load_stats_by_village_tier_df<-do.call(rbind,seasonal_load_stats_by_village_tier)
seasonal_load_stats_by_village_tier_df

seasonal_load_stats_by_village_tier_df[['Tier_Climate']]<-paste(seasonal_load_stats_by_village_tier_df$tier,seasonal_load_stats_by_village_tier_df$climate,sep=' / ')

Seasonal_Colors<-c(brewer.pal(length(cooling_techs),'Blues')[1:length(cooling_techs)],
             "#31A354",
             'grey')
names(Seasonal_Colors) <-c(cooling_techs,'Fridge freezer','Other Appliances')


p<-ggplot(seasonal_load_stats_by_village_tier_df, aes(x = Tier_Climate, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'stack') +
  coord_cartesian(expand=FALSE) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
  ylim(0,8000) +
  ylab('Daily Energy (Wh)') +
  xlab('Tier / Climate') +
  scale_fill_manual(name = "Source",values = Seasonal_Colors)

ggsave(paste(plot_path,'Seasonal_Loads_By_Tier.png',sep=''),plot=p,width=8,height=4)

