# Script to process excel outputs from CREST demand model and generate cleaner csv files for analysis in later scripts
# Sheridan Few, Nov 2020
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
library(viridis)

### SETTINGS

# Ensures plot come out the correct scale
quartz.options(width=8, height=4)

### PATH DEFINITION

root_path <- '/Users/Shez/Google\ Drive/Grantham/ElDemAcc/'

### INPUT PATHS

# A yearsworth of load from various sources to combine into a ten year profile in scenarios:

# Meshpower dom demand
Meshpower_dom_input_path <- paste(root_path,'Meshpower/Processed_Load/Annual_Hourly_Load_By_Dom_User/',sep='')

# Meshpower nondom demand
Meshpower_nondom_input_path <- paste(root_path,'Meshpower/Processed_Load/Annual_Hourly_Load_By_Nondom_Type/',sep='')

# Gram Oorja demand
GramOorja_input_path <- paste(root_path,'GramOorja/Processed_Load/Annual_Hourly_Load_By_Source/',sep='')

# CREST Demand Data by Village and Tier
CREST_input_path <- paste(root_path,'CREST/Processed_Load/Annual_Hourly_Load_By_Village_Tier_Dwelling/',sep='')

# THOUGHTS - Would be better here to select a random household each time one is added

output_path <- paste(root_path,'Outputs/',sep='')
plot_path <- paste(root_path,'Plots/',sep='')


### RANGES TO CONSIDER

N_households<-100
years_of_microgrid_operation<-10


 villages<-c('Gitaraga','Bhinjpur')
 climates<-c('TSav','HSub')


 contexts<-c('accessible','remote')
 tiers<-c(1:5)
 months<-c(1:12)
 households<-c(1:N_households)

nondoms_by_context<-list()
nondoms_by_context[['accessible']]<-as.data.frame(rbind(c('bar',4),c('cinema',1),c('hairdresser',2),c('mosque',1),c('shop',1),c('tailor',2),c('workshop',1)))
nondoms_by_context[['remote']]<-as.data.frame(rbind(c('Irrigation_Pump',1),c('Rice_Mill',1)))

nondoms_by_context<-lapply(nondoms_by_context,function(df){
	names(df)<-c('nondom_type','number')
	df$number<-as.numeric(as.character(df$number))
	rownames(df)<-df$nondom_type
	return(df)
})

# Parameters defining scenarios

scenario_ids<-c('slow','slowplus','medium','medplus','fast','fastplus','faster')
k_values<-c(1,1,1,1,1,1,1)
speed_values<-c(2,2,3,3,4,4,5)

midyear_by_tier_baseline<-c(0,10,20,30,40)

years<-c(1:years_of_microgrid_operation)


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


# DO STUFF

### 1. Import Data

# Dom data

Meshpower_annual_load_data_by_household_files <- list.files(path=Meshpower_dom_input_path, pattern="*.csv", full.names=TRUE, recursive=TRUE) # List of files containing usage data. 

Meshpower_annual_load_data_by_household<-lapply(Meshpower_annual_load_data_by_household_files,read.csv)

# Nondom data (by accessible/remote context)

nondom_data_by_context_type<-list()

nondom_data_by_context_type[['accessible']]<-lapply(nondoms_by_context[['accessible']]$nondom_type,function(nondom_type){
	read.csv(paste(Meshpower_nondom_input_path,nondom_type,'_annual_hourly.csv',sep=''))
})
names(nondom_data_by_context_type[['accessible']])<-nondoms_by_context[['accessible']]$nondom_type

nondom_data_by_context_type[['remote']]<-lapply(nondoms_by_context[['remote']]$nondom_type,function(nondom_type){
	read.csv(paste(GramOorja_input_path,nondom_type,'_annual_hourly.csv',sep=''))
})
names(nondom_data_by_context_type[['remote']])<-nondoms_by_context[['remote']]$nondom_type

 # Simulated load by tier/climate

CREST_input_files<-list.files(path=CREST_input_path, pattern="*.csv", full.names=TRUE, recursive=TRUE) 

 simulated_load_data_by_village_tier_household<-lapply(villages, function(village){
 	 simulated_load_data_by_tier_household<-lapply(tiers, function(tier){
 	 	village_tier_files <- subset(CREST_input_files, grepl(paste(village,"_Tier",tier,sep=''), CREST_input_files))
		simulated_load_data_by_household<-lapply(village_tier_files,read.csv)
		return(simulated_load_data_by_household)
	})
 	names(simulated_load_data_by_tier_household)<-tiers
 	return(simulated_load_data_by_tier_household)
 })
names(simulated_load_data_by_village_tier_household)<-villages

### 2. Generate a current year's profile for a remote and accessible setting

# Domestic profiles:

# Combine N_households, each a random member of the input data

domestic_profiles <- lapply(households,function(household_number){
	# Random number for import household id
	import_household_id<-ceiling(runif(1)*length(Meshpower_annual_load_data_by_household))
	df<-Meshpower_annual_load_data_by_household[[import_household_id]]
	df$hour_of_year <- c(1:nrow(df))
	return(df)
	})

domestic_profiles_bound<-do.call(rbind,domestic_profiles)
total_domestic_profile<-within(domestic_profiles[[1]], rm(load))
total_domestic_profile$load<-aggregate(domestic_profiles_bound,by=list(domestic_profiles_bound$hour_of_year), sum)$load

# Nondom profiles:

nondom_profiles_by_context_type<-lapply(contexts,function(context){
	nondom_profiles_by_type<-lapply(nondoms_by_context[[context]]$nondom_type,function(nondom_type){
		data_one_nondom<-nondom_data_by_context_type[[context]][[nondom_type]]
		data_n_nondoms<-within(data_one_nondom, rm(load))
		data_n_nondoms$load<-data_one_nondom$load * nondoms_by_context[[context]][nondom_type,]$number
		return(data_n_nondoms)
	})
	names(nondom_profiles_by_type)<-nondoms_by_context[[context]]$nondom_type
	return(nondom_profiles_by_type)
})
names(nondom_profiles_by_context_type)<-contexts


# As above, but only load rather than time related values

nondom_profiles_load_only_by_context_type<-lapply(contexts,function(context){
	nondom_profiles_by_type<-lapply(nondoms_by_context[[context]]$nondom_type,function(nondom_type){
		return(nondom_profiles_by_context_type[[context]][[nondom_type]]$load)
	})
	names(nondom_profiles_by_type)<-nondoms_by_context[[context]]$nondom_type
	return(nondom_profiles_by_type)
})
names(nondom_profiles_load_only_by_context_type)<-contexts

# All profiles in one dataframe

all_profiles_by_context<-lapply(contexts,function(context){
	df<-total_domestic_profile
	names(df) <- gsub(names(df), pattern = "load", replacement = "domestic")  
	df<-cbind(df,do.call(cbind,nondom_profiles_load_only_by_context_type[[context]]))
	return(df)
})
names(all_profiles_by_context)<-contexts

# Convert to mean hourly for one day from all types

mean_daily_profiles_by_context<-lapply(contexts,function(context){
	full_data<-within(all_profiles_by_context[[context]], rm(month, day, weekday_status,hour_of_year))
	df<-aggregate(full_data,by=list(full_data$hour), mean)
	return(within(df, rm(Group.1)))
})
names(mean_daily_profiles_by_context)<-contexts

# Convert to mean monthly from all types

mean_monthly_profiles_by_context<-lapply(contexts,function(context){
	full_data<-within(all_profiles_by_context[[context]], rm(hour, day, weekday_status,hour_of_year))
	df<-aggregate(full_data,by=list(full_data$month), mean)
	return(within(df, rm(Group.1)))
})
names(mean_monthly_profiles_by_context)<-contexts


### 3. Plot current load profile for a remote and accessible setting

# Define color pallettes

#myColors<-c(brewer.pal(length(nondoms_by_context[[context]]$nondom_type),palletes_by_context[[context]])[1:length(nondoms_by_context[[context]]$nondom_type)],'grey')
#names(myColors) <- unique(melted_data$Source)

# Alternative color scheme approach (not implemented)

#palletes_by_context<-list(remote='Greens',accessible='YlOrRd')

#myColors<-c(brewer.pal(length(nondoms_by_context[[context]]$nondom_type),palletes_by_context[[context]])[1:length(nondoms_by_context[[context]]$nondom_type)],'grey')
#names(myColors) <- unique(melted_data$Source)

# Plot Mean load across day
lapply(contexts,function(context){
	data<-mean_daily_profiles_by_context[[context]]
	data<-data%>%select(-domestic,everything())
	melted_data <- melt(data, id.var='hour')
	names(melted_data)[names(melted_data)=="hour"] <- "Hour"
	names(melted_data)[names(melted_data)=="value"] <- "Load"
	names(melted_data)[names(melted_data)=="variable"] <- "Source"
	# Capitalise and replace underscores with spaces
	melted_data$Source<-tools::toTitleCase(gsub(melted_data$Source, pattern = "_", replacement = " "))
	# Make source a factor so ggplot doesn't rearrange order
	melted_data$Source <- factor(melted_data$Source, levels = unique(melted_data$Source))

	#  Define pallettes (colorful mix for prod use, grey for domestic)
	if(context=='accessible'){
		myColors<-c(plasma(length(nondoms_by_context[[context]]$nondom_type)),'grey')
		names(myColors) <- unique(melted_data$Source)
	} else {
		myColors<-c(brewer.pal(length(nondoms_by_context[[context]]$nondom_type),'Greens')[1:length(nondoms_by_context[[context]]$nondom_type)],'grey')
		names(myColors) <- unique(melted_data$Source)
	}

	p<-ggplot(melted_data, aes(fill=Source, y=Load, x=Hour)) + 
	  geom_bar(position="stack", stat="identity") +   
	  coord_cartesian(expand=FALSE) +
	  theme_bw() +
	  theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
	  ylab('Mean Load (W)') +
	  scale_x_continuous(breaks = seq(0, 23, 3)) +
	  scale_fill_manual(name = "Source",values = myColors)

	ggsave(paste(plot_path,context,'_mean_daily_load.png',sep=''),plot=p,width=8,height=4)
})


# Plot Mean load across months
lapply(contexts,function(context){
	data<-mean_monthly_profiles_by_context[[context]]
	data<-data%>%select(-domestic,everything())
	melted_data <- melt(data, id.var='month')
	names(melted_data)[names(melted_data)=="month"] <- "Month"
	names(melted_data)[names(melted_data)=="value"] <- "Load"
	names(melted_data)[names(melted_data)=="variable"] <- "Source"
	# Capitalise and replace underscores with spaces
	melted_data$Source<-tools::toTitleCase(gsub(melted_data$Source, pattern = "_", replacement = " "))
	# Make source a factor so ggplot doesn't rearrange order
	melted_data$Source <- factor(melted_data$Source, levels = unique(melted_data$Source))

	if(context=='accessible'){
		myColors<-c(plasma(length(nondoms_by_context[[context]]$nondom_type)),'grey')
		names(myColors) <- unique(melted_data$Source)
	} else {
		myColors<-c(brewer.pal(length(nondoms_by_context[[context]]$nondom_type),'Greens')[1:length(nondoms_by_context[[context]]$nondom_type)],'grey')
		names(myColors) <- unique(melted_data$Source)
	}

	p<-ggplot(melted_data, aes(fill=Source, y=Load, x=Month)) + 
	  geom_bar(position="stack", stat="identity") +   
	  coord_cartesian(expand=FALSE) +
	  theme_bw() +
	  theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
	  ylab('Mean Load (W)') +
	  scale_x_continuous(breaks = seq(1, 12, 1)) +
	  scale_fill_manual(name = "Source",values = myColors)

	ggsave(paste(plot_path,context,'_mean_monthly_load.png',sep=''),plot=p,width=8,height=4)
})

### 3b. Characterise loads and plot

nondom_profiles_single_load_only_by_context_type<-lapply(contexts,function(context){
	nondom_profiles_by_type<-lapply(nondoms_by_context[[context]]$nondom_type,function(nondom_type){
		data_one_nondom<-nondom_data_by_context_type[[context]][[nondom_type]]
		return(data_one_nondom$load)
	})
	names(nondom_profiles_by_type)<-nondoms_by_context[[context]]$nondom_type
	return(do.call(cbind,nondom_profiles_by_type))
})

all_profiles_single<-cbind(total_domestic_profile/N_households,(do.call(cbind,nondom_profiles_single_load_only_by_context_type)))



### 4. Output CLOVER profile for continued current levels of demand:

# Hourly Load throughout system operation period:

CLOVER_no_growth_profile_by_context<-lapply(contexts,function(context){
	# Convert load into a CLOVER readable format
	load_data<-within(all_profiles_by_context[[context]], rm(month, day, weekday_status,hour,hour_of_year))
	nondom_load_data<-rowSums(within(load_data, rm(domestic)))
	CLOVER_load_data_oneyr<-cbind(Domestic=load_data$domestic,Commercial=nondom_load_data,Public=0.0)
	CLOVER_load_data_Nyrs<-do.call("rbind", replicate(years_of_microgrid_operation, CLOVER_load_data_oneyr, simplify = FALSE))
	hour_id<-c(0:(8760*years_of_microgrid_operation - 1))
	CLOVER_load_data_Nyrs<-as.data.frame(cbind(hour_id,CLOVER_load_data_Nyrs))
	names(CLOVER_load_data_Nyrs)[names(CLOVER_load_data_Nyrs)=="hour_id"] <- ""

	# Output to relevant folder
	write.csv(CLOVER_load_data_Nyrs, paste(output_path,'CLOVER_Loads/NoGrowth_',context,'_total_load.csv',sep=''), row.names=FALSE)

	return(CLOVER_load_data_Nyrs)
})

# Calculate yearly load stats

yearly_load_stats_no_growth_by_context<-lapply(contexts,function(context){
	Total_load<-CLOVER_no_growth_profile_by_context[[context]]$Domestic+
				CLOVER_no_growth_profile_by_context[[context]]$Commercial+
				CLOVER_no_growth_profile_by_context[[context]]$Public

	Total_Load_w_year<-as.data.frame(cbind(year=rep(1:years_of_microgrid_operation, each=365*24),Total_load))
	yearly_stats<-sapply(c(1:years_of_microgrid_operation),function(year){
		max<-max(Total_Load_w_year$Total_load[which(Total_Load_w_year$year==year)])
		mean<-mean(Total_Load_w_year$Total_load[which(Total_Load_w_year$year==year)])
		med<-median(Total_Load_w_year$Total_load[which(Total_Load_w_year$year==year)])
		return(c(Maximum=max,Mean=mean,Median=med))
	})

	year_id<-c(0:(years_of_microgrid_operation-1))

	yearly_stats<-t(rbind(year_id,yearly_stats))

	colnames(yearly_stats)[colnames(yearly_stats)=="year_id"] <- ""

	write.csv(yearly_stats, paste(output_path,'CLOVER_Loads/NoGrowth_',context,'_yearly_load_stats.csv',sep=''), row.names=FALSE)

	return(max(yearly_stats))
})

# Calculate max daily load (used in deciding parameter space to explore with CLOVER)

max_daily_load_no_growth_by_context<-lapply(contexts,function(context){
	Total_load<-CLOVER_no_growth_profile_by_context[[context]]$Domestic+
				CLOVER_no_growth_profile_by_context[[context]]$Commercial+
				CLOVER_no_growth_profile_by_context[[context]]$Public
	daily_loads<-colSums(matrix(Total_load, nrow=24))

	print(paste('max_daily_load[NoGrowth_',context,'_total_load]=',round(max(daily_loads),2),sep=''))

	return(max(daily_loads))
})


names(CLOVER_no_growth_profile_by_context)<-contexts



### 5. Generate S curves for proportion of housholds achieving tiers by year

years<-c(1:years_of_microgrid_operation)


load_growth_scenario_inputs<-cbind(k_values,speed_values)
rownames(load_growth_scenario_inputs)<-scenario_ids

# Generate S curves for proportion achieving tier by year
S_curves_by_scenario_id_tier_year<-lapply(scenario_ids,function(scenario_id){
	 S_curves_by_tier_year<-lapply(tiers,function(tier){
	 	midyear<-midyear_by_tier_baseline[tier]/load_growth_scenario_inputs[scenario_id,'speed_values']
	 	proportion_achieving_by_year<-lapply(years,function(year){
	 		proportion=(1/(1+exp(-load_growth_scenario_inputs[scenario_id,'k_values']*(year-midyear))))
	 		return(proportion)
	 	})
	 	return(as.numeric(proportion_achieving_by_year))
	 })
	 S_curves_by_tier_year_joined<-do.call(rbind, S_curves_by_tier_year)
	 rownames(S_curves_by_tier_year_joined)<-tiers
	 colnames(S_curves_by_tier_year_joined)<-years
	 return(S_curves_by_tier_year_joined)
})
names(S_curves_by_scenario_id_tier_year)<-scenario_ids

# Manual edits

# Tier 1 achieved at start of each scenario
S_curves_by_scenario_id_tier_year[['slow']][1,]<-1.0
S_curves_by_scenario_id_tier_year[['slowplus']][1,]<-1.0
S_curves_by_scenario_id_tier_year[['medium']][1,]<-1.0
S_curves_by_scenario_id_tier_year[['medplus']][1,]<-1.0
S_curves_by_scenario_id_tier_year[['fast']][1,]<-1.0
S_curves_by_scenario_id_tier_year[['fastplus']][1,]<-1.0
S_curves_by_scenario_id_tier_year[['faster']][1,]<-1.0

# Tier 3,4,5 not achieved in slow,medium,fast scenario respectively
# Tier 4,5 not achieved in slowplus,medplus scenario respectively
S_curves_by_scenario_id_tier_year[['slow']][3,]<-0.0
S_curves_by_scenario_id_tier_year[['slow']][4,]<-0.0
S_curves_by_scenario_id_tier_year[['slow']][5,]<-0.0
S_curves_by_scenario_id_tier_year[['medium']][4,]<-0.0
S_curves_by_scenario_id_tier_year[['medium']][5,]<-0.0
S_curves_by_scenario_id_tier_year[['fast']][5,]<-0.0

S_curves_by_scenario_id_tier_year[['slowplus']][4,]<-0.0
S_curves_by_scenario_id_tier_year[['slowplus']][5,]<-0.0
S_curves_by_scenario_id_tier_year[['medplus']][5,]<-0.0

# Proportion of households at tier (rather than at at least this tier)
Proportion_by_scenario_id_tier_year<-lapply(scenario_ids,function(scenario_id){
	Proportion_by_tier_year<-S_curves_by_scenario_id_tier_year[[scenario_id]]
	Proportion_by_tier_year[1,]<-S_curves_by_scenario_id_tier_year[[scenario_id]][1,]-S_curves_by_scenario_id_tier_year[[scenario_id]][2,]
	Proportion_by_tier_year[2,]<-S_curves_by_scenario_id_tier_year[[scenario_id]][2,]-S_curves_by_scenario_id_tier_year[[scenario_id]][3,]
	Proportion_by_tier_year[3,]<-S_curves_by_scenario_id_tier_year[[scenario_id]][3,]-S_curves_by_scenario_id_tier_year[[scenario_id]][4,]
	Proportion_by_tier_year[4,]<-S_curves_by_scenario_id_tier_year[[scenario_id]][4,]-S_curves_by_scenario_id_tier_year[[scenario_id]][5,]
	return(Proportion_by_tier_year)
	})
names(Proportion_by_scenario_id_tier_year)<-scenario_ids

### 6. Plot S curves for proportion of housholds achieving tiers by year
S_curve_plots_by_scenario_id<-lapply(scenario_ids,function(scenario_id){
	data<-S_curves_by_scenario_id_tier_year[[scenario_id]]

	# Remove tiers which stay at 0 from plots
	if(scenario_id=='slow')
	{
		data<-head(data,n=2)
	}

		if(scenario_id=='slowplus')
	{
		data<-head(data,n=3)
	}

	if(scenario_id=='medium')
	{
		data<-head(data,n=3)
	}

	if(scenario_id=='medplus')
	{
		data<-head(data,n=4)
	}

	if(scenario_id=='fast')
	{
		data<-head(data,n=4)
	}

	# Reshape data

	melted_S_curve_data <- melt(data)
	names(melted_S_curve_data)[names(melted_S_curve_data)=="Var1"] <- "Tier"
	names(melted_S_curve_data)[names(melted_S_curve_data)=="Var2"] <- "Year"
	names(melted_S_curve_data)[names(melted_S_curve_data)=="value"] <- "Proportion_Achieving"
	melted_S_curve_data$Year<-melted_S_curve_data$Year+2020
	melted_S_curve_data$Tier <- factor(melted_S_curve_data$Tier, levels = unique(melted_S_curve_data$Tier))


	# Define pallettes (colorful mix for prod use, grey for domestic)
	myColors<-brewer.pal(length(tiers),"Set1")
	names(myColors) <- tiers

	p<-ggplot(data=melted_S_curve_data, aes(x=Year, y=Proportion_Achieving, group=Tier, color=Tier)) +
	  geom_line() + geom_point()+
	  theme_minimal() +
	  theme(axis.text.x = element_text(angle=90, hjust=1)) +
	  ylab('Proportion of households') +
  	  scale_color_manual(name = "Tier",values = myColors) +
	  scale_x_continuous(breaks = seq(2021, 2030, 1))

	ggsave(paste(plot_path,'S_Curves/',scenario_id,'_S_curve.png',sep=''),plot=p,width=4.5,height=2.5)

	return(p)
})

### 7. Generate CLOVER inputs across scenarios:

# Hourly Load:

CLOVER_profile_by_scenario_climate_context<-lapply(scenario_ids,function(scenario){
	CLOVER_profile_by_climate_context<-lapply(climates,function(climate){
		village<-climate
		village<-gsub(village, pattern = "TSav", replacement = "Gitaraga")  
		village<-gsub(village, pattern = "HSub", replacement = "Bhinjpur") 
		dom_profile_by_year<-lapply(years,function(year){
			dom_profile_by_year_tier<-lapply(tiers,function(tier){
				N_households_at_tier<-round(N_households*Proportion_by_scenario_id_tier_year[[scenario]][tier,year])
				if(N_households_at_tier == 0){
					profile=rep(0,8760)
				} else {
				# Get random profile for each householder in this tier for this year (as per domestic profiles based on Meshpower data)
					dom_profile_by_household<-lapply(c(1:N_households_at_tier),function(household){
						import_household_id<-ceiling(runif(1)*length(simulated_load_data_by_village_tier_household[[village]][[tier]]))	
						return(simulated_load_data_by_village_tier_household[[village]][[tier]][[import_household_id]]$load)
					})
				# Sum these profiles for overall load for this tier (only if more than one household, get error if this isn't specified)
					if(N_households_at_tier == 1) {
						profile=dom_profile_by_household[[1]]
					} else { 
						profile=rowSums(do.call(cbind, dom_profile_by_household))
					}
				}
				return(profile)
			})
			# Sum across tiers
			dom_profile_by_year<-rowSums(do.call(cbind,dom_profile_by_year_tier))
			return(dom_profile_by_year)
		})

		# Bind years into one column for hourly dom load across 10 years
		dom_profile <- do.call(c,dom_profile_by_year)

		CLOVER_profile_by_context<-lapply(contexts,function(context){
			Base_profile<-CLOVER_no_growth_profile_by_context[[context]]
			Profile<-Base_profile
			Profile$Domestic<-dom_profile
			write.csv(Profile, paste(output_path,'CLOVER_Loads/',scenario,'_growth_',climate,'_climate_',context,'_total_load.csv',sep=''), row.names=FALSE)

			# Add time data in dataframe version (used for later analysis)
			time_data<-do.call("rbind", replicate(years_of_microgrid_operation, subset(domestic_profiles[[1]],select =-c(load)), simplify = FALSE))

			Profile<-cbind(time_data,Profile[2:4])

			return(Profile)
		})
		names(CLOVER_profile_by_context)<-contexts
		return(CLOVER_profile_by_context)
	})
	names(CLOVER_profile_by_climate_context)<-climates
	return(CLOVER_profile_by_climate_context)
})
names(CLOVER_profile_by_scenario_climate_context)<-scenario_ids


# Yearly Load Stats:

yearly_load_stats_by_scenario_climate_context<-lapply(scenario_ids,function(scenario){
	yearly_load_stats_by_climate_context<-lapply(climates,function(climate){
		yearly_load_stats_by_context<-lapply(contexts,function(context){
			Total_load<-CLOVER_profile_by_scenario_climate_context[[scenario]][[climate]][[context]]$Domestic+
						CLOVER_profile_by_scenario_climate_context[[scenario]][[climate]][[context]]$Commercial+
						CLOVER_profile_by_scenario_climate_context[[scenario]][[climate]][[context]]$Public

			Total_Load_w_year<-as.data.frame(cbind(year=rep(1:years_of_microgrid_operation, each=365*24),Total_load))
			yearly_stats<-sapply(c(1:years_of_microgrid_operation),function(year){
				max<-max(Total_Load_w_year$Total_load[which(Total_Load_w_year$year==year)])
				mean<-mean(Total_Load_w_year$Total_load[which(Total_Load_w_year$year==year)])
				med<-median(Total_Load_w_year$Total_load[which(Total_Load_w_year$year==year)])
				return(c(Maximum=max,Mean=mean,Median=med))
			})

			year_id<-c(0:(years_of_microgrid_operation-1))

			yearly_stats<-t(rbind(year_id,yearly_stats))

			colnames(yearly_stats)[colnames(yearly_stats)=="year_id"] <- ""

			write.csv(yearly_stats, paste(output_path,'CLOVER_Loads/',scenario,'_growth_',climate,'_climate_',context,'_yearly_load_stats.csv',sep=''), row.names=FALSE)

			return(yearly_stats)

			})
		names(yearly_load_stats_by_context)<-contexts
		return(yearly_load_stats_by_context)
	})
	names(yearly_load_stats_by_climate_context)<-climates
	return(yearly_load_stats_by_climate_context)
})
names(yearly_load_stats_by_scenario_climate_context)<-scenario_ids


# Max daily load (used in optimisation inputs determining parameter space to explore with CLOVER)

# Used in troubleshooting (should be commented out)
scenario=scenario_ids[1]
climate=climates[1]
context=contexts[1]


max_daily_load_by_scenario_climate_context<-lapply(scenario_ids,function(scenario){
	max_daily_load_by_climate_context<-lapply(climates,function(climate){
		max_daily_load_by_context<-lapply(contexts,function(context){
			Total_load<-CLOVER_profile_by_scenario_climate_context[[scenario]][[climate]][[context]]$Domestic+
						CLOVER_profile_by_scenario_climate_context[[scenario]][[climate]][[context]]$Commercial+
						CLOVER_profile_by_scenario_climate_context[[scenario]][[climate]][[context]]$Public
			daily_loads<-colSums(matrix(Total_load, nrow=24))

			print(paste('max_daily_load[',scenario,'_growth_',climate,'_climate_',context,'_total_load]=',round(max(daily_loads),2),sep=''))
			return(max(daily_loads))
			})
		names(max_daily_load_by_context)<-contexts
		return(max_daily_load_by_context)
	})
	names(max_daily_load_by_climate_context)<-climates
	return(max_daily_load_by_climate_context)
})
names(max_daily_load_by_scenario_climate_context)<-scenario_ids

# Mean daily load profile in final year (plotted later)

mean_daily_profile_by_scenario_climate_context<-lapply(scenario_ids,function(scenario){
	mean_daily_profile_by_climate_context<-lapply(climates,function(climate){
		mean_daily_profile_by_context<-lapply(contexts,function(context){
			final_year_start_hour=8760*(years_of_microgrid_operation-1)+1
			final_year_end_hour=8760*years_of_microgrid_operation
			final_year_data<-CLOVER_profile_by_scenario_climate_context[[scenario]][[climate]][[context]][final_year_start_hour:final_year_end_hour,]
			final_year_data<-within(final_year_data, rm(month, day, weekday_status,hour_of_year))
			df<-aggregate(final_year_data,by=list(final_year_data$hour), mean)
			return(within(df, rm(Group.1)))
			})
		names(mean_daily_profile_by_context)<-contexts
		return(mean_daily_profile_by_context)
	})
	names(mean_daily_profile_by_climate_context)<-climates
	return(mean_daily_profile_by_climate_context)
})
names(mean_daily_profile_by_scenario_climate_context)<-scenario_ids


# Mean monthly load (plotted later)

mean_monthly_load_by_scenario_climate_context<-lapply(scenario_ids,function(scenario){
	mean_monthly_load_by_climate_context<-lapply(climates,function(climate){
		mean_monthly_load_by_context<-lapply(contexts,function(context){
			final_year_start_hour=8760*(years_of_microgrid_operation-1)+1
			final_year_end_hour=8760*years_of_microgrid_operation
			final_year_data<-CLOVER_profile_by_scenario_climate_context[[scenario]][[climate]][[context]][final_year_start_hour:final_year_end_hour,]
			final_year_data<-within(final_year_data, rm(hour, day, weekday_status,hour_of_year))
			df<-aggregate(final_year_data,by=list(final_year_data$month), mean)
			return(within(df, rm(Group.1)))
			})
		names(mean_monthly_load_by_context)<-contexts
		return(mean_monthly_load_by_context)
	})
	names(mean_monthly_load_by_climate_context)<-climates
	return(mean_monthly_load_by_climate_context)
})
names(mean_monthly_load_by_scenario_climate_context)<-scenario_ids




### 8. Plot final year load profile across (1) growth scenarios, (2) climatic conditions, and (3) remote/accessible settings

# Define colors

color_by_context<-list(remote='green',accessible='orange')

# Plot Mean load across day

lapply(scenario_ids,function(scenario){
	lapply(climates,function(climate){
		lapply(contexts,function(context){
			data<-mean_daily_profile_by_scenario_climate_context[[scenario]][[climate]][[context]]
			data<-within(data, rm(Public))
			names(data) <- gsub(names(data), pattern = "Commercial", replacement = "Nondomestic")  
			melted_data <- melt(data, id.var='hour')
			names(melted_data)[names(melted_data)=="hour"] <- "Hour"
			names(melted_data)[names(melted_data)=="value"] <- "Load"
			names(melted_data)[names(melted_data)=="variable"] <- "Source"
			# Capitalise and replace underscores with spaces
			melted_data$Source<-tools::toTitleCase(gsub(melted_data$Source, pattern = "_", replacement = " "))
			# Make source a factor so ggplot doesn't rearrange order
			melted_data$Source <- factor(melted_data$Source, levels = unique(melted_data$Source))

			# Define pallettes (colorful mix for prod use, grey for domestic)
			myColors<-c('grey',color_by_context[[context]])
			names(myColors) <- unique(melted_data$Source)

			p<-ggplot(melted_data, aes(fill=Source, y=Load, x=Hour)) + 
			  geom_bar(position="stack", stat="identity") +   
			  coord_cartesian(expand=FALSE) +
			  theme_bw() +
			  theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
			  ylab('Mean Load (W)') +
			  scale_x_continuous(breaks = seq(0, 23, 3)) +
			  scale_fill_manual(name = "Source",values = myColors)

			ggsave(paste(plot_path,'Final_Year/mean_daily_load_',context,'_',scenario,'_',climate,'.png',sep=''),plot=p,width=8,height=4)
		})
	})
})

# Plot Mean load across months

lapply(scenario_ids,function(scenario){
	lapply(climates,function(climate){
		lapply(contexts,function(context){
			data<-mean_monthly_load_by_scenario_climate_context[[scenario]][[climate]][[context]]
			data<-within(data, rm(Public))
			names(data) <- gsub(names(data), pattern = "Commercial", replacement = "Nondomestic")  
			melted_data <- melt(data, id.var='month')
			names(melted_data)[names(melted_data)=="month"] <- "Month"
			names(melted_data)[names(melted_data)=="value"] <- "Load"
			names(melted_data)[names(melted_data)=="variable"] <- "Source"
			# Capitalise and replace underscores with spaces
			melted_data$Source<-tools::toTitleCase(gsub(melted_data$Source, pattern = "_", replacement = " "))
			# Make source a factor so ggplot doesn't rearrange order
			melted_data$Source <- factor(melted_data$Source, levels = unique(melted_data$Source))

			# Define pallettes (colorful mix for prod use, grey for domestic)
			myColors<-c('grey',color_by_context[[context]])
			names(myColors) <- unique(melted_data$Source)

			p<-ggplot(melted_data, aes(fill=Source, y=Load, x=Month)) + 
			  geom_bar(position="stack", stat="identity") +   
			  coord_cartesian(expand=FALSE) +
			  theme_bw() +
			  theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
			  ylab('Mean Load (W)') +
	  		  scale_x_continuous(breaks = seq(1, 12, 1)) +
			  scale_fill_manual(name = "Source",values = myColors)

			ggsave(paste(plot_path,'Final_Year/monthly_load_',context,'_',scenario,'_',climate,'.png',sep=''),plot=p,width=8,height=4)
		})
	})
})


