# Script to process raw outputs from CREST demand model and generate cleaner csv files for analysis in later scripts
# Sheridan Few, Oct 2020
# See also readme file

### PACKAGES

library(data.table)
library(zoo)
library(dplyr)
library(ggplot2)


### PATH DEFINITION

root_path <- '/Users/Shez/Google\ Drive/Grantham/ElDemAcc/'
input_path <-  paste(root_path,'CREST/Processed_Load/',sep='')
plot_path <- paste(root_path,'Plots/',sep='')

PV_input_file='/Users/Shez/Google Drive/Grantham/RENGA/PV_profiles/MtAbu_PV.csv'

PV_data<-read.csv(PV_file,skip = 3, header = T)

PV_data['local_time'] <- lapply(PV_data['local_time'], strptime, "%m/%d/%Y %H:%M")

PV_data$hour_tot<-as.integer(as.numeric(PV_data$local_time)/3600) # track hours (absolute, ie each day adds 24)

PV_data$hour_in_day=PV_data$hour_tot%%24

PV_mean<-aggregate(electricity ~ hour_in_day, PV_data, mean)

PV_mean_normalised<-PV_mean/sum(PV_mean$electricity)


# FUNCTIONS

# Split load data by dwelling index (also fixes data type of some columns
split_load_CREST <- function (x,data){
	load_data<-subset(data,data['Dwelling index']==x) # Subset by dwelling index
	load_data<-cbind(load_data[3],load_data[1],load_data[,-c(1:3)])
	load_data_numeric <- mutate_all(load_data[-1], function(x) as.numeric(as.character(x))) # Convert data to numeric (R doesn't recogines it as such yet - perhaps because input data has no decimal point)
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

# Convert minute by minute data to hourly peak. Would also work for other sub-hour temporal resolutions
convert_to_hourly_peak <- function(x){
	data<-aggregate(x, list(cut(as.POSIXct(x$Time,tz='Africa/Kigali'), "1 hour")), max)
	out<-data[-1]
	out$Time<-format(as.POSIXct(data[[1]],tz='Africa/Kigali'), "%H")
	#out$Time<-as.POSIXct(data[[1]],tz='Africa/Kigali')
	return(out)
}

sum_load <- function(x){
	total<-cbind(x[1:2],as.data.frame(rowSums(x[-1][-1])))
	colnames(total) <- c('Time','Dwelling Index','Total Load')
	return(total)
}

plot_load_w_tier <- function(x,name,ylab,title) {
 data <- x
 data$Hour <- rownames(data)
 melted_data <- melt(data, id.var='Hour')
 names(melted_data)[names(melted_data)=="value"] <- "Load"
 names(melted_data)[names(melted_data)=="variable"] <- "Device"

 ggplot(melted_data, aes(fill=Device, y=Load, x=Hour)) + 
  geom_bar(position="stack", stat="identity") +   
  coord_cartesian(expand=FALSE) +
  theme_bw() +
  theme(text = element_text(size=10),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
  ggtitle(title) +
  ylab(ylab) +
  geom_hline(yintercept=1/24, linetype="dashed", color = "red") +
  geom_label(x=3, y=1/24, label='T1', color="red", fill='white',size=7 ) +
  geom_hline(yintercept=200/24, linetype="dashed", color = "yellow") +
  geom_label(x=3, y=200/24, label='T2', color="yellow", fill='white',size=7 ) +
  geom_hline(yintercept=1000/24, linetype="dashed", color = "blue") +
  geom_label(x=3, y=1000/24, label='T3', color="blue", fill='white',size=7 ) +
  geom_hline(yintercept=3400/24, linetype="dashed", color = "green") +
  geom_label(x=3, y=3400/24, label='T4', color="green", fill='white',size=7 ) +
  geom_hline(yintercept=8200/24, linetype="dashed", color = "purple") +
  geom_label(x=3, y=8200/24, label='T5', color="purple", fill='white',size=7 )

 ggsave(paste(plot_path,'/',name,'_load_w_tiers.png',sep=''), plot = last_plot(), device=png())
 dev.off()

}

plot_max_w_tier <- function(x,name,ylab,title) {
 data <- x
 data$Hour <- rownames(data)
 melted_data <- melt(data, id.var='Hour')
 names(melted_data)[names(melted_data)=="value"] <- "Load"
 names(melted_data)[names(melted_data)=="variable"] <- "Device"

 ggplot(melted_data, aes(fill=Device, y=Load, x=Hour)) + 
  geom_bar(position="stack", stat="identity") +   
  coord_cartesian(expand=FALSE) +
  theme_bw() +
  theme(text = element_text(size=10),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
  ggtitle(title) +
  ylab(ylab) +
  geom_hline(yintercept=3, linetype="dashed", color = "red") +
  geom_label(x=3, y=3, label='T1', color="red", fill='white',size=7 ) +
  geom_hline(yintercept=50, linetype="dashed", color = "yellow") +
  geom_label(x=3, y=50, label='T2', color="yellow", fill='white',size=7 ) +
  geom_hline(yintercept=200, linetype="dashed", color = "blue") +
  geom_label(x=3, y=200, label='T3', color="blue", fill='white',size=7 ) +
  geom_hline(yintercept=800, linetype="dashed", color = "green") +
  geom_label(x=3, y=800, label='T4', color="green", fill='white',size=7 ) +
  geom_hline(yintercept=2000, linetype="dashed", color = "purple") +
  geom_label(x=3, y=2000, label='T5', color="purple", fill='white',size=7 )

 ggsave(paste(plot_path,'/',name,'_load_w_tiers.png',sep=''), plot = last_plot(), device=png())
}

plot_load <- function(x,name,ylab,title) {
	load_data <- x
	png(paste(plot_path,'/',name,'_load.png',sep=''))
	par(mar=c(10.1, 4.1, 4.1, 10.1), xpd=TRUE)
	barplot(as.matrix(t(load_data)),col=rainbow(length(load_data[1,])),ylab=ylab,las=2,main=title)
	legend("topright",inset=c(-0.2,0),legend=rev(colnames(load_data)),
		fill=rev(rainbow(length(load_data[1,]))))
	dev.off()
}


# DO STUFF

setwd(input_path)




# LOOP OVER ALL FILES

 files <- list.files(path=input_path, pattern="Load_By_Device_*", full.names=TRUE, recursive=FALSE) # List of disagg files containing usage data. These were downloaded from the Meshpower server based upon an SQL query to find mac_address names which have at some point been associated with Gitaraga
 files <- Filter(function(x) any(grepl(".csv", x)), files) # ronly include csvs - to save accidetally rying to process an excel file
 files <- Filter(function(x) !any(grepl("Tier1", x)), files) # remove Tier 1 - because this has only one nonero column, this causes problems in making clean mean data

summed_overlaps<-c()
average_loads<-c()
peak_loads<-c()
diversity_factors<-c()

for (file in files){

	if(endsWith(file,'.csv')==FALSE)
	{
		print('Skipping non csv file')
		next
	}

	print(paste('Running for file ',file,sep=''))

	# GET AND RESHAPE DATA

	# Get names of input files
	input_file<-file # Appliances input file
	d_input_file<- gsub(file, pattern = "Load_By_Device_", replacement = "Res_Disagg_")  
	file_no_path<-gsub(input_file, pattern = paste(input_path,'/',sep=''), replacement = "") 
	name_abbrev<-gsub(file_no_path, pattern = ".csv", replacement = "") 
	name_abbrev<-gsub(name_abbrev, pattern = "Load_By_Device_", replacement = "") 


	# Mess to deal with messy import files
	headers = read.csv(input_file, skip = 3, header = F, nrows = 1, as.is = T) 
	df = read.csv(input_file, skip = 6, header = F)
	colnames(df)= headers
	df <- head(df, -2)[1:33] 

	d_headers = read.csv(d_input_file, skip = 3, header = F, nrows = 1, as.is = T)
	d_df = read.csv(d_input_file, skip = 6, header = F)
	colnames(d_df)= d_headers

	# Combine appliance data with heating, cooling, and lighting columns of 'disaggregated' input file
	df <- cbind(df,Cooling=d_df[['Electricity used by cooling system']],Heating=d_df[['Electricity used by heating system']],Lighting=d_df[['Lighting demand']]) 
	#df <- head(df, n=4320L) # Temporary measure to reduce size for debugging

	# Rename phone -> mobile

	df  <- rename(df, c("Cordless telephone"="Mobile Phone"))

	# Rename/drop heating & cooling by tier as appropriate

	if(grepl('Tier1', file, fixed = TRUE) == TRUE)
	{
		df[['Cooling']] <- NULL
		df[['Heating']] <- NULL
	}
	else if(grepl('Tier2', file, fixed = TRUE) == TRUE)
	{
		df  <- rename(df, c("Cooling"="Fan"))
		df[['Heating']] <- NULL
	}
	else if(grepl('Tier3', file, fixed = TRUE) == TRUE)
	{
		df  <- rename(df, c("Cooling"="Air Cooler"))
		df[['Heating']] <- NULL
	}
	elseif(grepl('Tier4', file, fixed = TRUE) == TRUE)
	{
		df  <- rename(df, c("Cooling"="Air Cooler"))
		df[['Heating']] <- NULL
	}
	else if(grepl('Tier5', file, fixed = TRUE) == TRUE)
	{
		df  <- rename(df, c("Cooling"="Air Conditioning"))
		df  <- rename(df, c("Heating"="Electric Water Heater"))
	}
	 
	# Get R to recognise time column as times
	#df['Time'] <- lapply(df['Time'], strptime, "%I:%M:%S %p")
	df['Time'] <- lapply(df['Time'], strptime, "%H:%M:%S")

	# Export Loads by Tier
	write.table(df, paste(output_path,'All_loads_',name_abbrev,'.csv',sep=''), sep=",", row.names=FALSE)
}

for file in files 
{

	# Split dataframe into a list of dataframes representing each dwelling, indexed by the Dwelling index
	customer_numbers <- unique(df[["Dwelling index"]])
	df_by_customer <- lapply(customer_numbers, split_load_CREST, data=df )

	# Convert data into hourly (mean load across each minute in each hour)
	hourly_data <- lapply(df_by_customer, convert_to_hourly)

	# Convert data into hourly (maximum load across each minute in each hour)
	hourly_peak_data <- lapply(df_by_customer, convert_to_hourly_peak)

	# Combine all dwellings again (bit of redundancy here)
	total_hourly_df=do.call(rbind,hourly_data)
	total_hourly_peak_df=do.call(rbind,hourly_peak_data)

	# Get mean load across households by hour
	mean_hourly=aggregate(total_hourly_df[-1][-1],by=list(total_hourly_df$Time), mean)
	names(mean_hourly)[1] <- "Hour"
	rownames(mean_hourly) <- mean_hourly$Hour
	mean_hourly <- mean_hourly[-1]

	# Get max load across households by hour
	max_hourly=aggregate(total_hourly_peak_df[-1][-1],by=list(total_hourly_df$Time), max)
	names(max_hourly)[1] <- "Hour"
	rownames(max_hourly) <- max_hourly$Hour
	max_hourly <- max_hourly[-1]

	# PLOT STACKED BARCHARTS OF HOURLY MEAN AND PEAK LOAD BY DEVICE AS IT EMERGES FROM CREST MODEL

	# Remove columns with no nonzero values and plot MEAN load (as it emerges from the CREST model)
	mean_hourly_clean<-mean_hourly[, colSums(mean_hourly != 0) > 0]
	# plot_load(mean_hourly_clean,name=paste('mean_hourly_tier_',tier,sep=''),ylab='Mean Consumption (kWh)',title=paste('Mean Load Tier ',tier,sep=''))
	# plot_load_w_tier(mean_hourly_clean,name=paste('mean_hourly_',name_abbrev,sep=''),ylab='Mean Consumption (Wh)',title=paste('Mean Load ',name_abbrev,sep=''))

	# Remove columns with no nonzero values and plot MAX load (as it emerges from the CREST model)
	max_hourly_clean<-max_hourly[, colSums(max_hourly != 0) > 0]
	# plot_load(max_hourly_clean,name=paste('max_hourly_tier_',tier,sep=''),ylab='Max Load (W)',title=paste('Max Load Tier ',tier,sep=''))
	#plot_max_w_tier(max_hourly_clean,name=paste('max_hourly_tier_',tier,sep=''),ylab='Max Load (W)',title=paste('Max Load Tier ',tier,sep=''))

	# SCALE LOAD IN LINE WITH EST LOADS - PLOT STACKED BARCHARTS OF SCALED HOURLY MEAN AND PEAK LOAD BY DEVICE

	# Scale devices in line with EST median and then plot MEAN (now same as above as scaling done earlier)
	mean_hourly_scaled <- mean_hourly

	# Remove columns with no nonzero values and plot mean load (with revised load values)
	mean_hourly_scaled_clean<-mean_hourly_scaled[, colSums(mean_hourly_scaled != 0) > 0]
	plot_load_w_tier(mean_hourly_scaled_clean,name=paste('mean_hourly_scaled_',name_abbrev,sep=''),ylab='Mean Consumption (Wh)',title=paste('Mean Load ',name_abbrev,sep=''))

	# Scale devices in line with EST median and then plot MAX
	max_hourly_scaled <- max_hourly
	max_hourly_scaled_clean<-max_hourly_scaled[, colSums(max_hourly_scaled != 0) > 0]
	# Remove columns with no nonzero values and plot mean load (with revised load values)
	#plot_max_w_tier(max_hourly_scaled_clean,name=paste('max_hourly_scaled_tier',tier,sep=''),ylab='Max Load (W)',title=paste('Max Load Tier ',tier,sep=''))

	# ANALYSIS WITH PEAK/TOTAL LOAD

	total_load_by_minute_by_customer<-lapply(df_by_customer, sum_load)
	total_load_hourly_by_customer <- lapply(total_load_by_minute_by_customer, convert_to_hourly)
	combined_total_load_minute_data <- do.call(rbind,total_load_by_minute_by_customer)
	combined_total_load_hourly_data <- do.call(rbind,total_load_hourly_by_customer)

	# MEAN LOAD (overall)

	mean_load_by_minute<- aggregate(combined_total_load_minute_data[-2],by=list(combined_total_load_minute_data$Time), mean)

	average_load<-mean(mean_load_by_minute[['Total Load']])*length(customer_numbers)
	average_loads<-append(average_loads,average_load)

	# MEAN TOTAL LOAD PER HOUR
	mean_hourly_total<- aggregate(combined_total_load_hourly_data[-2],by=list(combined_total_load_hourly_data$Time), mean)
	mean_hourly_total <- mean_hourly_total[-2]
	mean_hourly_total[[1]] <- as.numeric(mean_hourly_total[[1]])
	names(mean_hourly_total)[1] <- "hour"
	names(mean_hourly_total)[2] <- "mean_wh"



	mean_norm<-mean_hourly_total$mean_wh/sum(mean_hourly_total$mean_wh)
	mean_norm<-as.data.frame(cbind(mean_hourly_total$hour,mean_norm))
	names(mean_norm)<-c('hour','mean_normalised')

	# GET DIVERSIY FACTOR
	peak_load_by_customer<-unlist(lapply(total_load_by_minute_by_customer,function(total_load_by_minute){
		return(max(total_load_by_minute[['Total Load']]))
	}))

	sum_individual_peak_load<-sum(peak_load_by_customer)

	max_aggregate_load<-max(mean_load_by_minute[['Total Load']])*length(customer_numbers)

	peak_loads<-append(peak_loads,max_aggregate_load)

	div_factor<-sum_individual_peak_load/max_aggregate_load

	diversity_factors<-append(diversity_factors,div_factor)


	# get overlap w PV data by hour for plotting purposes
	hourly_pv_overlap<-mean_norm
	hourly_pv_overlap$pv=PV_mean_normalised$electricity
	hourly_pv_overlap$overlap=pmin(hourly_pv_overlap$pv,hourly_pv_overlap$mean_normalised)

	summed_overlap<-sum(hourly_pv_overlap[['overlap']])

	summed_overlaps<-append(summed_overlaps,summed_overlap)


	# Barplot - PV overlap
	# ggplot(hourly_pv_overlap, aes(x=hour, y=overlap)) +
	# coord_cartesian(expand=FALSE) +
	#   theme_bw() +
	#   theme(text = element_text(size=10),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
	#   geom_bar(stat = "identity") +
	#   scale_y_continuous(limits = c(0, 0.25)) +
	#   geom_line(aes(x = as.numeric(hour), y = pv, color='PV')) +
	#   geom_line(aes(x = as.numeric(hour), y = mean_normalised,color='Load')) +
	#   ggtitle(paste(gsub(name_abbrev,pattern='MtAbu_20_6_',replacement=''),' - PV overlap',sep='')) +
	#   ylab('Normalised load/generation') + 
	#   xlab('Hour') +
 # 	  scale_colour_manual("", values=c("PV" = "blue", "Load" = "red"))


	#   ggsave(paste(plot_path,'/','PV_overlap_',name_abbrev,'.png',sep=''), plot = last_plot(), device=png())

	#   dev.off()


	# PEAK TOTAL LOAD PER CUSTOMER
	total_load_hourly_peak_data <- lapply(total_load_by_minute_by_customer, convert_to_hourly_peak)
	combined_total_load_hourly_peak_data <- do.call(rbind,total_load_hourly_peak_data)



	# MEAN
	mean_hourly_peak <- aggregate(combined_total_load_hourly_peak_data[-2],by=list(combined_total_load_hourly_peak_data$Time), mean)
	mean_hourly_peak <- mean_hourly_peak[-2]
	mean_hourly_peak[[1]] <- as.numeric(mean_hourly_peak[[1]])
	names(mean_hourly_peak)[1] <- "Hour"
	names(mean_hourly_peak)[2] <- "PeakLoad_W"

	# MEDIAN
	median_hourly_peak <- aggregate(combined_total_load_hourly_peak_data[-2],by=list(combined_total_load_hourly_peak_data$Time), median)
	median_hourly_peak <- median_hourly_peak[-2]
	median_hourly_peak[[1]] <- as.numeric(median_hourly_peak[[1]])
	names(median_hourly_peak)[1] <- "Hour"
	names(median_hourly_peak)[2] <- "PeakLoad_W"

	# SD
	hourly_peak_sd <- aggregate(combined_total_load_hourly_peak_data[-2],by=list(combined_total_load_hourly_peak_data$Time), sd)
	names(hourly_peak_sd)[1] <- "Hour"
	names(hourly_peak_sd)[3] <- "SD"

	mean_hourly_peak$SD<-hourly_peak_sd[[3]]
	median_hourly_peak$SD<-hourly_peak_sd[[3]]

	rownames(mean_hourly_peak) <- mean_hourly_peak$Hour
	rownames(median_hourly_peak) <- mean_hourly_peak$Hour

	# # Barplot - mean
	# ggplot(mean_hourly_peak, aes(x=Hour, y=PeakLoad_W)) + 
	#   coord_cartesian(expand=FALSE) +
	#   theme_bw() +
	#   theme(text = element_text(size=10),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
	#   geom_bar(stat = "identity") +   geom_errorbar(aes(x=Hour, ymin=PeakLoad_W-SD, ymax=PeakLoad_W+SD), width=.2, position=position_dodge(.9)) +
	#   ggtitle(paste('Mean Peak Load by Hour, ', name_abbrev, sep='')) +
	#   ylab('Peak Load (W)') +
	#   geom_hline(yintercept=3, linetype="dashed", color = "red") +
	#   geom_label(x=3, y=3, label='T1', color="red", fill='white',size=7 ) +
	#   geom_hline(yintercept=50, linetype="dashed", color = "yellow") +
	#   geom_label(x=3, y=50, label='T2', color="yellow", fill='white',size=7 ) +
	#   geom_hline(yintercept=200, linetype="dashed", color = "blue") +
	#   geom_label(x=3, y=200, label='T3', color="blue", fill='white',size=7 ) +
	#   geom_hline(yintercept=800, linetype="dashed", color = "green") +
	#   geom_label(x=3, y=800, label='T4', color="green", fill='white',size=7 ) +
	#   geom_hline(yintercept=2000, linetype="dashed", color = "purple") +
	#   geom_label(x=3, y=2000, label='T5', color="purple", fill='white',size=7 )

	#   ggsave(paste(plot_path,'/','Mean_Peak_Load_',name_abbrev,'_w_tiers_sd.png',sep=''), plot = last_plot(), device=png())
 #   	dev.off()


	# # Barplot - median
	# ggplot(median_hourly_peak, aes(x=Hour, y=PeakLoad_W)) + 
 #  coord_cartesian(expand=FALSE) +
 #  theme_bw() +
 #  theme(text = element_text(size=10),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
 #  geom_bar(stat = "identity") +   geom_errorbar(aes(x=Hour, ymin=PeakLoad_W-SD, ymax=PeakLoad_W+SD), width=.2, position=position_dodge(.9)) +
 #  ggtitle(paste('Median Peak Load by Hour, ', name_abbrev, sep='')) +
 #  ylab('Peak Load (W)') +
 #  geom_hline(yintercept=3, linetype="dashed", color = "red") +
 #  geom_label(x=3, y=3, label='T1', color="red", fill='white',size=7 ) +
 #  geom_hline(yintercept=50, linetype="dashed", color = "yellow") +
 #  geom_label(x=3, y=50, label='T2', color="yellow", fill='white',size=7 ) +
 #  geom_hline(yintercept=200, linetype="dashed", color = "blue") +
 #  geom_label(x=3, y=200, label='T3', color="blue", fill='white',size=7 ) +
 #  geom_hline(yintercept=800, linetype="dashed", color = "green") +
 #  geom_label(x=3, y=800, label='T4', color="green", fill='white',size=7 ) +
 #  geom_hline(yintercept=2000, linetype="dashed", color = "purple") +
 #  geom_label(x=3, y=2000, label='T5', color="purple", fill='white',size=7 )


 #  ggsave(paste(plot_path,'/',name_abbrev,'_w_tiers_sd.png',sep=''), plot = last_plot(), device=png())
 #  dev.off()

}


tiers<-c('Tier 2', 'Tier 3', 'Tier 4', 'Tier 5')

names(summed_overlaps)<-tiers

overlaps_df<-as.data.frame(cbind(tiers,summed_overlaps))
colnames(overlaps_df)<-c('Tier','overlap')

overlaps_df$overlap<-as.numeric(as.character(overlaps_df$overlap))

dotchart(overlaps_df$overlap,labels=row.names(overlaps_df),cex=.7,
	main="Overlap with PV by tier",
	xlab="Overlap")

dev.off()

# Div Factors

names(diversity_factors)<-tiers

diversity_factors_df<-as.data.frame(cbind(tiers,diversity_factors))
colnames(diversity_factors_df)<-c('Tier','diversity_factor')

diversity_factors_df$diversity_factor<-as.numeric(as.character(diversity_factors_df$diversity_factor))

dotchart(diversity_factors_df$diversity_factor,labels=row.names(diversity_factors_df),cex=.7,
	main="Diversity Factor by tier",
	xlab="Diversity Factor")

dev.off()

# Load factors

names(load_factors)<-tiers

load_factors_df<-as.data.frame(cbind(tiers,load_factors))
colnames(load_factors_df)<-c('Tier','load_factor')

load_factors_df$load_factor<-as.numeric(as.character(load_factors_df$load_factor))

dotchart(load_factors_df$load_factor,labels=row.names(load_factors_df),cex=.7,
	main="Load Factor by tier",
	xlab="Load Factor")

dev.off()



names(average_loads)<-tiers

average_loads_df<-as.data.frame(cbind(tiers,average_loads))
colnames(average_loads_df)<-c('Tier','average_load')

average_loads_df$average_load<-as.numeric(as.character(average_loads_df$average_load))

dotchart(average_loads_df$average_load,labels=row.names(average_loads_df),cex=.7,
	main="Average Load by tier",
	xlab="Average Load")

dev.off()

