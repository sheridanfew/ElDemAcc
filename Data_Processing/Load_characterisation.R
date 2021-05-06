# Script to characterise loads from Meshpower, Gram Oorja, and CREST model after they have been processed to a consistent format
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
library(viridis)


### PATH DEFINITION

root_path <- '/Users/Shez/Google\ Drive/Grantham/ElDemAcc/'

### INPUT PATHS

# Load from various sources to analyse:

# CREST Demand Data
CREST_input_path <- paste(root_path,'CREST/Processed_Load/',sep='')
# Full Demand Data
CREST_full_data_input_path <- paste(CREST_input_path,'Full_Demand_Data/',sep='')
# Annual_Hourly_Load_By_Village_Tier
CREST_Annual_Hourly_Load_By_Village_Tier_input_path <- paste(CREST_input_path,'Annual_Hourly_Load_By_Village_Tier/',sep='')
# Full Demand Annual_Hourly_Load_By_Village_Tier_Dwelling
CREST_Annual_Hourly_Load_By_Village_Tier_Dwelling_input_path <- paste(CREST_input_path,'Annual_Hourly_Load_By_Village_Tier_Dwelling/',sep='')

## Meshpower Demand Data
Meshpower_input_path <- paste(root_path,'Meshpower/Processed_Load/',sep='')
Meshpower_Annual_Hourly_Load_By_Dom_Type_input_path <- paste(Meshpower_input_path,'Annual_Hourly_Load_By_Dom_Type/',sep='')
Meshpower_Annual_Hourly_Load_By_Nondom_Type_input_path <- paste(Meshpower_input_path,'Annual_Hourly_Load_By_Nondom_Type/',sep='')
Meshpower_Annual_Hourly_Load_By_Dom_User_input_path <- paste(Meshpower_input_path,'Annual_Hourly_Load_By_Dom_User/',sep='')
Meshpower_Annual_Hourly_Load_By_Nondom_User_input_path <- paste(Meshpower_input_path,'Annual_Hourly_Load_By_Nondom_User/',sep='')

# Gram Oorja Demand Data
GramOorja_input_path <- paste(root_path,'GramOorja/Processed_Load/',sep='')
GramOorja_Annual_Hourly_Load_By_Source_input_path <- paste(GramOorja_input_path,'Annual_Hourly_Load_By_Source/',sep='')

# Climate data
Climate_input_path <- paste(root_path,'Climate_data/',sep='')

# OUTPUT PATHS

output_path <- paste(root_path,'Outputs/',sep='')
plot_path <- paste(root_path,'Plots/',sep='')

### RANGES TO CONSIDER

 villages<-c('Bhinjpur','Gitaraga')
 tiers<-c(1,2,3,4,5)
 daytypes<-c('wd','we')
 months<-c(6,7)

### PLOTTING PARAMETERS

height=5
width=8


### FUNCTIONS

### DO STUFF

### 1. IMPORT DATA

# Characterised Loads

Gram_Oorja_Load_Metrics<-read.csv(paste(GramOorja_input_path,'Load_Metrics_by_device.csv',sep=''))

Meshpower_Nondom_Load_Metrics<-read.csv(paste(Meshpower_input_path,'Load_Metrics_nondom.csv',sep=''))

Meshpower_Dom_Load_Metrics<-read.csv(paste(Meshpower_input_path,'Load_Metrics_dom.csv',sep=''))


PV_data <- lapply(villages,function(village){
	input_file<-paste(Climate_input_path,village,"_climate.csv",sep='')
	df<-read.csv(input_file,skip = 3, header = T)
	return(df)
})
names(PV_data)<-villages

### 2. COMBINE DATA

# Make names and case similar across data

names(Meshpower_Dom_Load_Metrics)<-names(Meshpower_Nondom_Load_Metrics)

Meshpower_Dom_Load_Metrics$end_user<-tools::toTitleCase(gsub(Meshpower_Dom_Load_Metrics$end_user, pattern = "_", replacement = " "))

Meshpower_Nondom_Load_Metrics$end_user<-tools::toTitleCase(gsub(Meshpower_Nondom_Load_Metrics$end_user, pattern = "_", replacement = " "))

Gram_Oorja_Load_Metrics$end_user<-tools::toTitleCase(gsub(Gram_Oorja_Load_Metrics$end_user, pattern = "_", replacement = " "))

# Remove fans from gram oorja data (not commercial)

Gram_Oorja_Load_Metrics<-Gram_Oorja_Load_Metrics[which(Gram_Oorja_Load_Metrics$end_user!="Fan"),]

# Combine data, 

Combined_load_metrics<-rbind(Meshpower_Dom_Load_Metrics,Meshpower_Nondom_Load_Metrics,Gram_Oorja_Load_Metrics)

write.csv(Combined_load_metrics,paste(output_path,'Load_Metrics_Combined.csv',sep=''), row.names=FALSE)

# convert NAs to 0s for plotting

Combined_load_metrics[is.na(Combined_load_metrics)] = 0

Combined_load_metrics$end_user<-factor(Combined_load_metrics$end_user,levels=unique(Combined_load_metrics$end_user))


### 2. DEFINE PALLETTE & PLOT CHARACTERISED LOADS

# Define pallettes

myColors<-c('gray',plasma(nrow(Meshpower_Nondom_Load_Metrics)),brewer.pal(nrow(Gram_Oorja_Load_Metrics),"Greens"))
	names(myColors) <- Combined_load_metrics$end_user

# Plot Daily Energy

p<-ggplot(data=Combined_load_metrics, aes(x= end_user, y= daily_energy, fill=end_user)) +
   geom_bar(stat="identity") + 
   #coord_cartesian(expand=FALSE) +
   theme_bw() +
   theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
   theme(axis.text.x = element_text(angle=90, hjust=1),axis.title.x=element_blank()) +
   scale_fill_manual(name = "end_user",values = myColors) +
   ylab('Mean Daily Energy (Wh)') +
   #scale_y_continuous(limits = c(0, 15000)) +
   geom_errorbar(aes(ymin= daily_energy-daily_energy_stdev_across_users, ymax= daily_energy + daily_energy_stdev_across_users), 
   				 width=.2, position=position_dodge(.9)) +
   geom_errorbar(aes(ymin= daily_energy-daily_energy_stdev_across_days_and_users, ymax= daily_energy+daily_energy_stdev_across_days_and_users), 
   				 width=.2, position=position_dodge(.9),linetype='dotted')

 ggsave(paste(plot_path,'Load_Characteristics/daily_energy.png',sep=''),plot=p,width=width,height=height)


# Plot Daily Energy (with y limits)

p<-ggplot(data=Combined_load_metrics, aes(x= end_user, y= daily_energy, fill=end_user)) +
   geom_bar(stat="identity") + 
   coord_cartesian(ylim=c(0,800)) +
   theme_bw() +
   theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
   theme(axis.text.x = element_text(angle=90, hjust=1),axis.title.x=element_blank()) +
   scale_fill_manual(name = "end_user",values = myColors) +
   ylab('Mean Daily Energy (Wh)') +
   geom_errorbar(aes(ymin=daily_energy-daily_energy_stdev_across_users, ymax= daily_energy + daily_energy_stdev_across_users), 
   				 width=.2, position=position_dodge(.9)) +
   geom_errorbar(aes(ymin= daily_energy-daily_energy_stdev_across_days_and_users, ymax= daily_energy+daily_energy_stdev_across_days_and_users), 
   				 width=.2, position=position_dodge(.9),linetype='dotted')

 ggsave(paste(plot_path,'Load_Characteristics/daily_energy_ylim.png',sep=''),plot=p,width=width,height=height)


# Plot Peak Load

p<-ggplot(data=Combined_load_metrics, aes(x= end_user, y= peak_load, fill=end_user)) +
   geom_bar(stat="identity") + 
   #coord_cartesian(expand=FALSE) +
   theme_bw() +
   theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
   theme(axis.text.x = element_text(angle=90, hjust=1),axis.title.x=element_blank()) +
   scale_fill_manual(name = "end_user",values = myColors) +
   ylab('Peak Load (W)') +

   #scale_y_continuous(limits = c(0, 15000)) +
   geom_errorbar(aes(ymin= peak_load-peak_load_stdev_across_users, ymax= peak_load+peak_load_stdev_across_users), 
   				 width=.2, position=position_dodge(.9))

 ggsave(paste(plot_path,'Load_Characteristics/peak_load.png',sep=''),plot=p,width=width,height=height)



# Plot Peak Load (with y limits)

p<-ggplot(data=Combined_load_metrics, aes(x= end_user, y= peak_load, fill=end_user)) +
   geom_bar(stat="identity") + 
   coord_cartesian(ylim=c(0,400)) +
   theme_bw() +
   theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
   theme(axis.text.x = element_text(angle=90, hjust=1),axis.title.x=element_blank()) +
   scale_fill_manual(name = "end_user",values = myColors) +
   ylab('Peak Load (W)') +
   geom_errorbar(aes(ymin= peak_load-peak_load_stdev_across_users, ymax= peak_load+peak_load_stdev_across_users), 
   				 width=.2, position=position_dodge(.9))

 ggsave(paste(plot_path,'Load_Characteristics/peak_load_ylim.png',sep=''),plot=p,width=width,height=height)


# Plot load_factor

p<-ggplot(data=Combined_load_metrics, aes(x= end_user, y= load_factor, fill=end_user)) +
   geom_bar(stat="identity") + 
   #coord_cartesian(expand=FALSE) +
   theme_bw() +
   theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
   theme(axis.text.x = element_text(angle=90, hjust=1),axis.title.x=element_blank()) +
   scale_fill_manual(name = "end_user",values = myColors) +
   ylab('Load Factor') +

   #scale_y_continuous(limits = c(0, 15000)) +
   geom_errorbar(aes(ymin= load_factor-load_factor_stdev_across_users, ymax= load_factor+load_factor_stdev_across_users), 
   				 width=.2, position=position_dodge(.9))

 ggsave(paste(plot_path,'Load_Characteristics/load_factor.png',sep=''),plot=p,width=width,height=height)


# Plot pv_overlap

p<-ggplot(data=Combined_load_metrics, aes(x= end_user, y= pv_overlap, fill=end_user)) +
   geom_bar(stat="identity") + 
   coord_cartesian(ylim=c(0,1)) +
   theme_bw() +
   theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
   theme(axis.text.x = element_text(angle=90, hjust=1),axis.title.x=element_blank()) +
   scale_fill_manual(name = "end_user",values = myColors) +
   ylab('PV Overlap') +

   #scale_y_continuous(limits = c(0, 15000)) +
   geom_errorbar(aes(ymin= pv_overlap-pv_overlap_stdev_across_users, ymax= pv_overlap+pv_overlap_stdev_across_users), 
   				 width=.2, position=position_dodge(.9))

 ggsave(paste(plot_path,'Load_Characteristics/pv_overlap.png',sep=''),plot=p,width=width,height=height)


# Plot seasonal_consistency

p<-ggplot(data=Combined_load_metrics, aes(x= end_user, y= seasonal_consistency, fill=end_user)) +
   geom_bar(stat="identity") + 
   coord_cartesian(ylim=c(0,1)) +
   theme_bw() +
   theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
   theme(axis.text.x = element_text(angle=90, hjust=1),axis.title.x=element_blank()) +
   scale_fill_manual(name = "end_user",values = myColors) +
   ylab('Seasonal Consistency') +

   #scale_y_continuous(limits = c(0, 15000)) +
   geom_errorbar(aes(ymin= seasonal_consistency-seasonal_consistency_stdev_across_users, ymax= seasonal_consistency+seasonal_consistency_stdev_across_users), 
   				 width=.2, position=position_dodge(.9))

 ggsave(paste(plot_path,'Load_Characteristics/seasonal_consistency.png',sep=''),plot=p,width=width,height=height)



### 4. PLOT CREST LOAD DIFFERENTIATED BY DEVICE



