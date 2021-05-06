# Script to process climate data for analysis in later scripts
# Sheridan Few, Oct 2020
# See also readme file

### PACKAGES

library(data.table)
library(zoo)
#library(plyr)
library(dplyr)
library(lubridate)
library(timeDate)
library(ggplot2)

### SETTINGS

quartz.options(width=8, height=4)

### PATH DEFINITION

root_path <- '/Users/Shez/Google\ Drive/Grantham/ElDemAcc/'
input_path <- paste(root_path,'GramOorja/Input_data/',sep='')
output_path <- paste(root_path,'Outputs/',sep='')
plot_path <- paste(root_path,'Plots/',sep='')


climate_data_input_path <- paste(root_path,'Climate_data/',sep='')

communities <- c('Gitaraga','Bhinjpur')
climates<-c('Tropical Savanna (TSav)','Humid Subtropical (HSub)')


### FUNCTIONS

# DO STUFF

### 1. IMPORT AND CLEAN DATA

# Temp data:
Temperature_data_by_month_by_climate<-lapply(communities,function(community){
	climate_data_input<-paste(climate_data_input_path,community,'_climate.csv',sep='')
	climate_data<-read.csv(climate_data_input,skip = 3, header = T)
	climate_data$month<-month(climate_data$time)
	temperature_min_by_month<-aggregate(temperature ~ month, climate_data, min)
	temperature_mean_by_month<-aggregate(temperature ~ month, climate_data, mean)
	temperature_max_by_month<-aggregate(temperature ~ month, climate_data, max)
	temp_stats_by_month<-cbind(community,temperature_min_by_month[1],min=temperature_min_by_month[[2]],mean=temperature_mean_by_month[[2]],max=temperature_max_by_month[[2]])
	write.csv(temp_stats_by_month, paste(output_path,'Temperature_Monthly_Stats_',community,'.csv',sep=''), row.names=FALSE)

	# Clunky lines I added to plot results for two communities separately 
	# names(temp_stats_by_month) <- gsub(names(temp_stats_by_month), pattern = "community", replacement = "Climate")  
	# temp_stats_by_month$Climate <- gsub(temp_stats_by_month$Climate, pattern = "Gitaraga", replacement = "Temperate")  
	# temp_stats_by_month$Climate <- gsub(temp_stats_by_month$Climate, pattern = "Bhinjpur", replacement = "Extreme")  

	return(temp_stats_by_month)
})

names(Temperature_data_by_month_by_climate)<-climates


monthly_temp_data_combined<-do.call(rbind, Temperature_data_by_month_by_climate)

names(monthly_temp_data_combined) <- gsub(names(monthly_temp_data_combined), pattern = "community", replacement = "Climate")  

monthly_temp_data_combined$Climate <- gsub(monthly_temp_data_combined$Climate, pattern = "Gitaraga", replacement = "Tropical Savanna (TSav)")  

monthly_temp_data_combined$Climate <- gsub(monthly_temp_data_combined$Climate, pattern = "Bhinjpur", replacement = "Humid Subtropical (HSub)")  


# PV output data:

PV_data_by_month_by_climate<-lapply(communities,function(community){
	climate_data_input<-paste(climate_data_input_path,community,'_climate.csv',sep='')
	climate_data<-read.csv(climate_data_input,skip = 3, header = T)
	climate_data$month<-month(climate_data$time)
	PV_min_by_month<-aggregate(electricity ~ month, climate_data, min)
	PV_mean_by_month<-aggregate(electricity ~ month, climate_data, mean)
	PV_max_by_month<-aggregate(electricity ~ month, climate_data, max)
	PV_stats_by_month<-cbind(community,PV_min_by_month[1],min=PV_min_by_month[[2]],mean=PV_mean_by_month[[2]],max=PV_max_by_month[[2]])
	write.csv(PV_stats_by_month, paste(output_path,'PV_Monthly_Stats_',community,'.csv',sep=''), row.names=FALSE)

	# Clunky lines I added to plot results for two communities separately 
	# names(temp_stats_by_month) <- gsub(names(temp_stats_by_month), pattern = "community", replacement = "Climate")  
	# temp_stats_by_month$Climate <- gsub(temp_stats_by_month$Climate, pattern = "Gitaraga", replacement = "Temperate")  
	# temp_stats_by_month$Climate <- gsub(temp_stats_by_month$Climate, pattern = "Bhinjpur", replacement = "Extreme")  

	return(PV_stats_by_month)
})

names(PV_data_by_month_by_climate)<-climates


monthly_PV_data_combined<-do.call(rbind, PV_data_by_month_by_climate)

names(monthly_PV_data_combined) <- gsub(names(monthly_PV_data_combined), pattern = "community", replacement = "Climate")  

monthly_PV_data_combined$Climate <- gsub(monthly_PV_data_combined$Climate, pattern = "Gitaraga", replacement = "Tropical Savanna (TSav)")  

monthly_PV_data_combined$Climate <- gsub(monthly_PV_data_combined$Climate, pattern = "Bhinjpur", replacement = "Humid Subtropical (HSub)")  

# irradiance output data:

irradiance_data_by_month_by_climate<-lapply(communities,function(community){
	climate_data_input<-paste(climate_data_input_path,community,'_climate.csv',sep='')
	climate_data<-read.csv(climate_data_input,skip = 3, header = T)
	climate_data$irradiance<-climate_data$irradiance_direct + climate_data$irradiance_diffuse
	climate_data$irradiance<-climate_data$irradiance*1000
	climate_data$month<-month(climate_data$time)
	irradiance_min_by_month<-aggregate(irradiance ~ month, climate_data, min)
	irradiance_mean_by_month<-aggregate(irradiance ~ month, climate_data, mean)
	irradiance_max_by_month<-aggregate(irradiance ~ month, climate_data, max)
	irradiance_stats_by_month<-cbind(community,irradiance_min_by_month[1],min=irradiance_min_by_month[[2]],mean=irradiance_mean_by_month[[2]],max=irradiance_max_by_month[[2]])
	write.csv(irradiance_stats_by_month, paste(output_path,'irradiance_Monthly_Stats_',community,'.csv',sep=''), row.names=FALSE)

	# Clunky lines I added to plot results for two communities separately 
	# names(temp_stats_by_month) <- gsub(names(temp_stats_by_month), pattern = "community", replacement = "Climate")  
	# temp_stats_by_month$Climate <- gsub(temp_stats_by_month$Climate, pattern = "Gitaraga", replacement = "Temperate")  
	# temp_stats_by_month$Climate <- gsub(temp_stats_by_month$Climate, pattern = "Bhinjpur", replacement = "Extreme")  

	return(irradiance_stats_by_month)
})

names(irradiance_data_by_month_by_climate)<-climates


monthly_irradiance_data_combined<-do.call(rbind, irradiance_data_by_month_by_climate)

names(monthly_irradiance_data_combined) <- gsub(names(monthly_irradiance_data_combined), pattern = "community", replacement = "Climate")  

monthly_irradiance_data_combined$Climate <- gsub(monthly_irradiance_data_combined$Climate, pattern = "Gitaraga", replacement = "Tropical Savanna (TSav)")  

monthly_irradiance_data_combined$Climate <- gsub(monthly_irradiance_data_combined$Climate, pattern = "Bhinjpur", replacement = "Humid Subtropical (HSub)")  

# Rainfall data:
rainfall_data_by_month_by_climate<-lapply(communities,function(community){
	rainfall_data_input<-paste(climate_data_input_path,community,'_monthly_rainfall.csv',sep='')
	rainfall_data_by_month<-read.csv(rainfall_data_input, header = T)
	rainfall_data_by_month<-cbind(community,rainfall_data_by_month)
	return(rainfall_data_by_month)
})

names(rainfall_data_by_month_by_climate)<-climates


monthly_rainfall_data_combined<-do.call(rbind, rainfall_data_by_month_by_climate)

names(monthly_rainfall_data_combined) <- gsub(names(monthly_rainfall_data_combined), pattern = "community", replacement = "Climate")  

monthly_rainfall_data_combined$Climate <- gsub(monthly_rainfall_data_combined$Climate, pattern = "Gitaraga", replacement = "Tropical Savanna (TSav)")  

monthly_rainfall_data_combined$Climate <- gsub(monthly_rainfall_data_combined$Climate, pattern = "Bhinjpur", replacement = "Humid Subtropical (HSub)")  


p<-ggplot(data=monthly_temp_data_combined, aes(x=month, y=mean, colour=Climate)) + geom_point() + geom_line() + ylab("Temperature (°C)") + xlab('Month')
p<-p+coord_cartesian(expand=FALSE) + theme_bw() + theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) + scale_x_continuous(breaks = seq(1, 12, 1))
p<-p+geom_ribbon(aes(ymin=monthly_temp_data_combined$max, ymax=monthly_temp_data_combined$min), linetype=2, alpha=0.1)

ggsave(paste(plot_path,'Temp_data.png',sep=''),plot=p,width=8,height=4)

p<-ggplot(data=monthly_PV_data_combined, aes(x=month, y=mean, colour=Climate)) + geom_point() + geom_line() + ylab("PV Cap Factor") + xlab('Month') + ylim(c(0,0.3))
p<-p+coord_cartesian(expand=FALSE) + theme_bw() + theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) + scale_x_continuous(breaks = seq(1, 12, 1))

ggsave(paste(plot_path,'PV_data.png',sep=''),plot=p,width=8,height=4)


p<-ggplot(data=monthly_irradiance_data_combined, aes(x=month, y=mean, colour=Climate)) + geom_point() + geom_line() + ylab(expression(paste("Mean Irradiance (W/", m^2, ")"))) + xlab('Month') + ylim(c(0,300))
p<-p+coord_cartesian(expand=FALSE) + theme_bw() + theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) + scale_x_continuous(breaks = seq(1, 12, 1))

ggsave(paste(plot_path,'irradiance_data.png',sep=''),plot=p,width=8,height=4)


p<-ggplot(data=monthly_rainfall_data_combined, aes(x=Month, y=Rainfall, colour=Climate)) + geom_point() + geom_line() + ylab("Mean Rainfall (mm)") + xlab('Month') + ylim(c(0,360))
p<-p+coord_cartesian(expand=FALSE) + theme_bw() + theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) + scale_x_continuous(breaks = seq(1, 12, 1))

ggsave(paste(plot_path,'rainfall_data.png',sep=''),plot=p,width=8,height=4)



# Clunky lines I added to plot results for two communities separately - requires clunky commented out lines in definition of Temperature_data_by_month_by_climate

p<-ggplot(data=Temperature_data_by_month_by_climate[[1]], aes(x=month, y=mean, colour=Climate)) + geom_point() + geom_line() + ylab("Temperature (°C)") + xlab('Month')
p<-p+coord_cartesian(expand=FALSE) + theme_bw() + theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) + scale_x_continuous(breaks = seq(1, 12, 1))
p<-p+geom_ribbon(aes(ymin=Temperature_data_by_month_by_climate[[1]]$max, ymax=Temperature_data_by_month_by_climate[[1]]$min), linetype=2, alpha=0.1)


p<-ggplot(data=Temperature_data_by_month_by_climate[[2]], aes(x=month, y=mean, colour=Climate)) + geom_point() + geom_line() + ylab("Temperature (°C)") + xlab('Month')
p<-p+coord_cartesian(expand=FALSE) + theme_bw() + theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) + scale_x_continuous(breaks = seq(1, 12, 1))
p<-p+geom_ribbon(aes(ymin=Temperature_data_by_month_by_climate[[2]]$max, ymax=Temperature_data_by_month_by_climate[[2]]$min), linetype=2, alpha=0.1)

