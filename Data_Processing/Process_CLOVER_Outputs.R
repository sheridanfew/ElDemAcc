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

root_path <- '/Users/Shez/Google\ Drive/Grantham/ElDemAcc/'
input_path <- paste(root_path,'Outputs/',sep='')
output_path<-paste(root_path,'Outputs/',sep='')

# Climate data (used in determining PV overlap factor)
climate_data_input_path <- paste(root_path,'Climate_data/',sep='')

plot_path <- paste(root_path,'Plots/',sep='')

 # Variable range
  villages<-c('Bhinjpur','Gitaraga')
  climates<-c('HSub','TSav')
  accessibility_contexts<-c('Peri-urban','Rural')
  growth_scenarios<-c('None','Slow','Medium','Fast','Faster')
  # Extra scenarios run:
  # growth_scenarios<-c('None','Slow','SlowPlus','Medium','MedPlus','Fast','FastPlus','Faster')

 # Pallettes
  accessibility_colours<-c("#FEB24C",'#A1D99B')
  names(accessibility_colours)<-accessibility_contexts

  growth_colours<-brewer.pal(length(growth_scenarios),"Set1")
  names(growth_colours) <- growth_scenarios

  climate_colours<-brewer.pal(length(climates),"Set1")[1:length(climates)]
  names(climate_colours) <- climates

### FUNCTIONS

# DO STUFF

### 1. IMPORT DATA

CLOVER_results<-read.csv(paste(input_path,'CLOVER_run_info_summarised.csv',sep=''))

CLOVER_results_TSav<-CLOVER_results[which(CLOVER_results$Climate!='HSub'),]
CLOVER_results_HSub<-CLOVER_results[which(CLOVER_results$Climate!='TSav'),]

# Set growth scenarios as factor so they come out in the same order as listed in plots. Include only those growth scenarios listed above.
#CLOVER_results$Growth<-factor(CLOVER_results$Growth,levels=unique(CLOVER_results_TSav$Growth))
CLOVER_results$Growth<-factor(CLOVER_results$Growth,levels=growth_scenarios)
CLOVER_results<-CLOVER_results[complete.cases(CLOVER_results), ]

# Rename "Faster" sceario to "Fast"

iris$Species[iris$Species == 'virginica'] <- 'setosa'

### 2. PLOT DATA

# ALL SCENARIOS TOGETHER (only this set of figures is used in the paper)

#CLOVER_results<-rbind(CLOVER_results[1:2,],CLOVER_results)
# Add both climatic conditions for no growth scenario (these are identical as they include no climate related data)
CLOVER_results$Climate[1:2]<-'TSav'
#CLOVER_results$Climate[3:4]<-'HSub'

CLOVER_results$Climate_Accessibility<-paste(CLOVER_results$Climate,CLOVER_results$Accessibility, sep=' / ')
CLOVER_results$Climate_Accessibility<-factor(CLOVER_results$Climate_Accessibility,levels=unique(CLOVER_results$Climate_Accessibility))


climate_accessibility_colours<-c("#9ECAE1","#3182BD","#FC9272","#DE2D26")
names(climate_accessibility_colours) <- unique(CLOVER_results$Climate_Accessibility)

# Alternative colour scheme
# climate_accessibility_colours<-c("#FFEDA0","#A1D99B","#FEB24C","#31A354")
# names(climate_accessibility_colours) <- unique(CLOVER_results$Climate_Accessibility)

# AMENDED to include two identical sets of bars for no growth. Could remove the few lines below for a single bar for no growth again.

CLOVER_results<-rbind(CLOVER_results,CLOVER_results[1,],CLOVER_results[2,])
CLOVER_results[nrow(CLOVER_results),]$Climate<-'HSub'
CLOVER_results[nrow(CLOVER_results)-1,]$Climate<-'HSub'
CLOVER_results$Climate_Accessibility<-paste(CLOVER_results$Climate,CLOVER_results$Accessibility, sep=' / ')

p1<-ggplot(CLOVER_results, aes(fill=Climate_Accessibility, y=PV_y0_kW, x=Growth)) + 
	 ylab('PV size (kWp)') +
	 xlab('Domestic Demand Growth') +
     geom_bar(position="dodge", stat="identity") +
	 coord_cartesian(expand=FALSE) +
	 theme_bw() +
	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
	 scale_fill_manual(name = "Climate / Rurality",values = climate_accessibility_colours)

	 ggsave(paste(plot_path,'CLOVER_Results/Everything_PV_y0_kW.png',sep=''),plot=p1,width=8,height=4)

p2<-ggplot(CLOVER_results, aes(fill=Climate_Accessibility, y=StoPerPV_y0, x=Growth)) + 
	 ylab('Storage per PV (kWh/kWp)') +
	 xlab('Domestic Demand Growth') +
     geom_bar(position="dodge", stat="identity") +
	 coord_cartesian(expand=FALSE) +
	 theme_bw() +
	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
	 scale_fill_manual(name = "Climate / Rurality",values = climate_accessibility_colours)

	 ggsave(paste(plot_path,'CLOVER_Results/Everything_StoPerPV_y0.png',sep=''),plot=p2,width=8,height=4)

p3<-ggplot(CLOVER_results, aes(fill=Climate_Accessibility, y=LCUE, x=Growth)) + 
	 ylab('LCUE ($/kWh)') +
	 xlab('Domestic Demand Growth') +
     geom_bar(position="dodge", stat="identity") +
	 coord_cartesian(expand=FALSE) +
	 theme_bw() +
	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
	 scale_fill_manual(name = "Climate / Rurality",values = climate_accessibility_colours)

	 ggsave(paste(plot_path,'CLOVER_Results/Everything_LCUE.png',sep=''),plot=p3,width=8,height=4)

p4<-ggplot(CLOVER_results, aes(fill=Climate_Accessibility, y=EmIntens, x=Growth)) + 
	 ylab('GHG intensity (gCO2e/kWh)') +
	 xlab('Domestic Demand Growth') +
     geom_bar(position="dodge", stat="identity") +
	 coord_cartesian(expand=FALSE) +
	 theme_bw() +
	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
	 scale_fill_manual(name = "Climate / Rurality",values = climate_accessibility_colours)

	 ggsave(paste(plot_path,'CLOVER_Results/Everything_EmIntens.png',sep=''),plot=p4,width=8,height=4)


p5<-ggplot(CLOVER_results, aes(fill=Climate_Accessibility, y=PV_y5_kW, x=Growth)) + 
	 ylab('PV size (kWp)') +
	 xlab('Domestic Demand Growth') +
     geom_bar(position="dodge", stat="identity") +
	 coord_cartesian(expand=FALSE) +
	 theme_bw() +
	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
	 scale_fill_manual(name = "Climate / Rurality",values = climate_accessibility_colours)

	 ggsave(paste(plot_path,'CLOVER_Results/Everything_PV_y5_kW.png',sep=''),plot=p5,width=8,height=4)

p6<-ggplot(CLOVER_results, aes(fill=Climate_Accessibility, y=StoPerPV_y5, x=Growth)) + 
	 ylab('Storage per PV (kWh/kWp)') +
	 xlab('Domestic Demand Growth') +
     geom_bar(position="dodge", stat="identity") +
	 coord_cartesian(expand=FALSE) +
	 theme_bw() +
	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
	 scale_fill_manual(name = "Climate / Rurality",values = climate_accessibility_colours)

	 ggsave(paste(plot_path,'CLOVER_Results/Everything_StoPerPV_y5.png',sep=''),plot=p6,width=8,height=4)


### Plots below commented out as not used in paper, but show fewer variables together & could aid comprehension

# Peri-urban VS Rural

# CLOVER_results_no_growth<-CLOVER_results[which(CLOVER_results$Growth=='None'),]

# variables<-c('PV_y0_kW','StoPerPV_y0','LCUE','EmIntens')
# y_labs<-c('PV size (kW)', 'Storage per PV (kWh/kW)', 'LCUE ($/kWh)', 'GHG intensity (gCO2e/kWh)')


# p1<-ggplot(CLOVER_results_no_growth, aes(fill=Accessibility, y=PV_y0_kW, x=Accessibility)) + 
# 	 ylab('PV size (kW)') +
# 	 xlab('Domestic Demand Growth') +
#      geom_bar(position="dodge", stat="identity") +
# 	 coord_cartesian(expand=FALSE) +
# 	 theme_bw() +
# 	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
# 	 scale_fill_manual(name = "Accessibility",values = accessibility_colours)

# 	 ggsave(paste(plot_path,'CLOVER_Results/NoGrowth_PV_y0_kW.png',sep=''),plot=p1,width=8,height=4)

# p2<-ggplot(CLOVER_results_no_growth, aes(fill=Accessibility, y=StoPerPV_y0, x=Accessibility)) + 
# 	 ylab('Storage per PV (kWh/kW)') +
# 	 xlab('Domestic Demand Growth') +
#      geom_bar(position="dodge", stat="identity") +
# 	 coord_cartesian(expand=FALSE) +
# 	 theme_bw() +
# 	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
# 	 scale_fill_manual(name = "Accessibility",values = accessibility_colours)

# 	 ggsave(paste(plot_path,'CLOVER_Results/NoGrowth_StoPerPV_y0.png',sep=''),plot=p2,width=8,height=4)

# p3<-ggplot(CLOVER_results_no_growth, aes(fill=Accessibility, y=LCUE, x=Accessibility)) + 
# 	 ylab('LCUE ($/kWh)') +
# 	 xlab('Domestic Demand Growth') +
#      geom_bar(position="dodge", stat="identity") +
# 	 coord_cartesian(expand=FALSE) +
# 	 theme_bw() +
# 	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
# 	 scale_fill_manual(name = "Accessibility",values = accessibility_colours)

# 	 ggsave(paste(plot_path,'CLOVER_Results/NoGrowth_LCUE.png',sep=''),plot=p3,width=8,height=4)

# p4<-ggplot(CLOVER_results_no_growth, aes(fill=Accessibility, y=EmIntens, x=Accessibility)) + 
# 	 ylab('GHG intensity (gCO2e/kWh)') +
# 	 xlab('Domestic Demand Growth') +
#      geom_bar(position="dodge", stat="identity") +
# 	 coord_cartesian(expand=FALSE) +
# 	 theme_bw() +
# 	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
# 	 scale_fill_manual(name = "Accessibility",values = accessibility_colours)

# 	 ggsave(paste(plot_path,'CLOVER_Results/NoGrowth_EmIntens.png',sep=''),plot=p4,width=8,height=4)



# # CLIMATE & GROWTH RATES


# CLOVER_results_w_growth<-CLOVER_results[which(CLOVER_results$Growth!='None'),]

# CLOVER_results_Peri-urban_growth<-CLOVER_results_w_growth[which(CLOVER_results_w_growth$Accessibility=='Peri-urban'),]


# p1<-ggplot(CLOVER_results_w_growth, aes(fill=Climate, y=PV_y0_kW, x=Growth)) + 
# 	 ylab('PV size (kW)') +
# 	 xlab('Domestic Demand Growth') +
#      geom_bar(position="dodge", stat="identity") +
# 	 coord_cartesian(expand=FALSE) +
# 	 theme_bw() +
# 	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
# 	 scale_fill_manual(name = "Climate",values = climate_colours)

# 	 ggsave(paste(plot_path,'CLOVER_Results/Growth_Climate_PV_y0_kW.png',sep=''),plot=p1,width=8,height=4)

# p2<-ggplot(CLOVER_results_w_growth, aes(fill=Climate, y=PV_y5_kW, x=Growth)) + 
# 	 ylab('PV size (kW)') +
# 	 xlab('Domestic Demand Growth') +
#      geom_bar(position="dodge", stat="identity") +
# 	 coord_cartesian(expand=FALSE) +
# 	 theme_bw() +
# 	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
# 	 scale_fill_manual(name = "Climate",values = climate_colours)

# 	 ggsave(paste(plot_path,'CLOVER_Results/Growth_Climate_PV_y5_kW.png',sep=''),plot=p2,width=8,height=4)

# p3<-ggplot(CLOVER_results_w_growth, aes(fill=Climate, y=StoPerPV_y0, x=Growth)) + 
# 	 ylab('Storage per PV (kWh/kW)') +
# 	 xlab('Domestic Demand Growth') +
#      geom_bar(position="dodge", stat="identity") +
# 	 coord_cartesian(expand=FALSE) +
# 	 theme_bw() +
# 	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
# 	 scale_fill_manual(name = "Climate",values = climate_colours)

# 	 ggsave(paste(plot_path,'CLOVER_Results/Growth_Climate_StoPerPV_y0.png',sep=''),plot=p3,width=8,height=4)

# p4<-ggplot(CLOVER_results_w_growth, aes(fill=Climate, y=StoPerPV_y5, x=Growth)) + 
# 	 ylab('Storage per PV (kWh/kW)') +
# 	 xlab('Domestic Demand Growth') +
#      geom_bar(position="dodge", stat="identity") +
# 	 coord_cartesian(expand=FALSE) +
# 	 theme_bw() +
# 	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
# 	 scale_fill_manual(name = "Climate",values = climate_colours)

# 	 ggsave(paste(plot_path,'CLOVER_Results/Growth_Climate_StoPerPV_y5.png',sep=''),plot=p4,width=8,height=4)

# p5<-ggplot(CLOVER_results_w_growth, aes(fill=Climate, y=LCUE, x=Growth)) + 
# 	 ylab('LCUE ($/kWh)') +
# 	 xlab('Domestic Demand Growth') +
#      geom_bar(position="dodge", stat="identity") +
# 	 coord_cartesian(expand=FALSE) +
# 	 theme_bw() +
# 	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
# 	 scale_fill_manual(name = "Climate",values = climate_colours)

# 	 ggsave(paste(plot_path,'CLOVER_Results/Growth_Climate_LCUE.png',sep=''),plot=p5,width=8,height=4)

# p6<-ggplot(CLOVER_results_w_growth, aes(fill=Climate, y=EmIntens, x=Growth)) + 
# 	 ylab('GHG intensity (gCO2e/kWh)') +
# 	 xlab('Domestic Demand Growth') +
#      geom_bar(position="dodge", stat="identity") +
# 	 coord_cartesian(expand=FALSE) +
# 	 theme_bw() +
# 	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
# 	 scale_fill_manual(name = "Climate",values = climate_colours)

# 	 ggsave(paste(plot_path,'CLOVER_Results/Growth_Climate_EmIntens.png',sep=''),plot=p6,width=8,height=4)



# # ACCESSIBILITY & GROWTH RATES


# p1<-ggplot(CLOVER_results_TSav, aes(fill=Accessibility, y=PV_y0_kW, x=Growth)) + 
# 	 ylab('PV size (kW)') +
#      geom_bar(position="dodge", stat="identity") +
# 	 coord_cartesian(expand=FALSE) +
# 	 theme_bw() +
# 	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
# 	 scale_fill_manual(name = "Accessibility",values = accessibility_colours)

# 	 ggsave(paste(plot_path,'CLOVER_Results/Growth_Accessibility_TSav_PV_y0_kW.png',sep=''),plot=p1,width=8,height=4)

# p2<-ggplot(CLOVER_results_TSav, aes(fill=Accessibility, y=StoPerPV_y0, x=Growth)) + 
# 	 ylab('Storage per PV (kWh/kW)') +
#      geom_bar(position="dodge", stat="identity") +
# 	 coord_cartesian(expand=FALSE) +
# 	 theme_bw() +
# 	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
# 	 scale_fill_manual(name = "Accessibility",values = accessibility_colours)

# 	 ggsave(paste(plot_path,'CLOVER_Results/Growth_Accessibility_TSav_StoPerPV_y0.png',sep=''),plot=p2,width=8,height=4)

# p3<-ggplot(CLOVER_results_TSav, aes(fill=Accessibility, y=LCUE, x=Growth)) + 
# 	 ylab('LCUE ($/kWh)') +
#      geom_bar(position="dodge", stat="identity") +
# 	 coord_cartesian(expand=FALSE) +
# 	 theme_bw() +
# 	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
# 	 scale_fill_manual(name = "Accessibility",values = accessibility_colours)

# 	 ggsave(paste(plot_path,'CLOVER_Results/Growth_Accessibility_TSav_LCUE.png',sep=''),plot=p3,width=8,height=4)

# p4<-ggplot(CLOVER_results_TSav, aes(fill=Accessibility, y=EmIntens, x=Growth)) + 
# 	 ylab('GHG intensity (gCO2e/kWh)') +
#      geom_bar(position="dodge", stat="identity") +
# 	 coord_cartesian(expand=FALSE) +
# 	 theme_bw() +
# 	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
# 	 scale_fill_manual(name = "Accessibility",values = accessibility_colours)

# 	 ggsave(paste(plot_path,'CLOVER_Results/Growth_Accessibility_TSav_EmIntens.png',sep=''),plot=p4,width=8,height=4)


# p5<-ggplot(CLOVER_results_TSav, aes(fill=Accessibility, y=PV_y5_kW, x=Growth)) + 
# 	 ylab('PV size (kW)') +
#      geom_bar(position="dodge", stat="identity") +
# 	 coord_cartesian(expand=FALSE) +
# 	 theme_bw() +
# 	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
# 	 scale_fill_manual(name = "Accessibility",values = accessibility_colours)

# 	 ggsave(paste(plot_path,'CLOVER_Results/Growth_Accessibility_TSav_PV_y5_kW.png',sep=''),plot=p5,width=8,height=4)

# p6<-ggplot(CLOVER_results_TSav, aes(fill=Accessibility, y=StoPerPV_y5, x=Growth)) + 
# 	 ylab('Storage per PV (kWh/kW)') +
#      geom_bar(position="dodge", stat="identity") +
# 	 coord_cartesian(expand=FALSE) +
# 	 theme_bw() +
# 	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
# 	 scale_fill_manual(name = "Accessibility",values = accessibility_colours)

# 	 ggsave(paste(plot_path,'CLOVER_Results/Growth_Accessibility_TSav_StoPerPV_y5.png',sep=''),plot=p6,width=8,height=4)



# p1<-ggplot(CLOVER_results_HSub, aes(fill=Accessibility, y=PV_y0_kW, x=Growth)) + 
# 	 ylab('PV size (kW)') +
#      geom_bar(position="dodge", stat="identity") +
# 	 coord_cartesian(expand=FALSE) +
# 	 theme_bw() +
# 	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
# 	 scale_fill_manual(name = "Accessibility",values = accessibility_colours)

# 	 ggsave(paste(plot_path,'CLOVER_Results/Growth_Accessibility_HSub_PV_y0_kW.png',sep=''),plot=p1,width=8,height=4)

# p2<-ggplot(CLOVER_results_HSub, aes(fill=Accessibility, y=StoPerPV_y0, x=Growth)) + 
# 	 ylab('Storage per PV (kWh/kW)') +
#      geom_bar(position="dodge", stat="identity") +
# 	 coord_cartesian(expand=FALSE) +
# 	 theme_bw() +
# 	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
# 	 scale_fill_manual(name = "Accessibility",values = accessibility_colours)

# 	 ggsave(paste(plot_path,'CLOVER_Results/Growth_Accessibility_HSub_StoPerPV_y0.png',sep=''),plot=p2,width=8,height=4)

# p3<-ggplot(CLOVER_results_HSub, aes(fill=Accessibility, y=LCUE, x=Growth)) + 
# 	 ylab('LCUE ($/kWh)') +
#      geom_bar(position="dodge", stat="identity") +
# 	 coord_cartesian(expand=FALSE) +
# 	 theme_bw() +
# 	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
# 	 scale_fill_manual(name = "Accessibility",values = accessibility_colours)

# 	 ggsave(paste(plot_path,'CLOVER_Results/Growth_Accessibility_HSub_LCUE.png',sep=''),plot=p3,width=8,height=4)

# p4<-ggplot(CLOVER_results_HSub, aes(fill=Accessibility, y=EmIntens, x=Growth)) + 
# 	 ylab('GHG intensity (gCO2e/kWh)') +
#      geom_bar(position="dodge", stat="identity") +
# 	 coord_cartesian(expand=FALSE) +
# 	 theme_bw() +
# 	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
# 	 scale_fill_manual(name = "Accessibility",values = accessibility_colours)

# 	 ggsave(paste(plot_path,'CLOVER_Results/Growth_Accessibility_HSub_EmIntens.png',sep=''),plot=p4,width=8,height=4)


# p5<-ggplot(CLOVER_results_HSub, aes(fill=Accessibility, y=PV_y5_kW, x=Growth)) + 
# 	 ylab('PV size (kW)') +
#      geom_bar(position="dodge", stat="identity") +
# 	 coord_cartesian(expand=FALSE) +
# 	 theme_bw() +
# 	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
# 	 scale_fill_manual(name = "Accessibility",values = accessibility_colours)

# 	 ggsave(paste(plot_path,'CLOVER_Results/Growth_Accessibility_HSub_PV_y5_kW.png',sep=''),plot=p5,width=8,height=4)

# p6<-ggplot(CLOVER_results_HSub, aes(fill=Accessibility, y=StoPerPV_y5, x=Growth)) + 
# 	 ylab('Storage per PV (kWh/kW)') +
#      geom_bar(position="dodge", stat="identity") +
# 	 coord_cartesian(expand=FALSE) +
# 	 theme_bw() +
# 	 theme(text = element_text(size=20),panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
# 	 scale_fill_manual(name = "Accessibility",values = accessibility_colours)

# 	 ggsave(paste(plot_path,'CLOVER_Results/Growth_Accessibility_HSub_StoPerPV_y5.png',sep=''),plot=p6,width=8,height=4)


