#===================================================================

# Script to make plots
# Requires CRSeafood Functions and CRSeafood Call Script

# Kat Millage
# Last modified 3/14/16

#====================================================================

#===PART 1=======================================
# Prices and catches
#================================================

priceplot <- ggplot() +
  geom_line(data = prices, aes(Year, dollars), color = '#187E85', size = 2) +
  geom_point(data = prices, aes(Year, dollars), color = '#187E85', size = 5)+
  geom_hline(yintercept=mean(prices$dollars), size = 2, linetype = "dashed")+
  labs(y ="Average Dock Price (2013 $USD/kg)", x = "Year")+
  theme_bw()+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  theme(axis.text.y = element_text(size=20, margin = margin(0, 5, 0, 0)), axis.title.y = element_text(size=22, vjust=1, face = "bold", margin = margin(0, 20, 0, 0)))+
  theme(axis.text.x = element_text(size=20, margin = margin(3, 0, 0, 0)), axis.title.x = element_text(size=22, vjust=1, face = "bold", margin = margin(10, 0, 0, 0)))+
  theme(legend.position = "none")+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, 
                                    linetype="solid"))+
  scale_y_continuous(limits = c(3,6), breaks = c(3,3.5, 4, 4.5, 5, 5.5, 6))

#CPUE plot

cpueplot <- ggplot() +
  geom_line(data = CPUE, aes(Year, CPUE), color = '#187E85', size = 2) +
  geom_point(data = CPUE, aes(Year, CPUE), color = '#187E85', size = 5)+
  geom_hline(yintercept=mean(CPUE$CPUE), size = 2, linetype = "dashed")+
  labs(y ="Catch per Unit Effort (snapper/trip)", x = "Year")+
  theme_bw()+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  theme(axis.text.y = element_text(size=24, margin = margin(0, 5, 0, 0)), axis.title.y = element_text(size=26, vjust=1, face = "bold", margin = margin(0, 20, 0, 0)))+
  theme(axis.text.x = element_text(size=24, margin = margin(3, 0, 0, 0)), axis.title.x = element_text(size=26, vjust=1, face = "bold", margin = margin(10, 0, 0, 0)))+
  theme(legend.position = "none")+
  theme(panel.border = element_rect(fill=NA,color="black", size=3, 
                                    linetype="solid"))+
  scale_x_continuous(limits = c(2007,2013), breaks = c(2007,2008, 2009, 2010, 2011, 2012, 2013))

# Catch plot
catchesplot <- ggplot() +
  geom_line(data = best_fit, aes(Year, (catch/1000)), color = '#187E85', size = 2) +
  geom_point(data = best_fit, aes(Year, (catch/1000)), color = '#187E85', size = 5)+
  geom_hline(yintercept=mean(best_fit$catch/1000), size = 2, linetype = "dashed")+
  #geom_area(data = best_fit, aes(Year, ((catch/2.6)/1000)), fill = '#32363B') +
  #geom_point(data = best_fit, aes(Year, ((catch/2.6)/1000)), color = '#32363B', size = 5) +
  labs(x="Year", y ="Total Catch (tons)")+
  theme_bw()+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  theme(axis.text.y = element_text(size=20, margin = margin(0, 5, 0, 0)), axis.title.y = element_text(size=22, vjust=1, face = "bold", margin = margin(0, 20, 0, 0)))+
  theme(axis.text.x = element_text(size=20, margin = margin(3, 0, 0, 0)), axis.title.x = element_text(size=22, vjust=1, face = "bold", margin = margin(10, 0, 0, 0)))+
  theme(legend.position = "none")+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, 
                                    linetype="solid"))+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2020, 2030, 2040, 2050))+
  scale_y_continuous(breaks = c(0,250, 500, 750, 1000, 1250, 1500))

#===PART 2=======================================
# Model fit plots Schaefer
#================================================

# Cbserved CPUE vs. predicted (Schaefer)
obs_vs_pred_cpue_plot <- ggplot() +
  geom_ribbon(data = best_fit, aes(Year, ymin = lowerCPUE, ymax = upperCPUE), alpha = 0.2, fill = "black")+
  geom_point(data = best_fit, aes(Year,CPUE), color = 'black', size = 3) +
  geom_line(data = best_fit, aes(Year, pred_cpue), color = 'black', size = 2) +
  labs(y ="CPUE (#/trip)", x = "Year")+
  theme_bw()+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  theme(axis.text.y = element_text(size=20, margin = margin(0, 5, 0, 0)), axis.title.y = element_text(size=22, vjust=1, face = "bold", margin = margin(0, 20, 0, 0)))+
  theme(axis.text.x = element_text(size=20, margin = margin(3, 0, 0, 0)), axis.title.x = element_text(size=22, vjust=1, face = "bold", margin = margin(10, 0, 0, 0)))+
  theme(legend.position = "none")+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, 
                                    linetype="solid"))+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010))+
  scale_y_continuous(limits = c(0, 450), breaks = c(0, 100, 200, 300, 400))

# Pella
obs_vs_pred_cpue_plot_pella <- ggplot() +
  geom_ribbon(data = best_fit_pella, aes(Year, ymin = lowerCPUE, ymax = upperCPUE), alpha = 0.2, fill = "black")+
  geom_point(data = best_fit_pella, aes(Year,CPUE), color = 'black', size = 3) +
  geom_line(data = best_fit_pella, aes(Year, pred_cpue), color = 'black', size = 2) +
  labs(y ="CPUE (#/trip)", x = "Year")+
  theme_bw()+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  theme(axis.text.y = element_text(size=24, margin = margin(0, 5, 0, 0)), axis.title.y = element_text(size=26, vjust=1, face = "bold", margin = margin(0, 20, 0, 0)))+
  theme(axis.text.x = element_text(size=24, margin = margin(3, 0, 0, 0)), axis.title.x = element_text(size=26, vjust=1, face = "bold", margin = margin(10, 0, 0, 0)))+
  theme(legend.position = "none")+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, 
                                    linetype="solid"))+
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2020, 2030, 2040, 2050))+
  scale_y_continuous(limits = c(0, 200), breaks = c(0, 100, 200, 300, 400,500,600,700))

grid.arrange(obs_vs_pred_cpue_plot, obs_vs_pred_cpue_plot_pella, nrow = 1, ncol = 2)

########### Biomass plot

# Schaefer
biomass_plot <- ggplot()+
  geom_line(data = best_fit, aes(Year, biomass/1000), color = '#4E4E4E', size = 2) +
  geom_line(data = bau, aes(Year, biomass/1000), color = "#187E85", size = 2)+
  geom_ribbon(data = model_results_yearaverage, aes(Year, ymin = (min_biomass/1000), ymax = (max_biomass/1000), group = gamma_start, fill = gamma_start), alpha = 0.3)+
  scale_fill_continuous(low = "#57275E", high = "#EDA3F9", breaks = c(0.1, 0.5, 1), name = "Percentage of Region Certified", labels = c("10%", "50%", "100%"))+
  geom_line(data = model_results_yearaverage, aes(x =Year, y = (avg_biomass/1000), group = gamma_start, color = gamma_start), size = 2)+
  scale_colour_continuous(low = "#57275E", high = "#EDA3F9", breaks = c(0.1, 0.5, 1), name = "Percentage of Region Certified", labels = c("10%", "50%", "100%"))+
  facet_grid(gamma_start ~ ., scales = "free")+
  labs(x="Year", y ="Biomass (tons)")+
  theme_bw()+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  theme(axis.text.y = element_text(size=24, margin = margin(0, 5, 0, 0)), axis.title.y = element_text(size=26, vjust=1, face = "bold", margin = margin(0, 10, 0, 0)))+
  theme(axis.text.x = element_text(size=24, margin = margin(3, 0, 0, 0)), axis.title.x = element_text(size=26, vjust=1, face = "bold", margin = margin(10, 0, 0, 0)))+
  theme(legend.position = "none")+
  theme(strip.background = element_blank())+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, 
                                    linetype="solid"))+
  theme(legend.position = "none")+
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020, 2030, 2040, 2050))


# Pella
biomass_plot_pella <- ggplot()+
  geom_ribbon(data = best_fit_pella, aes(Year,ymin = lowerbiomass, ymax = upperbiomass), alpha = 0.2, fill = "#002A66")+
  #geom_ribbon(data = projection, aes(Year, ymin = lowerbiomass, ymax = upperbiomass), alpha = 0.2, fill = "#FF570B")+
  geom_line(data = best_fit_pella, aes(Year, biomass), color = '#002A66') +
  #geom_line(data = projection, aes(Year, biomass), color = "#FF570B")+
  labs(x="Year", y ="Biomass (kg)")+
  theme_bw()+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=14, vjust=1, face = "bold"))+
  theme(axis.text.x = element_text(size=12), axis.title.x = element_text(size=14, vjust=0.1, face = "bold"))+
  theme(legend.position = "none")+
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020, 2030, 2040, 2050))

####### Plotting catch/msy history

# Schaefer
catch_msy_plot <- ggplot(best_fit, aes(Year, CvMSY)) +
  geom_line(color = "#187E85", size = 2) +
  geom_hline(aes(yintercept = 1), color = 'black', size = 2, linetype = "dashed")+
  labs(x="Year", y ="Catch/MSY")+
  theme_bw()+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  theme(axis.text.y = element_text(size=24, margin = margin(0, 5, 0, 0)), axis.title.y = element_text(size=26, vjust=1, face = "bold", margin = margin(0, 20, 0, 0)))+
  theme(axis.text.x = element_text(size=24, margin = margin(3, 0, 0, 0)), axis.title.x = element_text(size=26, vjust=1, face = "bold", margin = margin(10, 0, 0, 0)))+
  theme(legend.position = "none")+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, 
                                    linetype="solid"))+
  scale_y_continuous(limits = c(0,3), breaks = c(0, 1, 2, 3))

# Pella
catch_msy_plot_pella <- ggplot(best_fit_pella, aes(Year, CvMSY)) +
  geom_line(color = "#187E85", size = 2) +
  geom_hline(aes(yintercept = 1), color = 'black', size = 2, linetype = "dashed")+
  labs(x="Year", y ="Catch/MSY")+
  theme_bw()+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  theme(axis.text.y = element_text(size=24, margin = margin(0, 5, 0, 0)), axis.title.y = element_text(size=26, vjust=1, face = "bold", margin = margin(0, 20, 0, 0)))+
  theme(axis.text.x = element_text(size=24, margin = margin(3, 0, 0, 0)), axis.title.x = element_text(size=26, vjust=1, face = "bold", margin = margin(10, 0, 0, 0)))+
  theme(legend.position = "none")+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, 
                                    linetype="solid"))+
  scale_y_continuous(limits = c(0,3), breaks = c(0, 1, 2, 3))

grid.arrange(catch_msy_plot, catch_msy_plot_pella, nrow = 1, ncol = 2)

####### Kobe plot

# Schaefer 
kobe_trend_plot <- ggplot(best_fit, aes(BvBmsy, FvFmsy)) +
  geom_point(size = 5, aes(color = Year)) +
  labs(x = expression(B/B[MSY]), y = expression(F/F[MSY]))+
  geom_path(aes(colour = Year), size = 1) +
  theme_bw()+
  geom_hline(aes(yintercept = 1), linetype = 'longdash', size = 1.5) +
  geom_vline(aes(xintercept = 1), linetype = 'longdash', size = 1.5) +
  theme(axis.text.y = element_text(size=24, margin = margin(0, 5, 0, 0)), axis.title.y = element_text(size=26, vjust=1, face = "bold", margin = margin(0, 15, 0, 0)))+
  theme(axis.text.x = element_text(size=24, margin = margin(3, 0, 0, 0)), axis.title.x = element_text(size=26, vjust=1, face = "bold", margin = margin(10, 0, 0, 0)))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, 
                                    linetype="solid"))+
  theme(legend.title = element_text(size = 20), legend.text = element_text(size = 18))+
  scale_y_continuous(limits = c(0,9), breaks = c(0,1,2,3,4,5,6,7,8,9))+
  scale_x_continuous(limits = c(0,2), breaks = c(0,0.5,1,1.5,2))+
  scale_colour_gradient(low = '#2C3E50', high = '#FC6D68')+
  guides(colour = guide_colorbar(barwidth = 0.5, barheight = 10))

# Pella 
kobe_trend_plot_pella <- ggplot(best_fit_pella, aes(BvBmsy, FvFmsy)) +
  geom_point(size = 5, aes(color = Year)) +
  labs(x = expression(B/B[MSY]), y = expression(F/F[MSY]))+
  geom_path(aes(colour = Year), size = 1) +
  theme_bw()+
  geom_hline(aes(yintercept = 1), linetype = 'longdash', size = 1.5) +
  geom_vline(aes(xintercept = 1), linetype = 'longdash', size = 1.5) +
  theme(axis.text.y = element_text(size=24, margin = margin(0, 5, 0, 0)), axis.title.y = element_text(size=26, vjust=1, face = "bold", margin = margin(0, 15, 0, 0)))+
  theme(axis.text.x = element_text(size=24, margin = margin(3, 0, 0, 0)), axis.title.x = element_text(size=26, vjust=1, face = "bold", margin = margin(10, 0, 0, 0)))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, 
                                    linetype="solid"))+
  theme(legend.title = element_text(size = 20), legend.text = element_text(size = 18))+
  scale_y_continuous(limits = c(0,3), breaks = c(0,1,2,3,4,5,6,7,8,9))+
  scale_x_continuous(limits = c(0,2), breaks = c(0,0.5,1,1.5,2))+
  scale_colour_gradient(low = '#2C3E50', high = '#FC6D68')+
  guides(colour = guide_colorbar(barwidth = 0.5, barheight = 10))

grid.arrange(kobe_trend_plot, kobe_trend_plot_pella, nrow = 1, ncol = 2)

#===PART 3=======================================
# BAU and Fair Trade USA Projections
#================================================

# Efort plot for BAU vs. Scenarios
effort_plot <- ggplot(model_results_change)+
  geom_line(aes(x =Year, y = effort, group = scenario, colour = effortreduction))  +
  geom_line(data = bau_test, aes(x =Year, y = effort), colour = "blue")+
  scale_colour_continuous(low = "#6E0401", high = "#FC6D68", breaks = c(1, 0.8, 0.7, 0.5), name = "Effort Reduction", labels = c("0", "20%", "30%", "50%"))+
  labs(x="Year", y ="Effort (trips)")+
  theme_bw()+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  theme(axis.text.y = element_text(size=20), axis.title.y = element_text(size=22, vjust=1, face = "bold"))+
  theme(axis.text.x = element_text(size=20), axis.title.x = element_text(size=22, vjust=1, face = "bold"))

# Biomass plot for BAU vs. Scenarios
biomass_plot <- ggplot()+
  geom_ribbon(data = best_fit, aes(Year, ymin = (lowerbiomass/1000), ymax = (upperbiomass/1000)), alpha = 0.2, fill = "black")+
  geom_line(data = best_fit, aes(Year, (biomass/1000)), color = 'black', size = 2) +
  #geom_line(data = model_results_change, aes(x =Year, y = (biomass/1000), group = scenario, colour = effortreduction), size = 2)  +
  geom_line(data = bau, aes(x =Year, y = (biomass/1000)), colour = "#187E85", size = 2)+
  scale_colour_continuous(low = "#6E0401", high = "#FC6D68", breaks = c(1, 0.8, 0.7, 0.5), name = "Effort Reduction", labels = c("0", "20%", "30%", "50%"))+
  labs(x="Year", y ="Biomass (tons)")+
  theme_bw()+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  theme(axis.text.y = element_text(size=24, margin = margin(0, 5, 0, 0)), axis.title.y = element_text(size=24, vjust=1, face = "bold", margin = margin(0, 10, 0, 0)))+
  theme(axis.text.x = element_text(size=24, margin = margin(3, 0, 0, 0)), axis.title.x = element_text(size=26, vjust=1, face = "bold", margin = margin(10, 0, 0, 0)))+
  theme(legend.position = "none")+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, 
                                    linetype="solid"))+
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020, 2030, 2040, 2050))+
  scale_y_continuous(limits = c(0,10100), breaks = c(0, 2500, 5000, 7500, 10000))
  

# Biomass/profit tradeoff plot for scenarios
biomass_profit_plot <- ggplot(model_results_summary_0.99)+
  geom_point(aes(x =(last_biomass/1000), y = npv/1000, color = gamma), size = 4, alpha = 0.8)+
  geom_point(aes(x =(last_biomass_bau/1000), y = npv_bau/1000), size = 4, color = "#187E85")+
  scale_color_continuous(low = "#6E0401", high = "#FC6D68", breaks = c(1, 0.5, 0.1))+
  #labs(x="Biomass in 2050 (tons)", y ="NPV of 2050 Fishery Profits (thousands of 2013 $)")+
  theme_bw()+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  theme(axis.text.y = element_text(size=22, margin = margin(0, 5, 0, 0)), axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size=22, margin = margin(3, 0, 0, 0)), axis.title.x = element_blank())+
  #theme(legend.position = "none")+
  theme(panel.border = element_rect(fill=NA,color="black", size=3, 
                                    linetype="solid"))+
  scale_y_continuous(limits = c(-50,0), breaks = c(-50,-40, -30,-20,-10,0))+
  scale_x_continuous(limits=c(0,200), breaks = c(0, 50, 100, 150, 200))
  #theme(legend.position = "none")

# Biomass/cdp tradeoff plot for scenarios
biomass_cdp_plot <- ggplot(model_results_summary_0.8)+
  geom_point(aes(x =(last_biomass/1000), y = cdp_per_com, color = gamma), size = 5, alpha = 0.8)+
  geom_point(aes(x =(last_biomass_bau/1000), y = cdp_bau), size = 6, color = "#187E85")+
  labs(x="Snapper Biomass in 2050 (tons)", y ="Average CDP per Community (2013 $US)")+
  #geom_point(data = modsum_filtered, aes(x =(last_biomass/1000), y = cdp_per_com), size = 5, alpha = 0.8, color = "#FC6D68")+
  #geom_point(aes(x =(bau_biomass_change/1000), y = 0), size = 8, color = "blue")+
  #scale_shape_discrete(range = c(4,12))+
  scale_color_continuous(low = "#6E0401", high = "#FC6D68", breaks = c(1, 0.5, 0.1))+
  #labs(x="Change in Biomass Relative to BAU (tons)", y ="Average CDP per Community ($)")+
  theme_bw()+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  theme(axis.text.y = element_text(size=20, margin = margin(0, 7, 0, 0)), axis.title.y = element_text(size=22, vjust=1, face = "bold", margin = margin(0, 10, 0, 0)))+
  theme(axis.text.x = element_text(size=20, margin = margin(7, 0, 0, 0)), axis.title.x = element_text(size=22, vjust=1, face = "bold", margin = margin(10, 0, 0, 0)))+
  theme(legend.position = "none")+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, 
                                    linetype="solid"))
  #theme(legend.position = "none")+
  #scale_y_continuous(limits = c(0,1500), breaks = c(0, 500, 1000,1500))+
#scale_x_continuous(limits=c(0,2500), breaks = c(0, 500, 1000, 1500, 2000, 2500))

grid.arrange(biomass_profit_plot, biomass_cdp_plot, nrow = 2, ncol = 1)

# Catch MSY plot for projection. 
catch_msy_plot2 <- ggplot(best_fit, aes(Year, CvMSY)) +
  geom_line(color = "#187E85", size = 2) +
  geom_line(data = BAU_extend, aes(Year, CvMSY), color = "black", size = 2)+
  geom_hline(aes(yintercept = 1), color = 'black', size = 2, linetype = "dashed")+
  labs(x="Year", y ="Catch/MSY")+
  theme_bw()+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  theme(axis.text.y = element_text(size=24, margin = margin(0, 5, 0, 0)), axis.title.y = element_text(size=26, vjust=1, face = "bold", margin = margin(0, 20, 0, 0)))+
  theme(axis.text.x = element_text(size=24, margin = margin(3, 0, 0, 0)), axis.title.x = element_text(size=26, vjust=1, face = "bold", margin = margin(10, 0, 0, 0)))+
  theme(legend.position = "none")+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, 
                                    linetype="solid"))+
  scale_y_continuous(limits = c(0,3), breaks = c(0, 1, 2, 3))+
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020, 2030, 2040, 2050))

# Kobe plot
kobe_trend_plot2 <- ggplot(best_fit, aes(BvBmsy, FvFmsy)) +
  geom_point(size = 5, aes(color = Year)) +
  geom_point(data = BAU_extend, aes(BvBmsy, FvFmsy), size = 5)+
  labs(x = expression(B/B[MSY]), y = expression(F/F[MSY]))+
  geom_path(aes(colour = Year), size = 1) +
  theme_bw()+
  geom_hline(aes(yintercept = 1), linetype = 'longdash', size = 1.5) +
  geom_vline(aes(xintercept = 1), linetype = 'longdash', size = 1.5) +
  theme(axis.text.y = element_text(size=24, margin = margin(0, 5, 0, 0)), axis.title.y = element_text(size=26, vjust=1, face = "bold", margin = margin(0, 15, 0, 0)))+
  theme(axis.text.x = element_text(size=24, margin = margin(3, 0, 0, 0)), axis.title.x = element_text(size=26, vjust=1, face = "bold", margin = margin(10, 0, 0, 0)))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, 
                                    linetype="solid"))+
  theme(legend.title = element_text(size = 20), legend.text = element_text(size = 18))+
  scale_y_continuous(limits = c(0,9), breaks = c(0,1,2,3,4,5,6,7,8,9))+
  scale_x_continuous(limits = c(0,2), breaks = c(0,0.5,1,1.5,2))+
  scale_colour_gradient(low = '#2C3E50', high = '#FC6D68')+
  guides(colour = guide_colorbar(barwidth = 0.5, barheight = 10))

