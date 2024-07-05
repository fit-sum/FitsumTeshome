
library(dplyr)
#setwd("C:/Users/17867/OneDrive - University of Florida/Desktop/Crop water stress indices")
setwd("C:/Users/fteshome/OneDrive - University of Florida/Desktop/Crop water stress indices")

#CanopyTemp	AirTemp	Plot	Date	Time
IRT202122 <- read.csv("21_22_processed_ground_data.csv")

IRT202122$Date <- as.Date(IRT202122$Date, format = "%m/%d/%y")
#IRT202122$Date <- format(IRT202122$Date, "%m/%d/%Y")

IRT202122$Date <- as.Date(IRT202122$Date, format = "%m/%d/%Y")
IRT202122 <- IRT202122[IRT202122$Date >= as.Date("2021-12-01") & IRT202122$Date <= as.Date("2022-03-03"), ]

#Combine date and time columns into a single datetime column
IRT202122$DateTime <- as.POSIXct(paste(IRT202122$Date, IRT202122$Time), format = "%Y-%m-%d %H:%M:%S")
IRT202122$AirTemp <- as.numeric(IRT202122$AirTemp)

# Assuming you have a column named "Plot" indicating the plot number
HourlyIRT202122 <- aggregate(cbind(CanopyTemp, AirTemp) ~ Plot + 
                               cut(DateTime, breaks = "hour"), data = IRT202122, FUN = mean)
colnames(HourlyIRT202122) <- c("Plot","Date","CanopyTemp","AirTemp")

S2Chourlyweather <- read.csv("S2Chourlyweatherc.csv", header = T)
#write.csv(S2Chourlyweather,"S2Chourlyweatherc.csv")
common_dates <- intersect(S2Chourlyweather$Date, HourlyIRT202122$Date);common_dates

HourlyIRTweather202122 <- merge(HourlyIRT202122, S2Chourlyweather, by = c("Date"))

HourlyIRTweather202122$Elev <- 2
library(lubridate)
#s1cweather$Tmax <- as.numeric(s1cweather$Tmax)
#s1cweather$RHmax <- as.numeric(s1cweather$RHmax)
#s1cweather$RHav <- (s1cweather$RHmin+s1cweather$RHmax)/2

#HourlyIRTweather202122$Date <- as.POSIXct(HourlyIRTweather202122$Date, format = "%d/%m/%Y")
#s1cweather$Date <- mdy(s1cweather$Date)
#s1cweather$Tav <- (s1cweather$Tmax+s1cweather$Tmin)/2

HourlyIRTweather202122$Date <- as.POSIXct(as.character(HourlyIRTweather202122$Date), format = "%Y-%m-%d %H:%M:%S")
HourlyIRTweather202122$DOY <- yday(HourlyIRTweather202122$Date)
#HourlyIRTweather202122$ws <- HourlyIRTweather202122$Wsav*4.87/(log2(67.8*10-5.42))
HourlyIRTweather202122$ws <- HourlyIRTweather202122$Wsav

#25.53N 
HourlyIRTweather202122$eotmin <- 0.6108*exp((17.27*HourlyIRTweather202122$Tmin)/(HourlyIRTweather202122$Tmin+237.3))
HourlyIRTweather202122$eotmax <- 0.6108*exp((17.27*HourlyIRTweather202122$Tmax)/(HourlyIRTweather202122$Tmax+237.3))
HourlyIRTweather202122$es <- (HourlyIRTweather202122$eotmax+HourlyIRTweather202122$eotmin)/2
range(HourlyIRTweather202122$es)
#HourlyIRTweather202122$ea <- ((HourlyIRTweather202122$eotmin*HourlyIRTweather202122$RHmax/100)+(HourlyIRTweather202122$eotmax*HourlyIRTweather202122$RHmin/100))/2

HourlyIRTweather202122$eatmin <- 0.6108*exp(17.27*HourlyIRTweather202122$Tmin/(HourlyIRTweather202122$Tmin+237.3))
HourlyIRTweather202122$eatmax <- 0.6108*exp(17.27*HourlyIRTweather202122$Tmax/(HourlyIRTweather202122$Tmax+237.3))
HourlyIRTweather202122$ea <- (HourlyIRTweather202122$RH/100)*((HourlyIRTweather202122$eatmin + HourlyIRTweather202122$eatmax)/2)
range(HourlyIRTweather202122$ea)
HourlyIRTweather202122$VPD <- HourlyIRTweather202122$es-HourlyIRTweather202122$ea
range(HourlyIRTweather202122$VPD)

HourlyIRTweather202122$delta <- 4098*((0.6108*exp((17.27*HourlyIRTweather202122$Tav)/(HourlyIRTweather202122$Tav+237.3))))/((HourlyIRTweather202122$Tav+237.3)^2)
HourlyIRTweather202122$p <- 101.3*((293-0.0065*HourlyIRTweather202122$Elev)/293)^5.26
HourlyIRTweather202122$y <- HourlyIRTweather202122$p*0.000665  #psychrometeric constant
HourlyIRTweather202122$dr <- 1+0.033*cos(2*pi*HourlyIRTweather202122$DOY/365)
HourlyIRTweather202122$d1c <-  0.409*(sin((2*pi*HourlyIRTweather202122$DOY/365)-1.39))
HourlyIRTweather202122$lat_rad <- 25.53

HourlyIRTweather202122$ws1 <- acos(-tan(pi*HourlyIRTweather202122$lat_rad/180)*tan(HourlyIRTweather202122$d1c))
HourlyIRTweather202122$ra <-  24*60*0.082*HourlyIRTweather202122$dr*(HourlyIRTweather202122$ws1*sin(pi*HourlyIRTweather202122$lat_rad/180)*sin(HourlyIRTweather202122$d1c)+cos(pi*HourlyIRTweather202122$lat_rad/180)*cos(HourlyIRTweather202122$lat_rad)*sin(HourlyIRTweather202122$ws1))/pi
HourlyIRTweather202122$rso <- (0.75+(2*10^-5)*HourlyIRTweather202122$Elev)*HourlyIRTweather202122$ra

HourlyIRTweather202122$RsdRso <- HourlyIRTweather202122$SRad/HourlyIRTweather202122$rso

HourlyIRTweather202122$rns <-  (1-0.23)*HourlyIRTweather202122$SRad

HourlyIRTweather202122$rnl <- 4.903*10^-9*(((HourlyIRTweather202122$Tmax+273.16)^4+(HourlyIRTweather202122$Tmin+273.16)^4)/2)*(0.34-0.14*sqrt(HourlyIRTweather202122$ea))*((1.35*HourlyIRTweather202122$SRad/HourlyIRTweather202122$rso)-0.35)

HourlyIRTweather202122$rn <- HourlyIRTweather202122$rns-HourlyIRTweather202122$rnl

HourlyIRTweather202122$PM <- (0.408*HourlyIRTweather202122$delta*HourlyIRTweather202122$rn+HourlyIRTweather202122$y*(900/(HourlyIRTweather202122$Tav+273))*HourlyIRTweather202122$ws*(HourlyIRTweather202122$es-HourlyIRTweather202122$ea))/(HourlyIRTweather202122$delta+HourlyIRTweather202122$y*(1+0.34*HourlyIRTweather202122$ws))

# Convert datetime column to POSIXct format
datetime_posix <- as.POSIXct(HourlyIRTweather202122$Date, format = "%Y-%m-%d %H")

HourlyIRTweather202122$dtll <- HourlyIRTweather202122$CanopyTemp -HourlyIRTweather202122$AirTemp

write.csv(HourlyIRTweather202122,"HourlyIRTweather202122.csv")

# Extract date and time components
HourlyIRTweather202122$datec <- format(datetime_posix, "%Y-%m-%d")
HourlyIRTweather202122$timec <- format(datetime_posix, "%H:%M:%S")

IrrigS2 <- read.csv("IrrigS2C.csv", header = T)
#write.csv(IrrigS2, "IrrigS2C.csv")
common_dates1 <- intersect(HourlyIRTweather202122$datec, IrrigS2$Date);common_dates1 

Rainyday <- as.data.frame(S2Chourlyweather[S2Chourlyweather$Rain > 2, c("Date", "Rain")])
datetime_posix2 <- as.POSIXct(Rainyday$Date, format = "%Y-%m-%d %H")
Rainyday$Dateonly <- format(datetime_posix2, "%Y-%m-%d")
Rainyday$Time <- format(datetime_posix2, "%H:%M:%S")
common_dates2 <- intersect(HourlyIRTweather202122$datec,Rainyday$Dateonly);common_dates2

combined_dates <- union(common_dates1, common_dates2);combined_dates

# It should one day after irrigation and rainfall events
#datep <- "2021-12-22", "2021-12-24", "2021-12-27", "2021-12-30", "2022-01-03", 
#"2022-01-07", "2022-01-14", "2022-01-26","2022-02-04", "2021-12-15", "2021-12-16", 
#"2021-12-21", "2022-01-08", "2022-01-09", "2022-01-16", "2022-01-21", "2022-01-27"

#datep+1 <- c("2021-12-23", "2021-12-25", "2021-12-28", "2021-12-31", "2022-01-04", 
#"2022-01-08", "2022-01-15", "2022-01-27","2022-02-05", "2021-12-16", "2021-12-17", 
#"2021-12-22", "2022-01-09", "2022-01-10", "2022-01-17", "2022-01-22", "2022-01-28")

#PlotFI <- c('3','7','12','15','17','22')

# Load the necessary libraries
library(ggplot2)
library(ggpubr)

# Filter data for the specified time range and dates
HourlyIRTweather202122SLD <- HourlyIRTweather202122 %>%
  filter(datec%in%c("2021-12-23", "2021-12-25", "2021-12-28", "2021-12-31", "2022-01-04", 
                    "2022-01-08", "2022-01-15", "2022-01-27","2022-02-05", "2021-12-16", "2021-12-17", 
                    "2021-12-22", "2022-01-09", "2022-01-10", "2022-01-17", "2022-01-22", "2022-01-28"))%>% 
  filter(format(as.POSIXct(Date), "%H:%M:%S") >= "10:00:00"&
           format(as.POSIXct(Date), "%H:%M:%S") <= "14:00:00")%>% 
  filter(Plot%in%c('2','3','5','7','9','12','15','16','17','19','21','22'))%>%filter(RsdRso>=0.7)
#'2','3','5','7','9','12','15','16','17','19','21','22'

# Create a ggplot for dtll vs VPD
plot202122A <- ggplot(HourlyIRTweather202122SLD, aes(x = VPD, y = dtll)) +
  geom_point() +
  labs(x = "VPD", y = "dtll") +
  theme_minimal()

# Facet the plot by dates and plots
plot202122 <- plot202122A + facet_grid(Plot ~ datec)+
  stat_regline_equation()+
  stat_cor(aes(label = paste(..rr.label..)),
           label.x.npc = "left", label.y.npc = "bottom");plot202122

############################
# Define the function to perform linear regression and filter based on R-squared
filter_by_r_squared <- function(data) {
  lm_results <- lm(dtll ~ VPD, data = data)
  slope <- coef(lm_results)[2]  # Extracting the slope coefficient
  r_squared <- summary(lm_results)$r.squared
  if (r_squared > 0.65 && slope < 0) {  # Adding condition for negative slope
    return(data)
  } else {
    return(NULL)
  }
}

# Filter data for specified dates and plots
filtered_data <- HourlyIRTweather202122SLD %>%
  filter(datec %in% c("2021-12-23", "2021-12-25", "2021-12-28", "2021-12-31", "2022-01-04", 
                      "2022-01-08", "2022-01-15", "2022-01-27","2022-02-05", "2021-12-16", "2021-12-17", 
                      "2021-12-22", "2022-01-09", "2022-01-10", "2022-01-17", "2022-01-22", "2022-01-28")) %>%
  filter(Plot %in% c('2','3','5','7','9','12'))
  #filter(Plot %in% c('15','16','17','19','21','22'))

# Group data by date and plot, perform linear regression, and filter by R-squared
filtered_datasets <- filtered_data %>%
  group_split(datec, Plot) %>%
  lapply(filter_by_r_squared) %>%
  Filter(Negate(is.null), .)

# Combine the filtered datasets into a single dataset
combined_dataset <- bind_rows(filtered_datasets)

write.csv(combined_dataset, "GBS2.csv")

#Perform linear regression
lm_model <- lm(dtll ~ VPD, data = combined_dataset)

# Extract coefficients and R-squared value
intercept <- coef(lm_model)[1]
slope <- coef(lm_model)[2]
r_squared <- summary(lm_model)$r.squared

# Perform linear regression
# Create the scatter plot
plot1 <- ggplot(combined_dataset, aes(x = VPD, y = dtll)) +
  geom_point() +  
  #geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +  
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  annotate("text", x = max(combined_dataset$VPD), y = max(combined_dataset$dtll),
           label = paste("y =", round(intercept, 2), round(slope, 2), "* x"), hjust = 1) +  
  annotate("text", x = 0.80, y = 2.5,
           label = paste("R² =", round(r_squared, 2)), vjust = 0, hjust = 1) +  
  labs(x = "VPD", y = "dtll") +  
  theme_minimal();plot1

#ggsave("SCplotS1.png", plot = plotSCS2, width = 11, height = 8, dpi = 300)

####################
# Sweet corn
a_sc <- -3.43
b_sc <- 0.51
r2 = 0.75

# Green beans
a_gb <- -1.86
b_gb <- -0.38
r2 = 0.25

# Calculate Tav_b
HourlyIRTweather202122$Tav_b <- ifelse(HourlyIRTweather202122$Plot >= 13 & 
                                         HourlyIRTweather202122$Plot <= 24,
                                       HourlyIRTweather202122$Tav + b_gb, HourlyIRTweather202122$Tav + b_sc)

HourlyIRTweather202122$VPG <- 0.6108 * exp(17.27 * HourlyIRTweather202122$Tav/(HourlyIRTweather202122$Tav + 237.3)) -
  0.6108 * exp(17.27 * HourlyIRTweather202122$Tav_b/(HourlyIRTweather202122$Tav_b + 237.3))

# Calculate dTll and dTul
HourlyIRTweather202122$dTll <- ifelse(HourlyIRTweather202122$Plot >= 13 & 
                                        HourlyIRTweather202122$Plot <= 24, a_gb * HourlyIRTweather202122$VPD + b_gb, a_sc * HourlyIRTweather202122$VPD + b_sc)
HourlyIRTweather202122$dTul <- ifelse(HourlyIRTweather202122$Plot >= 13 & 
                                        HourlyIRTweather202122$Plot <= 24, a_gb * HourlyIRTweather202122$VPG + b_gb, a_sc * HourlyIRTweather202122$VPG + b_sc)
HourlyIRTweather202122$dTm <- HourlyIRTweather202122$CanopyTemp - HourlyIRTweather202122$AirTemp

# Combine the filtered datasets into a single dataset
write.csv(HourlyIRTweather202122, "VPGGBS2.csv")

# Calculate CWSI
HourlyIRTweather202122$CWSI <- (HourlyIRTweather202122$dTm - HourlyIRTweather202122$dTll)/
  (HourlyIRTweather202122$dTul - HourlyIRTweather202122$dTll)

# Show the range of CWSI
HourlyIRTCWSI202122 <- as.data.frame(HourlyIRTweather202122[HourlyIRTweather202122$CWSI <= 1 & 
                                                   HourlyIRTweather202122$CWSI >= 0, c("Plot","Date", "CWSI", "PM")])

HourlyIRTCWSI202122$Date <- as.Date(HourlyIRTCWSI202122$Date)

DailyIRTCWSI202122 <- aggregate(cbind(CWSI, PM) ~ Plot + 
                               cut(Date, breaks = "day"), data = HourlyIRTCWSI202122, FUN = mean)
colnames(DailyIRTCWSI202122) <- c("Plot","Date","CWSI", "PM")

DailyIRTCWSI202122$ETc <- (1-DailyIRTCWSI202122$CWSI)*DailyIRTCWSI202122$PM

DailyIRTCWSI202122$Date <- as.Date(DailyIRTCWSI202122$Date)

# Assuming you have a dataframe called DailyCWSI202021 with columns Date, CWSI, SM, and Plot

###measured ET
ETwb <- read.csv("ETwb.csv", header = T)
ETm <- ETwb[,c(2,3,4,5,6,15,17)]
ETm$Date <- as.Date(ETm$Date)
#ETm$Date <- format(ETm$Date, "%m-%d-%Y")
common_dates <- intersect(ETm$Date, DailyIRTCWSI202122$Date);common_dates
#DailyUAVHourlyIRTweather202021ETm <- merge(DailyUAVHourlyIRTweather202021, ETm, by = "Date" & "Plot")
DailySMCWSI202122 <- merge(DailyIRTCWSI202122, ETm, by = c("Date", "Plot"))

library(plyr)
library(tidyr)
DailySMCWSI202122<- na.omit(DailySMCWSI202122)
etstat <- ddply(DailySMCWSI202122%>%
                  mutate(ET_wb=ET_wb)%>%
                  filter(ET_wb>=0),.(Crop,EXPT_YEAR,IRR,Plot),function(df){
                    rsq <- summary(lm(df$ETc~df$ET_wb))$r.squared 
                    mia= hydroGOF::md(df$ET_wb, df$ETc)
                    nse <- hydroGOF::NSE(df$ET_wb, df$ETc)
                    mae <- hydroGOF::mae(df$ET_wb, df$ETc)
                    rmse1 <- hydroGOF::rmse(df$ET_wb, df$ETc)
                    bias1 <- hydroGOF::pbias(df$ET_wb, df$ETc)
                    d<- hydroGOF::d.data.frame(df$ET_wb, df$ETc)
                    m.details <- c(rsq,nse,mae,rmse1,mia,bias1,d)
                    names(m.details) <- c("Rsq","NSE","MAE",'RMSE','MIA','BIAS','D')
                    return(m.details)  
                  })
etstat
plot_names <- c("25%", "50%", "75%", "100%")

p1 <- DailySMCWSI202122 %>%
  filter(Crop %in% "Green beans") %>%
  filter(EXPT_YEAR %in% "2021-2022") %>%
  filter(Plot %in% c("21","23","22","24"))%>%
  gather(ET_source, Obs_Sim, c('ET_wb', 'ETc'))%>%
  mutate(Plot = factor(Plot, levels = c("21","23","22","24"))) %>%
  ggplot(., aes(DAS, Obs_Sim, color = ET_source, group = ET_source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ Plot, scales = 'free', labeller = labeller(Plot = setNames(plot_names, c("21","23","22","24")))) +
  labs(y = "Evapotranspiration (mm"~~day^-1*")", x = "Days after planting (DAP)") +
  ylim(0, 5) + theme_bw() +
  theme(legend.position = "top", legend.title = element_blank()) +
  theme(axis.title.y = element_text(color = "black", size = 13, angle = 90, vjust = 0.3)) +
  theme(text = element_text(color = "black", size = 13)) +
  theme(axis.text.x = element_text(color = "black", size = 13, angle = 0))

p1

ggsave("p1.png", plot = p1, width = 8, height = 6, dpi = 300)

p2 <- DailySMCWSI202122 %>%
  filter(Crop %in% "Sweet corn") %>%
  filter(EXPT_YEAR %in% "2021-2022") %>%
  filter(Plot %in% c("6","11","2", "12"))%>%
  gather(ET_source, Obs_Sim, c('ET_wb', 'ETc'))%>%
  mutate(Plot = factor(Plot, levels = c("6","11","2", "12"))) %>%
  ggplot(., aes(DAS, Obs_Sim, color = ET_source, group = ET_source)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ Plot, scales = 'free', labeller = labeller(Plot = setNames(plot_names, c("6","11","2", "12")))) +
  labs(y = "Evapotranspiration (mm"~~day^-1*")", x = "Days after planting (DAP)") +
  ylim(0, 5) + theme_bw() +
  theme(legend.position = "top", legend.title = element_blank()) +
  theme(axis.title.y = element_text(color = "black", size = 13, angle = 90, vjust = 0.3)) +
  theme(text = element_text(color = "black", size = 13)) +
  theme(axis.text.x = element_text(color = "black", size = 13, angle = 0))

p2
ggsave("p2.png", plot = p2, width = 8, height = 6, dpi = 300)

DailySM <- read.csv("DailySM.csv", header = T)

DailySM$Date <- as.Date(DailySM$Date)
common_dates <- intersect(DailyIRTCWSI202122$Date,DailySM$Date);common_dates

DailySMCWSI202122 <- inner_join(DailyIRTCWSI202122, DailySM, by = c("Plot", "Date"))

plot1 <- ggplot(DailySMCWSI202122, aes(x = Date)) +
  geom_line(aes(y = CWSI), color = "blue") + # Plot CWSI
  geom_line(aes(y = SM), color = "red") + # Plot SM
  #geom_line(aes(y = IRRmm), color = "brown") + # Plot SM
  #geom_line(aes(y = DailySMCWSI202021$Rain), color = "green") + # Plot SM
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + # Customize x-axis
  labs(x = "Date", y = NULL) +  # Axis labels (y-axis labels will be added dynamically)
  facet_wrap(~ Plot, scales = "free_x") + # Facet by Plot with independent x-axis scales
  theme_minimal();plot1

# Plotting
CWSIData$Date <- as.Date(CWSIData$Date)
plot1 <- ggplot(CWSIData, aes(x = Date, y = CWSI)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + # Customize x-axis
  labs(x = "Date", y = "Variable") +  # Axis labels
  facet_wrap(~ Plot, scales = "free_x") + # Facet by month
  theme_minimal()
plot1


################
# Sweet corn
# a_sc <- -3.46
# b_sc <- 0.54
# r2 = 0.74
# 
# # Green beans
# a_gb <- -7.61
# b_gb <- 3.56
# 
# # Calculate Tav_b
# DailyIRTweather202122$Tav_b <- ifelse(DailyIRTweather202122$Plot >= 13 & 
#                                         DailyIRTweather202122$Plot <= 24,
#                                       DailyIRTweather202122$Tav + b_gb, DailyIRTweather202122$Tav + b_sc)
# 
# DailyIRTweather202122$VPG <- 0.6108 * exp(17.27 * DailyIRTweather202122$Tav/(DailyIRTweather202122$Tav + 237.3)) -
#   0.6108 * exp(17.27 * DailyIRTweather202122$Tav_b/(DailyIRTweather202122$Tav_b + 237.3))
# 
# # Calculate dTll and dTul
# DailyIRTweather202122$dTll <- ifelse(DailyIRTweather202122$Plot >= 13 & 
#                                        DailyIRTweather202122$Plot <= 24, a_gb * DailyIRTweather202122$VPD + b_gb, a_sc * DailyIRTweather202122$VPD + b_sc)
# DailyIRTweather202122$dTul <- ifelse(DailyIRTweather202122$Plot >= 13 & 
#                                        DailyIRTweather202122$Plot <= 24, a_gb * DailyIRTweather202122$VPG + b_gb, a_sc * DailyIRTweather202122$VPG + b_sc)
# DailyIRTweather202122$dTm <- DailyIRTweather202122$CanopyTemp - DailyIRTweather202122$AirTemp
# 
# # Calculate CWSI
# DailyIRTweather202122$CWSI <- (DailyIRTweather202122$dTm - DailyIRTweather202122$dTll)/
#   (DailyIRTweather202122$dTul - DailyIRTweather202122$dTll)
# 
# # Show the range of CWSI
# CWSIData <- as.data.frame(DailyIRTweather202122[DailyIRTweather202122$CWSI <= 1 & 
#                                                   DailyIRTweather202122$CWSI >= 0, c("Plot","Date", "CWSI")])

######################
#DailyIRTweather202122 <- aggregate(cbind(CanopyTemp, AirTemp, VPD, Tav, Tav_b) ~ Plot + 
#                                     cut(Date, breaks = "day"), data = HourlyIRTweather202122, FUN = mean)
#colnames(DailyIRTweather202122) <- c("Plot","Date",'CanopyTemp', 'AirTemp', 'VPD', 'Tav', 'Tav_b')
