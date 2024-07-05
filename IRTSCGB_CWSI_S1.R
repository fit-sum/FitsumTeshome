
library(dplyr)
#setwd("C:/Users/17867/OneDrive - University of Florida/Desktop/Crop water stress indices")
setwd("C:/Users/fteshome/OneDrive - University of Florida/Desktop/Crop water stress indices")

#CanopyTemp	AirTemp	Plot	Date	Time
IRT202021 <- read.csv("20_21_processed_ground_data.csv") 

#Combine date and time columns into a single datetime column
IRT202021$DateTime <- as.POSIXct(paste(IRT202021$Date, IRT202021$Time), format = "%d/%m/%Y %H:%M:%S")
IRT202021$AirTemp <- as.numeric(IRT202021$AirTemp)

# Assuming you have a column named "Plot" indicating the plot number
HourlyIRT202021 <- aggregate(cbind(AirTemp, CanopyTemp) ~ Plot + 
                               cut(DateTime, breaks = "hour"), data = IRT202021, FUN = mean)
colnames(HourlyIRT202021) <- c("Plot","Date","AirTemp","CanopyTemp")

###################
S1Chourlyweather <- read.csv("S1Chourlyweatherc.csv", header = T)
#write.csv(S1Chourlyweather, "S1Chourlyweatherc.csv")
common_dates <- intersect(S1Chourlyweather$Date, HourlyIRT202021$Date);common_dates
HourlyIRTweather202021 <- merge(HourlyIRT202021, S1Chourlyweather, by = c("Date"))

HourlyIRTweather202021$Elev <- 2

library(lubridate)
#HourlyIRTweather202021$Tmax <- as.numeric(HourlyIRTweather202021$Tmax)
#HourlyIRTweather202021$RHmax <- as.numeric(HourlyIRTweather202021$RHmax)
#HourlyIRTweather202021$RHav <- (HourlyIRTweather202021$RHmin+HourlyIRTweather202021$RHmax)/2

###########
#HourlyIRTweather202021$Date <- as.POSIXct(HourlyIRTweather202021$Date, format = "%d/%m/%Y")
#HourlyIRTweather202021$Date <- mdy(HourlyIRTweather202021$Date)
#HourlyIRTweather202021$Tav <- (HourlyIRTweather202021$Tmax+HourlyIRTweather202021$Tmin)/2
HourlyIRTweather202021$Date <- as.POSIXct(as.character(HourlyIRTweather202021$Date), format = "%Y-%m-%d %H:%M:%S")
HourlyIRTweather202021$DOY <- yday(HourlyIRTweather202021$Date)
#HourlyIRTweather202021$ws <- HourlyIRTweather202021$Wsav*4.87/(log2(67.8*10-5.42))
HourlyIRTweather202021$ws <- HourlyIRTweather202021$Wsav

#25.53N 
HourlyIRTweather202021$eotmin <- 0.6108*exp((17.27*HourlyIRTweather202021$Tmin)/(HourlyIRTweather202021$Tmin+237.3))
HourlyIRTweather202021$eotmax <- 0.6108*exp((17.27*HourlyIRTweather202021$Tmax)/(HourlyIRTweather202021$Tmax+237.3))
HourlyIRTweather202021$es <- (HourlyIRTweather202021$eotmax+HourlyIRTweather202021$eotmin)/2
#s2cweather$ea <- ((s2cweather$eotmin*s2cweather$RHmax/100)+(s2cweather$eotmax*s2cweather$RHmin/100))/2
range(HourlyIRTweather202021$es)

HourlyIRTweather202021$eatmin <- 0.6108*exp(17.27*HourlyIRTweather202021$Tmin/(HourlyIRTweather202021$Tmin+237.3))
HourlyIRTweather202021$eatmax <- 0.6108*exp(17.27*HourlyIRTweather202021$Tmax/(HourlyIRTweather202021$Tmax+237.3))
HourlyIRTweather202021$ea <- (HourlyIRTweather202021$RH/100)*((HourlyIRTweather202021$eatmin + HourlyIRTweather202021$eatmax)/2)
range(HourlyIRTweather202021$ea)
HourlyIRTweather202021$VPD <- HourlyIRTweather202021$es-HourlyIRTweather202021$ea
range(HourlyIRTweather202021$VPD)

HourlyIRTweather202021$delta <- 4098*((0.6108*exp((17.27*HourlyIRTweather202021$Tav)/(HourlyIRTweather202021$Tav+237.3))))/
  ((HourlyIRTweather202021$Tav+237.3)^2)
HourlyIRTweather202021$p <- 101.3*((293-0.0065*HourlyIRTweather202021$Elev)/293)^5.26
HourlyIRTweather202021$y <- HourlyIRTweather202021$p*0.000665  #psychrometeric constant
HourlyIRTweather202021$dr <- 1+0.033*cos(2*pi*HourlyIRTweather202021$DOY/365)
HourlyIRTweather202021$d1c <-  0.409*(sin((2*pi*HourlyIRTweather202021$DOY/365)-1.39))
HourlyIRTweather202021$lat_rad <- 25.53

HourlyIRTweather202021$ws1 <- acos(-tan(pi*HourlyIRTweather202021$lat_rad/180)*tan(HourlyIRTweather202021$d1c))
HourlyIRTweather202021$ra <-  24*60*0.082*HourlyIRTweather202021$dr*(HourlyIRTweather202021$ws1*sin(pi*HourlyIRTweather202021$lat_rad/180)*
                                                                       sin(HourlyIRTweather202021$d1c)+cos(pi*HourlyIRTweather202021$lat_rad/180)*
                                                                       cos(HourlyIRTweather202021$lat_rad)*sin(HourlyIRTweather202021$ws1))/pi
HourlyIRTweather202021$rso <- (0.75+(2*10^-5)*HourlyIRTweather202021$Elev)*HourlyIRTweather202021$ra

HourlyIRTweather202021$RsdRso <- HourlyIRTweather202021$SRad/HourlyIRTweather202021$rso

HourlyIRTweather202021$rns <-  (1-0.23)*HourlyIRTweather202021$SRad

HourlyIRTweather202021$rnl <- 4.903*10^-9*(((HourlyIRTweather202021$Tmax+273.16)^4+(HourlyIRTweather202021$Tmin+273.16)^4)/2)*(0.34-0.14*sqrt(HourlyIRTweather202021$ea))*((1.35*HourlyIRTweather202021$SRad/HourlyIRTweather202021$rso)-0.35)

HourlyIRTweather202021$rn <- HourlyIRTweather202021$rns-HourlyIRTweather202021$rnl

HourlyIRTweather202021$PM <- (0.408*HourlyIRTweather202021$delta*HourlyIRTweather202021$rn+HourlyIRTweather202021$y*(900/(HourlyIRTweather202021$Tav+273))*HourlyIRTweather202021$ws*(HourlyIRTweather202021$es-HourlyIRTweather202021$ea))/(HourlyIRTweather202021$delta+HourlyIRTweather202021$y*(1+0.34*HourlyIRTweather202021$ws))

# Convert datetime column to POSIXct format
datetime_posix <- as.POSIXct(HourlyIRTweather202021$Date, format = "%Y-%m-%d %H")

HourlyIRTweather202021$dtll <- HourlyIRTweather202021$CanopyTemp -HourlyIRTweather202021$AirTemp

write.csv(HourlyIRTweather202021,"HourlyIRTweather202021.csv")

# Extract date and time components
HourlyIRTweather202021$datec <- format(datetime_posix, "%Y-%m-%d")
HourlyIRTweather202021$timec <- format(datetime_posix, "%H:%M:%S")

IrrigS1 <- read.csv("IrrigS1C.csv", header = T)
#write.csv(IrrigS1, "IrrigS1C.csv")
common_dates1 <- intersect(HourlyIRTweather202021$datec, IrrigS1$Date);common_dates1 

Rainyday <- as.data.frame(S1Chourlyweather[S1Chourlyweather$Rain > 2, c("Date", "Rain")])
datetime_posix2 <- as.POSIXct(Rainyday$Date, format = "%Y-%m-%d %H")
Rainyday$Dateonly <- format(datetime_posix2, "%Y-%m-%d")
Rainyday$Time <- format(datetime_posix2, "%H:%M:%S")
common_dates2 <- intersect(HourlyIRTweather202021$datec,Rainyday$Dateonly);common_dates2

combined_dates <- union(common_dates1, common_dates2);combined_dates

# It should one day after irrigation and rainfall events
#datep1 <- c("2020-12-04", "2020-12-10", "2020-12-16", "2020-12-20", "2020-12-24", "2020-12-30", "2021-01-03", 
#"2021-01-07", "2021-01-12", "2021-01-17", "2020-12-07", "2020-12-11", "2020-12-12", "2020-12-25", "2021-01-13", 
#"2021-02-01", "2021-02-06","2021-02-15", "2021-02-16", "2021-02-24")

#datep+1 <- c("2020-12-05", "2020-12-11", "2020-12-17", "2020-12-21", "2020-12-25", "2020-12-31", "2021-01-04", 
#"2021-01-08","2021-01-13", "2021-01-18", "2020-12-08", "2020-12-12", "2020-12-13", "2020-12-26", "2021-01-14", "2021-02-02",
#"2021-02-07", "2021-02-16", "2021-02-17", "2021-02-25")

# full irrigation treatments for green beans and sweet corn
#PlotFI <- c("2","7","12","15","17","24")

# Load the necessary libraries
library(ggplot2)
library(ggpubr)

# Filter data for the specified time range and dates
HourlyIRTweather202021SLD <- HourlyIRTweather202021 %>%
  filter(datec %in% c("2020-12-05", "2020-12-11", "2020-12-17", "2020-12-21", "2020-12-25", "2020-12-31", "2021-01-04", 
                      "2021-01-08","2021-01-13", "2021-01-18", "2020-12-08", "2020-12-12", "2020-12-13", "2020-12-26", "2021-01-14", "2021-02-02",
                      "2021-02-07", "2021-02-16", "2021-02-17", "2021-02-25"))%>%
  filter(format(as.POSIXct(Date), "%H:%M:%S") >= "10:00:00" &
           format(as.POSIXct(Date), "%H:%M:%S") <= "14:00:00")%>% 
  filter(Plot %in% c("2",'3','5',"7",'9',"12","15",'16',"17",'19','22',"24"))%>%filter(RsdRso>= 0.7)

# Create a ggplot for dtll vs VPD
plot202021A <- ggplot(HourlyIRTweather202021SLD, aes(x = VPD, y = dtll)) +
  geom_point() +
  labs(x = "VPD", y = "dtll") +
  theme_minimal()

# Facet the plot by dates and plots
plot202021 <- plot202021A + facet_grid(Plot ~ datec)+
  stat_regline_equation()+
  stat_cor(aes(label = paste(..rr.label..)),
           label.x.npc = "left", label.y.npc = "bottom");plot202021

###################
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
filtered_data <- HourlyIRTweather202021SLD %>%
  filter(datec %in% c("2020-12-05", "2020-12-11", "2020-12-17", "2020-12-21", "2020-12-25", "2020-12-31", "2021-01-04", 
                      "2021-01-08","2021-01-13", "2021-01-18", "2020-12-08", "2020-12-12", "2020-12-13", "2020-12-26", "2021-01-14", "2021-02-02",
                      "2021-02-07", "2021-02-16", "2021-02-17", "2021-02-25")) %>%
  #filter(Plot %in% c("15",'16',"17",'19','22',"24"))
  filter(Plot %in% c("2",'3','5',"7",'9',"12"))

# Group data by date and plot, perform linear regression, and filter by R-squared
filtered_datasets <- filtered_data %>%
  group_split(datec, Plot) %>%
  lapply(filter_by_r_squared) %>%
  Filter(Negate(is.null), .)

# Combine the filtered datasets into a single dataset
combined_dataset <- bind_rows(filtered_datasets)
write.csv(combined_dataset, "SCS1.csv")

#Perform linear regression
lm_model <- lm(dtll ~ VPD, data = combined_dataset)

# Extract coefficients and R-squared value
intercept <- coef(lm_model)[1]
slope <- coef(lm_model)[2]
r_squared <- summary(lm_model)$r.squared

# Create the scatter plot
plot <- ggplot(combined_dataset, aes(x = VPD, y = dtll)) +
  geom_point(color = "black") +  
  #geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +  
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  annotate("text", x = 0.83, y = 2.5,
           label = paste("y =", round(intercept, 3), round(slope, 2), "*x"), hjust = 1) +  
  annotate("text", x = 0.83, y = 2.0,
           label = paste("R² =", round(r_squared, 3)), vjust = 0, hjust = 1) +  
  labs(x = "VPD", y = "dTll") +  
  theme_minimal()+ theme_bw() +
  theme(legend.position = "top", legend.title = element_blank()) +
  theme(axis.title.y = element_text(color = "black", size = 13, angle = 90, vjust = 0.3)) +
  theme(text = element_text(color = "black", size = 13)) +
  theme(axis.text.x = element_text(color = "black", size = 13, angle = 0));plot

ggsave("plotS1VPDGB.png", plot = plot, width = 8, height = 6, dpi = 300)

#################################################################################################
#library(tidyr)
#ggsave("plotGBS1.png", plot = plotGBS1, width = 11, height = 8, dpi = 300)

#-0.869 
#2.85 feet
###################
# Sweet corn
a_sc <- -4.27
b_sc <- 1.78
R2 = 0.68

# Green beans
a_gb <- -5.73
b_gb <- 2.06
R2 = 0.59

# Calculate Tav_b
HourlyIRTweather202021$Tav_b <- ifelse(HourlyIRTweather202021$Plot >= 13 & 
                                         HourlyIRTweather202021$Plot <= 24,
                                       HourlyIRTweather202021$Tav + b_gb, HourlyIRTweather202021$Tav + b_sc)

HourlyIRTweather202021$VPG <- 0.6108 * exp(17.27 * HourlyIRTweather202021$Tav/(HourlyIRTweather202021$Tav + 237.3)) -
  0.6108 * exp(17.27 * HourlyIRTweather202021$Tav_b/(HourlyIRTweather202021$Tav_b + 237.3))

# Calculate dTll and dTul
HourlyIRTweather202021$dTll <- ifelse(HourlyIRTweather202021$Plot >= 13 & 
                                        HourlyIRTweather202021$Plot <= 24, a_gb * HourlyIRTweather202021$VPD + b_gb, a_sc * HourlyIRTweather202021$VPD + b_sc)
HourlyIRTweather202021$dTul <- ifelse(HourlyIRTweather202021$Plot >= 13 & 
                                        HourlyIRTweather202021$Plot <= 24, a_gb * HourlyIRTweather202021$VPG + b_gb, a_sc * HourlyIRTweather202021$VPG + b_sc)
HourlyIRTweather202021$dTm <- HourlyIRTweather202021$CanopyTemp - HourlyIRTweather202021$AirTemp

# Combine the filtered datasets into a single dataset
write.csv(HourlyIRTweather202021, "VPGSCS1.csv")

# Calculate CWSI
HourlyIRTweather202021$CWSI <- (HourlyIRTweather202021$dTm - HourlyIRTweather202021$dTll)/
  (HourlyIRTweather202021$dTul - HourlyIRTweather202021$dTll)

# Show the range of CWSI
HourlyIRTweather202021 <- as.data.frame(HourlyIRTweather202021[HourlyIRTweather202021$CWSI <= 1 & 
                                                   HourlyIRTweather202021$CWSI >= 0, c("Plot","Date","PM", "CWSI")])

HourlyIRTweather202021$Date <- as.Date(HourlyIRTweather202021$Date)

DailyIRTweather202021 <- aggregate(cbind(CWSI, PM) ~ Plot + 
                                      cut(Date, breaks = "day"), data = HourlyIRTweather202021, FUN = mean)

colnames(DailyIRTweather202021) <- c("Plot","Date","CWSI", "PM")

DailyIRTweather202021$Date <- as.Date(DailyIRTweather202021$Date)

DailyIRTweather202021$ETc <- (1-DailyIRTweather202021$CWSI)*DailyIRTweather202021$PM

###measured ET
ETwb <- read.csv("ETwb.csv", header = T)
ETm <- ETwb[,c(2,3,4,5,6,15,17)]
ETm$Date <- as.Date(ETm$Date)
#ETm$Date <- format(ETm$Date, "%m-%d-%Y")
common_dates <- intersect(ETm$Date, DailyIRTweather202021$Date);common_dates
#DailyUAVHourlyIRTweather202021ETm <- merge(DailyUAVHourlyIRTweather202021, ETm, by = "Date" & "Plot")
DailyIRTweather202021 <- merge(DailyIRTweather202021, ETm, by = c("Date", "Plot"))

library(plyr)
library(tidyr)
DailyIRTweather202021<- na.omit(DailyIRTweather202021)
etstat <- ddply(DailyIRTweather202021%>%
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

p1 <- DailyIRTweather202021 %>%
  filter(Crop %in% "Green beans") %>%
  filter(EXPT_YEAR %in% "2020-2021") %>%
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

p2 <- DailyIRTweather202021 %>%
  filter(Crop %in% "Sweet corn") %>%
  filter(EXPT_YEAR %in% "2020-2021") %>%
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

############
HourlyIRTweather202021<- read.csv("HourlyIRTweather202021.csv", header = T)
HourlyIRTweather202021$Date <- format(HourlyIRTweather202021$Date, format = "%m-%d-%y")
HourlyIRTweather202021$Elev <- 2

# Assuming you have a dataframe called DailyCWSI202021 with columns Date, CWSI, SM, and Plot

DailySM <- read.csv("DailySM.csv", header = T)
DailySM$Date <- as.Date(DailySM$Date)
common_dates <- intersect(DailyCWSI202021$Date,DailySM$Date);common_dates

DailySMCWSI202021 <- inner_join(DailyCWSI202021, DailySM, by = c("Plot", "Date"))

plot1 <- ggplot(DailySMCWSI202021, aes(x = Date)) +
  geom_line(aes(y = CWSI), color = "blue") + # Plot CWSI
  geom_line(aes(y = SM), color = "red") + # Plot SM
  #geom_line(aes(y = IRRmm), color = "brown") + # Plot SM
  #geom_line(aes(y = DailySMCWSI202021$Rain), color = "green") + # Plot SM
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + # Customize x-axis
  labs(x = "Date", y = NULL) +  # Axis labels (y-axis labels will be added dynamically)
  facet_wrap(~ Plot, scales = "free_x") + # Facet by Plot with independent x-axis scales
  theme_minimal();plot1



##########################################
# DailyIRTweather202021 <- aggregate(cbind(CanopyTemp, AirTemp, VPD, Tav, Tav_b) ~ Plot + 
#                                      cut(Date, breaks = "day"), data = HourlyIRTweather202122, FUN = mean)
# colnames(DailyIRTweather202021) <- c("Plot","Date",'CanopyTemp', 'AirTemp', 'VPD', 'Tav', 'Tav_b')
# 
# #sweet corn
# a_sc= -4.24
# b_sc = 1.83
# r2 = 0.65
# 
# #green bean
# a_gb= -5.73
# b_gb = 2.13
# r2 = 0.58
# 
# # Calculate Tav_b
# 
# DailyIRTweather202021$Tav_b <- ifelse(DailyIRTweather202021$Plot >= 13 & 
#                                          DailyIRTweather202021$Plot <= 24,
#                                          DailyIRTweather202021$Tav + b_gb, DailyIRTweather202021$Tav + b_sc)
# 
# DailyIRTweather202021$VPG <- 0.6108 * exp(17.27 * DailyIRTweather202021$Tav/(DailyIRTweather202021$Tav + 237.3)) -
#   0.6108 * exp(17.27 * DailyIRTweather202021$Tav_b/(DailyIRTweather202021$Tav_b + 237.3))
# 
# # Calculate dTll and dTul
# DailyIRTweather202021$dTll <- ifelse(DailyIRTweather202021$Plot >= 13 & 
#                                        DailyIRTweather202021$Plot <= 24, a_gb * 
#                                        DailyIRTweather202021$VPD + b_gb, a_sc * DailyIRTweather202021$VPD + b_sc)
# DailyIRTweather202021$dTul <- ifelse(DailyIRTweather202021$Plot >= 13 & 
#                                        DailyIRTweather202021$Plot <= 24, a_gb * 
#                                        DailyIRTweather202021$VPG + b_gb, a_sc * DailyIRTweather202021$VPG + b_sc)
# DailyIRTweather202021$dTm <- DailyIRTweather202021$CanopyTemp - DailyIRTweather202021$AirTemp
# 
# # Calculate CWSI
# DailyIRTweather202021$CWSI <- (DailyIRTweather202021$dTm - DailyIRTweather202021$dTll)/
#   (DailyIRTweather202021$dTul - DailyIRTweather202021$dTll)
# 
# # Show the range of CWSI
# CWSIData <- as.data.frame(DailyIRTweather202021[DailyIRTweather202021$CWSI <= 1 & 
#                                                   DailyIRTweather202021$CWSI >= 0, c("Plot","Date", "CWSI")])

