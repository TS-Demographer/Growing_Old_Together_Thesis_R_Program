# This is the R program for 'Growing Old Together? The Sex Gap in Population Ageing'

## Please enter your Human Mortality Database username and password below: ####
username.hmd = ""
password.hmd = ""
# Please select a country for the variable-r decomposition by assigning the 
# "Select_country" variable a number corresponding to one of the following countries:
# 1-NOR, 2-CHE, 3-GBRTENW, 4-GBR_SCO, 5-NLD, 6-SWE, 7-FIN, 8-DNK, 9-FRATNP, 10-ESP, 11-ITA
Select_country <- 1

# No other changes to the program are required.
# Please note that this program will take a few minutes to run.

# Part 1: Population mean age and life table mean age analysis for 4 countries ####
### Setup ####
library("dplyr")
library("ggplot2")
library("tidyr")
library("ggrepel")
library("HMDHFDplus")
library("zoo")

Names<-c("SWE","ESP", "USA", "AUS")
Fullnames<-c("Sweden","Spain", "United States", "Australia")

Result_mean_F<-data.frame(matrix(0,nrow=0, ncol=4))
Result_mean_M<-data.frame(matrix(0,nrow=0, ncol=4))
colnames(Result_mean_F) <- c("Country", "Year", "Pop_mean", "LT_mean")
colnames(Result_mean_M) <- c("Country", "Year", "Pop_mean", "LT_mean")

age_decomp_means_overt<-data.frame(matrix(0,nrow=0, ncol=5))
colnames(age_decomp_means_overt) <- c("Country", "Period", "Age", "Mean", "Contribution")
age_decomp_means_pit<-data.frame(matrix(0,nrow=0, ncol=5))
colnames(age_decomp_means_pit) <- c("Country", "Period", "Age", "Mean", "Contribution")

# select start years for the three periods in figure 3
period_start <- c(1950, 1980, 2010)
# select how long the three periods in figure 3 should be
period_length <- c(10, 10, 10)

#### Functions to be used in analysis ####
change<-function(vt,vth,h){
  r<-log(vth/vt)/h
  vth2<-vt*exp(r*h/2)
  vdot=r*vth2
  return(vdot)
}
midp<-function(vt,vth,h){
  r<-log(vth/vt)/h
  vth2<-vt*exp(r*h/2)
  vth2[is.na(vth2)]<-0
  return(vth2)
}
reldev <- function(vt, vth, h){
  devv <- change(vt, vth, h)
  reldevv <- devv/midp(vt, vth, h)
  return(reldevv)
}
meancal <- function(data,time, sex){
  datares <- data %>% filter(data$Year==time)
  datares <- datares %>% pivot_longer(cols=c(Male,Female,Total), names_to = "Sex", values_to = "Count")
  datares <- datares %>% filter(datares$Sex==sex)
  datares$Age <- as.character(datares$Age)
  datares$Age[datares$Age=="110+"] <- "110"
  datares$Age <- as.numeric(datares$Age)
  meanval <- sum((datares$Age)*(datares$Count))/sum(datares$Count)
  return(meanval)
}
# For the meancal function, select sex as "Male" or "Female" (use the quotation marks)
meanLTcal <- function(data,time){
  datares <- data %>% filter(data$Year==time)
  datares$Age <- as.character(datares$Age)
  datares$Age[datares$Age=="110+"] <- "110"
  datares$Age <- as.numeric(datares$Age)
  meanval <- sum((datares$Age)*(datares$lx))/sum(datares$lx)
  return(meanval)
}

#### Load data ####
# This runs the program for the 4 countries
for (ii in 1:4){
  # First load data on population
  pop_data = readHMDweb(CNTRY = Names[ii], item = "Population", username =
                          username.hmd, password = password.hmd)
  pop_data<-pop_data[,c(1,2,4,5,6)]
  colnames(pop_data)=c("Year", "Age", "Female", "Male", "Total")
  
  # Load life table data
  LT_data_F = readHMDweb(CNTRY = Names[ii], item = "fltper_1x1", username = username.hmd, password = password.hmd)
  LT_data_M = readHMDweb(CNTRY = Names[ii], item = "mltper_1x1", username = username.hmd, password = password.hmd)
  LT_data_F=LT_data_F[,-11]
  LT_data_M=LT_data_M[,-11]
  
  # Fix 110+ formatting as a character
  pop_data$Age <- as.character(pop_data$Age)
  pop_data$Age[pop_data$Age=="110+"] <- "110"
  pop_data$Age <- as.numeric(pop_data$Age)
  
  #### Format data for Figures 1, 2 and A1 #### 
  # Create data frame of mean values for females and males by year
  mean <- cbind.data.frame(min(pop_data$Year):max(pop_data$Year), rep(0, length(unique(pop_data$Year))), rep(0, length(unique(pop_data$Year))))
  colnames(mean)<-c("Year", "Females", "Males")
  for (i in 1:length(unique(pop_data$Year))) {
    singleyear <- pop_data %>% filter(Year==i+min(pop_data$Year))
    mean[i, "Females"] <- sum((singleyear[, "Female"])*(singleyear[, "Age"]))/sum(singleyear[, "Female"])
    mean[i, "Males"] <- sum((singleyear[, "Male"])*(singleyear[, "Age"]))/sum(singleyear[, "Male"])
  }
  mean <- mean %>% mutate(Difference=Females - Males)
  
  LT_mean_F <- data.frame(Year=unique(LT_data_F$Year), Mean=rep(0), Sex=rep("Female"))
  for (i in unique(LT_data_F$Year)){
    data <- LT_data_F[LT_data_F$Year==i,]
    LT_mean_F[(i-min(LT_data_F$Year)+1),2] <- sum((data[,2]+0.5)*data[,6])/sum(data[,6])
  }
  LT_mean_M <- data.frame(Year=unique(LT_data_M$Year), Mean=rep(0), Sex=rep("Male"))
  for (i in unique(LT_data_M$Year)){
    data <- LT_data_M[LT_data_M$Year==i,]
    LT_mean_M[(i-min(LT_data_M$Year)+1),2] <- sum((data[,2]+0.5)*data[,6])/sum(data[,6])
  }
  
  Country_Results_F <- cbind(Names[ii],mean$Year, mean$Females, LT_mean_F$Mean)  
  colnames(Country_Results_F) <- c("Country", "Year", "Pop_mean", "LT_mean")
  Result_mean_F<-rbind(Result_mean_F,Country_Results_F)
  
  Country_Results_M <- cbind(Names[ii],mean$Year, mean$Males, LT_mean_M$Mean)  
  colnames(Country_Results_M) <- c("Country", "Year", "Pop_mean", "LT_mean")
  Result_mean_M<-rbind(Result_mean_M,Country_Results_M)
  
  #### Format data for Figures 3 and A2 ####
  # create a loop for time periods
  for(x in 1:3){
    timet <- period_start[x]
    hvalue <- period_length[x]
    timeth <- timet+hvalue
    
    pop_data_t <- pop_data %>% filter(Year==timet)
    pop_data_th <- pop_data %>% filter(Year==timeth)
    LT_data_t_F <- LT_data_F %>% filter(Year==timet)
    LT_data_t_F <- LT_data_t_F$lx
    LT_data_th_F <- LT_data_F %>% filter(Year==timeth)
    LT_data_th_F <- LT_data_th_F$lx
    LT_data_t_M <- LT_data_M %>% filter(Year==timet)
    LT_data_t_M <- LT_data_t_M$lx
    LT_data_th_M <- LT_data_M %>% filter(Year==timeth)
    LT_data_th_M <- LT_data_th_M$lx
    
    ## to calculate population mean
    # r values
    r_F<-log(pop_data_th$Female/pop_data_t$Female)/hvalue
    r_M<-log(pop_data_th$Male/pop_data_t$Male)/hvalue
    r_F[is.infinite(r_F)]<-0
    r_F[is.nan(r_F)]<-0
    r_M[is.infinite(r_M)]<-0
    r_M[is.nan(r_M)]<-0
    
    # c midpoint values
    c_t_F<-pop_data_t$Female/sum(pop_data_t$Female)
    c_th_F<-pop_data_th$Female/sum(pop_data_th$Female)
    c_midp_F<-midp(c_t_F,c_th_F,hvalue)
    c_t_M<-pop_data_t$Male/sum(pop_data_t$Male)
    c_th_M<-pop_data_th$Male/sum(pop_data_th$Male)
    c_midp_M<-midp(c_t_M,c_th_M,hvalue)
    
    # mean midpoint values
    mean_t_F <- meancal(pop_data, timet, "Female")
    mean_th_F <- meancal(pop_data, timeth, "Female")
    mean_t_M <- meancal(pop_data, timet, "Male")
    mean_th_M <- meancal(pop_data, timeth, "Male")
    
    mean_midp_F <- midp(mean_t_F,mean_th_F,hvalue)
    mean_midp_M <- midp(mean_t_M,mean_th_M,hvalue)
    
    # Find the derivatives of mean age wrt time for females and males
    mean_dev_time_F2 <- sum(r_F*c_midp_F*(pop_data_t$Age-mean_midp_F))
    mean_dev_time_M2 <- sum(r_M*c_midp_M*(pop_data_t$Age-mean_midp_M))
    
    # Take the derivative of these wrt sex
    mean_dev_time_sex2 <- change(mean_dev_time_M2, mean_dev_time_F2,1)
    
    # find age-specific values
    agespec_dev_time_sex2B <- change(r_M*c_midp_M*(pop_data_t$Age-mean_midp_M),r_F*c_midp_F*(pop_data_t$Age-mean_midp_F),1)
    
    ## for life table mean
    # r values
    r_LT_F<-log(LT_data_th_F/LT_data_t_F)/hvalue
    r_LT_M<-log(LT_data_th_M/LT_data_t_M)/hvalue
    r_LT_F[is.infinite(r_LT_F)]<-0
    r_LT_F[is.nan(r_LT_F)]<-0
    r_LT_M[is.infinite(r_LT_M)]<-0
    r_LT_M[is.nan(r_LT_M)]<-0
    
    # c midpoint values
    c_LT_t_F<-LT_data_t_F/sum(LT_data_t_F)
    c_LT_th_F<-LT_data_th_F/sum(LT_data_th_F)
    c_LT_midp_F<-midp(c_LT_t_F,c_LT_th_F,hvalue)
    c_LT_t_M<-LT_data_t_M/sum(LT_data_t_M)
    c_LT_th_M<-LT_data_th_M/sum(LT_data_th_M)
    c_LT_midp_M<-midp(c_LT_t_M,c_LT_th_M,hvalue)
    
    # meanLT midpoint values
    meanLT_t_F <- meanLTcal(LT_data_F, timet)
    meanLT_th_F <- meanLTcal(LT_data_F, timeth)
    meanLT_t_M <- meanLTcal(LT_data_M, timet)
    meanLT_th_M <- meanLTcal(LT_data_M, timeth)
    
    meanLT_midp_F <- midp(meanLT_t_F,meanLT_th_F,hvalue)
    meanLT_midp_M <- midp(meanLT_t_M,meanLT_th_M,hvalue)
    
    # Find the derivatives of meanLT age wrt time for females and males
    meanLT_dev_time_F2 <- sum(r_LT_F*c_LT_midp_F*(pop_data_t$Age-meanLT_midp_F))
    meanLT_dev_time_M2 <- sum(r_LT_M*c_LT_midp_M*(pop_data_t$Age-meanLT_midp_M))
    
    # Take the derivative of these wrt sex
    meanLT_dev_time_sex2 <- change(meanLT_dev_time_M2, meanLT_dev_time_F2,1)
    
    # find age-specific values
    agespec_LT_dev_time_sex2B <- change(r_LT_M*c_LT_midp_M*(pop_data_t$Age-meanLT_midp_M),r_LT_F*c_LT_midp_F*(pop_data_t$Age-meanLT_midp_F),1)
    
    # add to the data frame
    agespec_df <- data.frame(rep(Names[ii], 111), rep(period_start[x], 111), c(0:110), rep("Population mean", 111), agespec_dev_time_sex2B)
    colnames(agespec_df) <- c("Country", "Period", "Age", "Mean", "Contribution")
    agespec_df2 <- data.frame(rep(Names[ii], 111), rep(period_start[x], 111), c(0:110), rep("Life table mean", 111), agespec_LT_dev_time_sex2B)
    colnames(agespec_df2) <- c("Country", "Period", "Age", "Mean", "Contribution")
    
    agespec_df <- rbind(agespec_df, agespec_df2)
    
    age_decomp_means_overt <- rbind(age_decomp_means_overt,agespec_df)
  }
  
  # loop for point in time
  for(x in 1:3){
    timet <- period_start[x]
    
    pop_data_pit <- pop_data %>% filter(Year==timet)
    LT_data_pit_F <- LT_data_F %>% filter(Year==timet)
    LT_data_pit_F <- LT_data_pit_F$lx
    LT_data_pit_M <- LT_data_M %>% filter(Year==timet)
    LT_data_pit_M <- LT_data_pit_M$lx
    
    ## for population mean
    # c values
    c_pit_F<-pop_data_pit$Female/sum(pop_data_pit$Female)
    c_pit_M<-pop_data_pit$Male/sum(pop_data_pit$Male)
    
    ## for life table mean
    # c (life table) values
    c_LT_pit_F<-LT_data_pit_F/sum(LT_data_pit_F)
    c_LT_pit_M<-LT_data_pit_M/sum(LT_data_pit_M)
    
    agespec_dev_pit <- c(0:110)*(c_pit_F-c_pit_M)
    agespec_LT_dev_pit <- c(0:110)*(c_LT_pit_F-c_LT_pit_M)
    
    # add to the data frame
    agespec_pop_pit <- data.frame(rep(Names[ii], 111), rep(period_start[x], 111), c(0:110), rep("Population mean", 111), agespec_dev_pit)
    colnames(agespec_pop_pit) <- c("Country", "Period", "Age", "Mean", "Contribution")
    agespec_lt_pit <- data.frame(rep(Names[ii], 111), rep(period_start[x], 111), c(0:110), rep("Life table mean", 111), agespec_LT_dev_pit)
    colnames(agespec_lt_pit) <- c("Country", "Period", "Age", "Mean", "Contribution")
    
    agespec_pit_df <- rbind(agespec_pop_pit, agespec_lt_pit)
    
    age_decomp_means_pit <- rbind(age_decomp_means_pit,agespec_pit_df)
  }
}

### Figures ####
#### Figure 1 prep ####
Result_mean_both <- as.data.frame(cbind(Result_mean_F$Country, Result_mean_F$Year, Result_mean_F$Pop_mean, Result_mean_M$Pop_mean))
colnames(Result_mean_both) <- c("Country", "Year", "Female", "Male")

Result_mean_both_long <- pivot_longer(Result_mean_both, cols=c(Female, Male), names_to = "Sex", values_to = "Mean")
Result_mean_both_long$Year<-as.numeric(Result_mean_both_long$Year)
Result_mean_both_long$Mean<-as.numeric(Result_mean_both_long$Mean)

##### Figure 1 ####
Figure_1<-ggplot(data=Result_mean_both_long, aes(x=Year, y=Mean, group=interaction(Country,Sex)))+
  geom_line(aes(linetype=Sex, color=Country), size=0.8)+
  scale_colour_manual(values=c("paleturquoise3", "plum3", "palevioletred", "darkolivegreen3"), labels=c("Australia", "Spain", "Sweden", "United States"))+
  xlim(1900,2023)+
  ylab("Mean age in the population")+
  #facet_wrap(Country~.)+
  theme_light()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5), legend.position = "bottom") + 
  #labs(color="")+
  ggtitle(paste("Population mean age by year, sex and country"))
Figure_1

#### Figure 2 prep ####
Result_mean_M$Year<-as.numeric(Result_mean_M$Year)
Result_mean_F$Year<-as.numeric(Result_mean_F$Year)
Result_mean_M$Pop_mean<-as.numeric(Result_mean_M$Pop_mean)
Result_mean_F$Pop_mean<-as.numeric(Result_mean_F$Pop_mean)
Result_mean_M$LT_mean<-as.numeric(Result_mean_M$LT_mean)
Result_mean_F$LT_mean<-as.numeric(Result_mean_F$LT_mean)

Result_mean_comparison <- as.data.frame(cbind(Result_mean_F$Country, Result_mean_F$Year, Result_mean_F$Pop_mean-Result_mean_M$Pop_mean, Result_mean_F$LT_mean- Result_mean_M$LT_mean))
colnames(Result_mean_comparison) <- c("Country", "Year", "Pop_mean_dif", "LT_mean_dif")

Result_mean_comparison_long <- pivot_longer(Result_mean_comparison, cols=c(Pop_mean_dif, LT_mean_dif), names_to = "Mean_measure", values_to = "Difference")

Result_mean_comparison_long$Country<-as.character(Result_mean_comparison_long$Country)
Result_mean_comparison_long[Result_mean_comparison_long == "US"] <- "USA"
Result_mean_comparison_long$Country<-as.factor(Result_mean_comparison_long$Country)
Result_mean_comparison_long$Country <- factor(Result_mean_comparison_long$Country, levels = c("SWE","ESP","USA","AUS"))

Result_mean_comparison_long$Year<-as.numeric(Result_mean_comparison_long$Year)
Result_mean_comparison_long$Difference<-as.numeric(Result_mean_comparison_long$Difference)

Result_mean_comparison_long$Country<-as.character(Result_mean_comparison_long$Country)
Result_mean_comparison_long[Result_mean_comparison_long == "AUS"] <- "Australia"
Result_mean_comparison_long[Result_mean_comparison_long == "ESP"] <- "Spain"
Result_mean_comparison_long[Result_mean_comparison_long == "SWE"] <- "Sweden"
Result_mean_comparison_long[Result_mean_comparison_long == "USA"] <- "United States"
Result_mean_comparison_long$Country<-as.factor(Result_mean_comparison_long$Country)

##### Figure 2 ####
Figure_2<-ggplot(data=Result_mean_comparison_long, aes(x=Year, y=Difference, group=Mean_measure))+
  geom_line(aes(color=Mean_measure), size=0.8)+
  xlim(1900,2023)+
  scale_y_continuous(expand = c(0,0))+
  geom_hline(yintercept=0, color="grey")+
  geom_vline(xintercept=1950, color="paleturquoise3")+
  geom_vline(xintercept=1980, color="plum3")+
  geom_vline(xintercept=2010, color="darkolivegreen3")+
  annotate('rect', xmin=1950, xmax=1960, ymin=-1, ymax=4.3, alpha=.2, fill='paleturquoise3')+
  annotate('rect', xmin=1980, xmax=1990, ymin=-1, ymax=4.3, alpha=.2, fill='plum3')+
  annotate('rect', xmin=2010, xmax=2020, ymin=-1, ymax=4.3, alpha=.2, fill='darkolivegreen3')+
  ylab("Sex gap (female - male)")+
  facet_wrap(Country~.)+
  theme_light()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5), legend.position = "bottom") + 
  labs(color="")+
  scale_color_manual(values=c("forestgreen", "palevioletred"),labels = c("Life table mean", "Population mean"))+
  ggtitle(paste("Sex gap in population and life table mean ages by year and country"))
Figure_2

#### Figure 3 prep ####
age_decomp_means_pit$Country<-as.character(age_decomp_means_pit$Country)
age_decomp_means_pit[age_decomp_means_pit == "AUS"] <- "Australia"
age_decomp_means_pit[age_decomp_means_pit == "ESP"] <- "Spain"
age_decomp_means_pit[age_decomp_means_pit == "SWE"] <- "Sweden"
age_decomp_means_pit[age_decomp_means_pit == "USA"] <- "United States"
age_decomp_means_pit$Country<-as.factor(age_decomp_means_pit$Country)

age_decomp_means_pit$Period<-as.factor(age_decomp_means_pit$Period)
age_decomp_means_pit <- age_decomp_means_pit %>% rename("Year" = Period)

##### Figure 3 ####
Figure_3 <- ggplot(data=age_decomp_means_pit, aes(x=Age, y=Contribution, group=interaction(Year,Mean)))+
  geom_line(aes(color=Year), size=0.8)+  
  facet_wrap(factor(Mean,levels=c("Population mean", "Life table mean"))~Country, ncol=4)+
  theme_light()+
  ylab("Age-specific contribution to sex gap")+
  scale_colour_manual(values=c("paleturquoise3", "plum3", "darkolivegreen3"))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5), legend.position = "bottom") + 
  theme(legend.position = "bottom") + 
  ggtitle("Age-specific contributions to the sex gap in population and life table mean ages", subtitle = "by time point and country")
Figure_3

#### Figure A1 prep ####
Result_mean_comparison_long <- arrange(Result_mean_comparison_long, by_group=Mean_measure)
Result_mean_comparison_long_new <- data.frame(matrix(0,nrow=0, ncol=6))
colnames(Result_mean_comparison_long_new) <- c("Country", "Year", "Mean_measure", "Difference", "Derivative", "Rollingmean")

for (i in 1:4){
  Result_mean_comparison_long_country <- filter(Result_mean_comparison_long, Country==Fullnames[i])
  Result_mean_comparison_long_pop <- filter(Result_mean_comparison_long_country, Mean_measure=="Pop_mean_dif")
  Result_mean_comparison_long_LT <- filter(Result_mean_comparison_long_country, Mean_measure=="LT_mean_dif")
  
  Result_mean_comparison_long_pop$Derivative <- change(Result_mean_comparison_long_pop$Difference,c(Result_mean_comparison_long_pop$Difference[-c(1:10)], rep(NA, 10)), 10)
  Result_mean_comparison_long_LT$Derivative <- change(Result_mean_comparison_long_LT$Difference,c(Result_mean_comparison_long_LT$Difference[-c(1:10)], rep(NA, 10)), 10)
  Result_mean_comparison_long_pop$Rollingmean <- c(NA,NA, rollmean(Result_mean_comparison_long_pop$Derivative, 5), NA,NA)
  Result_mean_comparison_long_LT$Rollingmean <- c(NA,NA, rollmean(Result_mean_comparison_long_LT$Derivative, 5), NA,NA)
  
  Result_mean_comparison_long_new <- rbind(Result_mean_comparison_long_new, Result_mean_comparison_long_pop, Result_mean_comparison_long_LT)
}

Result_mean_comparison_long_new$Year<-Result_mean_comparison_long_new$Year+10

##### Figure A1 ####
Figure_A1<-ggplot(data=Result_mean_comparison_long_new, aes(x=Year, y=Rollingmean, group=Mean_measure))+
  geom_line(aes(color=Mean_measure), size=0.8)+
  geom_hline(yintercept = 0, colour="gray")+
  xlim(1900,2023)+
  ylim(-0.25, 0.25)+
  geom_vline(xintercept=1950, color="paleturquoise3")+
  geom_vline(xintercept=1980, color="plum3")+
  geom_vline(xintercept=2010, color="darkolivegreen3")+
  ylab("Derivative of sex gap")+
  facet_wrap(Country~.)+
  theme_light()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5), legend.position = "bottom") +
  labs(color="")+
  scale_color_manual(values=c("forestgreen", "palevioletred"),labels = c("Life table mean", "Population mean"))+
  ggtitle("Derivative of the sex gap in population and life table mean ages by year and country", subtitle="10-year derivative, 5-year rolling average")
Figure_A1

#### Figure A2 prep ####
age_decomp_means_overt$Country<-as.character(age_decomp_means_overt$Country)
age_decomp_means_overt[age_decomp_means_overt == "AUS"] <- "Australia"
age_decomp_means_overt[age_decomp_means_overt == "ESP"] <- "Spain"
age_decomp_means_overt[age_decomp_means_overt == "SWE"] <- "Sweden"
age_decomp_means_overt[age_decomp_means_overt == "USA"] <- "United States"
age_decomp_means_overt$Country<-as.factor(age_decomp_means_overt$Country)

age_decomp_means_overt$Period<-as.factor(age_decomp_means_overt$Period)

##### Figure A2 ####
Figure_A2 <- ggplot(data=age_decomp_means_overt, aes(x=Age, y=Contribution, group=interaction(Period,Mean)))+
  geom_line(aes(color=Period), size=0.8)+
  facet_wrap(factor(Mean,levels=c("Population mean", "Life table mean"))~Country, ncol=4)+
  theme_light()+
  ylab("Age-specific contribution to sex gap")+
  scale_colour_manual(values=c("paleturquoise3", "plum3", "darkolivegreen3"),labels = c("1950-60", "1980-90","2010-20"))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5), legend.position = "bottom") + 
  theme(legend.position = "bottom") + 
  ggtitle("Age-specific contributions to the change in sex gap in population and life table mean ages", sub="by 10-year time period and country")
Figure_A2

# Part 2: Variable-r decomposition for selected country ####

ResultF<-data.frame(matrix(0,nrow=11, ncol=7))
ResultM<-data.frame(matrix(0,nrow=11, ncol=7))
colnames(ResultF) <- c("Country","meantF","meanthF","meandotF", "BirthsF", "SurvivorshipF", "MigrationF")
colnames(ResultM) <- c("Country","meantM","meanthM","meandotM", "BirthsM", "SurvivorshipM", "MigrationM")

### Setup ####

#### Load data ####
Names<-c("NOR", "CHE", "GBRTENW", "GBR_SCO", "NLD", "SWE", "FIN", "DNK", "FRATNP", "ESP", "ITA")
# First load data on population
popdata = readHMDweb(CNTRY = Names[Select_country], item = "Population", username =
                       username.hmd, password = password.hmd)
popdata<-popdata[,c(1,2,4,5,6)]
colnames(popdata)=c("Year", "Age", "Female", "Male", "Total")

# Now load births data
birthsdata = readHMDweb(CNTRY = Names[Select_country], item = "Births", username =
                          username.hmd, password = password.hmd)

# Load deaths data
deathsdata = readHMDweb(CNTRY = Names[Select_country], item = "Mx_1x1", username =
                          username.hmd, password = password.hmd)
deathsdata=deathsdata[,-6]

# Fix 110+ formatting as a character
popdata$Age <- as.character(popdata$Age)
popdata$Age[popdata$Age=="110+"] <- "110"
popdata$Age <- as.numeric(popdata$Age)
deathsdata$Age <- as.character(deathsdata$Age)
deathsdata$Age[deathsdata$Age=="110+"] <- "110"
deathsdata$Age <- as.numeric(deathsdata$Age)

if(Names[Select_country]=="DNK"){
  popdata$Year <- as.character(popdata$Year)
  popdata$Year[popdata$Year=="1921+"] <- "1921"
  popdata$Year <- as.numeric(popdata$Year)
  popdata=popdata[popdata$Year !="1921-",]
}

#### Decompose with respect to time for females #### 
# This sets years t and t+h as 2010 and 2020, respectively
hvalue <- 10
timeth <- 2020
timet <- timeth-hvalue

# This restricts the data to the years we are concerned with
popdata2<-popdata[(as.numeric(as.character(popdata$Year))<(timeth+1))&(as.numeric(as.character(popdata$Year))>(timet-101)),]
popdata1<-na.omit(popdata2[as.numeric(as.character(popdata2$Age))<101,])
birthsdata1<-birthsdata[(birthsdata$Year<(timeth))&(birthsdata$Year>(timet-102)),]
deathsdata2<-deathsdata[(deathsdata$Year<(timeth))&(deathsdata$Year>(timet-102)),]
deathsdata1<-na.omit(deathsdata2[as.numeric(as.character(deathsdata2$Age))<101,])

# Extrapolating population data at times t and t+h
popdatat=popdata1[popdata1$Year==c(timet),]
popdatath=popdata1[popdata1$Year==c(timeth),]

# This function takes the values of a variable, v, 
# at two times, t and t+h, and returns the derivative over that interval
change<-function(vt,vth,h){
  r<-log(vth/vt)/h
  vth2<-vt*exp(r*h/2)
  vdot=r*vth2
  return(vdot)
}

# This function also takes the values of a variable, v, 
# at two times, t and t+h, but this time returns the midpoint,
# that is, the value of v at time t+(h/2)
midp<-function(vt,vth,h){
  r<-log(vth/vt)/h
  vth2<-vt*exp(r*h/2)
  vth2[is.na(vth2)]<-0
  return(vth2)
}

# These are the c values at all ages x at times t and t+h
cdatatF<-popdatat$Female/sum(popdatat$Female)
cdatathF<-popdatath$Female/sum(popdatath$Female)

# This takes the above c values to the midpoint
cmidpointdataF<-midp(cdatatF,cdatathF,hvalue)

# This is the growth rate for all ages x from time t to t+h
rxF<-log(popdatath$Female/popdatat$Female)
rxF[is.infinite(rxF)]<-0
rxF[is.nan(rxF)]<-0

##### Births ####
nF=length(birthsdata1$Female)
# This takes the log of the ratio of birth rates h years apart all divided by h. This is the change in births.
RbirthsF=log(birthsdata1$Female[-(1:hvalue)]/birthsdata1$Female[-((nF-(hvalue-1)):nF)])
# This makes a data frame of the change in birth rates by year
BirthratesF<-cbind(birthsdata1$Year[-((nF-(hvalue-1)):nF)],RbirthsF)
colnames(BirthratesF) <-c("Year","RbirthsF")
# This is a vector of the change in births from youngest to oldest cohort as required
RbirthsFback<-rev(RbirthsF)

##### Deaths ####
# This is a function that makes a matrix of cohort death rates where rows are age and columns are cohort birth year. The input to the function is the end year
cohortdeathrate<-function(endyear){ 
  endyear=endyear-1
  cohortmxth2 <-matrix(0,101,101)
  dimnames(cohortmxth2) <-list(0:100, (endyear-100):(endyear)) 
  for (t in 1:101){
    cohortmxth <-c()
    for (i in 0:(t-1)){
      new1<-deathsdata1[deathsdata1$Year==c(endyear+1-t+i),]
      new2<-as.numeric(as.character(new1[new1$Age==c(i),"Female"]))
      new2[is.na(new2)]<-0
      cohortmxth<-c(cohortmxth,new2)
    }
    cohortmxth2[1:t,102-t]<-cohortmxth
  }
  return(cohortmxth2)
}
# This uses the above function to get the cohort deaths rates for times t and t+h
cdrtimethF<-cohortdeathrate(timeth)
cdrtimetF<-cohortdeathrate(timet)
# This finds the change in survivorship rates (it becomes survivorship as we want improvements (i.e. declines) in death rates to be expressed as positive change)
SurvratesF<-(colSums(cdrtimetF)-colSums(cdrtimethF))
# As with the births, this is a vector of the change in death rates from youngest to oldest cohort as required
SurvratesFback<-rev(SurvratesF)

##### Migration (as residual) ####
# This takes the change in migration as the residual of change in population not explained by changes in births or deaths
MigratesF <- rxF-RbirthsFback - SurvratesFback
# This creates a table of combined population, birth, deaths and migration growth rates
combinedratesF<-cbind(c(0:100), rxF, RbirthsFback,SurvratesFback, MigratesF)
colnames(combinedratesF) <-c("Year","rxF","BirthratesF (back)","SurvratesF (back)", "MigrationratesF")

##### Find derivative ####
## Check the derivative of the mean 3 different ways
meantF <- sum((popdatat[, "Female"])*(popdatat[, "Age"]))/sum(popdatat[, "Female"])
meanthF <- sum((popdatath[, "Female"])*(popdatath[, "Age"]))/sum(popdatath[, "Female"])
meandotF <- change(meantF, meanthF, hvalue)

apatF <- sum((popdatat[, "Female"])*(popdatat[, "Age"]))
apathF <- sum((popdatath[, "Female"])*(popdatath[, "Age"]))
reldevapaF <- (change(apatF, apathF, hvalue))/midp(apatF, apathF, hvalue)
reldevpaF <- (change(sum(popdatat[, "Female"]), sum(popdatath[, "Female"]), hvalue))/midp(sum(popdatat[, "Female"]), sum(popdatath[, "Female"]), hvalue)
meanmidF <- midp(meantF, meanthF, hvalue)
meandotF2 <- meanmidF*(reldevapaF - reldevpaF)

meandotF3 <- sum(rxF*(midp(popdatat$Female, popdatath$Female, hvalue)/sum(midp(popdatat$Female, popdatath$Female, hvalue)))*(popdatat[, "Age"]-meanmidF))


## Now for the three components
ResultyearF <- data.frame()
ResultyearF <- cbind(Names[Select_country],meantF, meanthF)

for (j in 2:5) {
  component <- sum(combinedratesF[,j]*(midp(popdatat$Female, popdatath$Female, hvalue)/sum(midp(popdatat$Female, popdatath$Female, hvalue)))*(popdatat[, "Age"]-meanmidF))
  ResultyearF <- cbind(ResultyearF, component)
}
colnames(ResultyearF) <- c(Names[Select_country],"meantF","meanthF","meandotF", "BirthsF", "SurvivorshipF", "MigrationF")

ResultF[Select_country,] <- ResultyearF

## Final check
meandotF4<- sum(as.numeric(ResultF[Select_country,5]), as.numeric(ResultF[Select_country,6]), as.numeric(ResultF[Select_country,7]))
checkF <- c(meandotF, meandotF2, meandotF3, meandotF4)
checkF
ResultF

#### Decompose with respect to time for Males #### 
# These are the c values at all ages x at times t and t+h
cdatatM<-popdatat$Male/sum(popdatat$Male)
cdatathM<-popdatath$Male/sum(popdatath$Male)

# This takes the above c values to the midpoint
cmidpointdataM<-midp(cdatatM,cdatathM,hvalue)

# This is the growth rate for all ages x from time t to t+h
rxM<-log(popdatath$Male/popdatat$Male)
rxM[is.infinite(rxM)]<-0
rxM[is.nan(rxM)]<-0

##### Births ####
nM=length(birthsdata1$Male)
# This takes the log of the ratio of birth rates h years apart all divided by h. This is the change in births.
RbirthsM=log(birthsdata1$Male[-(1:hvalue)]/birthsdata1$Male[-((nM-(hvalue-1)):nM)])
# This makes a data frame of the change in birth rates by year
BirthratesM<-cbind(birthsdata1$Year[-((nM-(hvalue-1)):nM)],RbirthsM)
colnames(BirthratesM) <-c("Year","RbirthsM")
# This is a vector of the change in births from youngest to oldest cohort as required
RbirthsMback<-rev(RbirthsM)

##### Deaths ####
# This is a function that makes a matrix of cohort death rates where rows are age and columns are cohort birth year. The input to the function is the end year
cohortdeathrate<-function(endyear){ 
  endyear=endyear-1
  cohortmxth2 <-matrix(0,101,101)
  dimnames(cohortmxth2) <-list(0:100, (endyear-100):(endyear)) 
  for (t in 1:101){
    cohortmxth <-c()
    for (i in 0:(t-1)){
      new1<-deathsdata1[deathsdata1$Year==c(endyear+1-t+i),]
      new2<-as.numeric(as.character(new1[new1$Age==c(i),"Male"]))
      new2[is.na(new2)]<-0
      cohortmxth<-c(cohortmxth,new2)
    }
    cohortmxth2[1:t,102-t]<-cohortmxth
  }
  return(cohortmxth2)
}
# This uses the above function to get the cohort deaths rates for times t and t+h
cdrtimethM<-cohortdeathrate(timeth)
cdrtimetM<-cohortdeathrate(timet)
# This finds the change in survivorship rates (it becomes survivorship as we want improvements (i.e. declines) in death rates to be expressed as positive change)
SurvratesM<-(colSums(cdrtimetM)-colSums(cdrtimethM))
# As with the births, this is a vector of the change in death rates from youngest to oldest cohort as required
SurvratesMback<-rev(SurvratesM)

##### Migration (as residual) ####
# This takes the change in migration as the residual of change in population not explained by changes in births or deaths
MigratesM <- rxM-RbirthsMback - SurvratesMback
# This creates a table of combined population, birth, deaths and migration growth rates
combinedratesM<-cbind(c(0:100), rxM, RbirthsMback,SurvratesMback, MigratesM)
colnames(combinedratesM) <-c("Year","rxM","BirthratesM (back)","SurvratesM (back)", "MigrationratesM")

##### Find derivative ####
## Check the derivative of the mean 3 different ways
meantM <- sum((popdatat[, "Male"])*(popdatat[, "Age"]))/sum(popdatat[, "Male"])
meanthM <- sum((popdatath[, "Male"])*(popdatath[, "Age"]))/sum(popdatath[, "Male"])
meandotM <- change(meantM, meanthM, hvalue)

apatM <- sum((popdatat[, "Male"])*(popdatat[, "Age"]))
apathM <- sum((popdatath[, "Male"])*(popdatath[, "Age"]))
reldevapaM <- (change(apatM, apathM, hvalue))/midp(apatM, apathM, hvalue)
reldevpaM <- (change(sum(popdatat[, "Male"]), sum(popdatath[, "Male"]), hvalue))/midp(sum(popdatat[, "Male"]), sum(popdatath[, "Male"]), hvalue)
meanmidM <- midp(meantM, meanthM, hvalue)
meandotM2 <- meanmidM*(reldevapaM - reldevpaM)

meandotM3 <- sum(rxM*(midp(popdatat$Male, popdatath$Male, hvalue)/sum(midp(popdatat$Male, popdatath$Male, hvalue)))*(popdatat[, "Age"]-meanmidM))

## Now for the three components
ResultyearM <- data.frame()
ResultyearM <- cbind(Names[Select_country],meantM, meanthM)

for (j in 2:5) {
  component <- sum(combinedratesM[,j]*(midp(popdatat$Male, popdatath$Male, hvalue)/sum(midp(popdatat$Male, popdatath$Male, hvalue)))*(popdatat[, "Age"]-meanmidM))
  ResultyearM <- cbind(ResultyearM, component)
}
colnames(ResultyearM) <- c(Names[Select_country],"meantM","meanthM","meandotM", "BirthsM", "SurvivorshipM", "MigrationM")

ResultM[Select_country,] <- ResultyearM

## Final check
meandotM4<- sum(as.numeric(ResultM[Select_country,5]), as.numeric(ResultM[Select_country,6]), as.numeric(ResultM[Select_country,7]))

checkM <- c(meandotM, meandotM2, meandotM3, meandotM4)
checkM

ResultF
ResultM

rm(list=setdiff(ls(), c("ResultF","ResultM", "Names", "username.hmd", "password.hmd")))

### Results ####
#### Pieces for Table 1 ####
ResultF1 <- as.data.frame(sapply(ResultF, as.numeric))
ResultF1<-round(ResultF1, 2)
ResultF1[,1]<- ResultF[,1]
ResultF<-ResultF1
colnames(ResultF) <- c("Country", "Mean_t", "Mean_th", "Mean_dot", "Births", "Survivorship", "Migration")

ResultM1 <- as.data.frame(sapply(ResultM, as.numeric))
ResultM1<-round(ResultM1, 2)
ResultM1[,1]<- ResultM[,1]
ResultM<-ResultM1
colnames(ResultM) <- c("Country", "Mean_t", "Mean_th", "Mean_dot", "Births", "Survivorship", "Migration")

Result_comparison <- cbind(ResultF[,1],ResultF[,-1]-ResultM[,-1])
colnames(Result_comparison) <- c("Country", "mean_dif_t", "mean_dif_th", "mean_dot_dif", "Births_dif", "Surv_dif", "Mig_dif")

View(ResultF)
View(ResultM)
View(Result_comparison)

ResultF_long <- ResultF %>% pivot_longer(cols=c("Births", "Survivorship", "Migration"), names_to="Component", values_to="Growth")
ResultM_long <- ResultM %>% pivot_longer(cols=c("Births", "Survivorship", "Migration"), names_to="Component", values_to="Growth")
ResultF_long <- ResultF_long %>% mutate (Sex="F")
ResultM_long <- ResultM_long %>% mutate (Sex="M")

Result_combined_long <- rbind(ResultF_long[,-(2:4)], ResultM_long[,-(2:4)])
Result_combined_long$Country<-as.character(Result_combined_long$Country)
Result_combined_long[Result_combined_long == "GBRTENW"] <- "E&W"
Result_combined_long[Result_combined_long == "GBR_SCO"] <- "SCO"
Result_combined_long[Result_combined_long == "FRATNP"] <- "FRA"
Result_combined_long[Result_combined_long == "US"] <- "USA"
Result_combined_long$Country<-as.factor(Result_combined_long$Country)
Result_combined_long$Country <- factor(Result_combined_long$Country, levels = c("ITA","FRA","FIN","ESP","SCO","CHE","USA","E&W","SWE","DNK","NLD","AUS","NOR"))

#### Figure 4 ####
Figure_4 <- ggplot(data=Result_combined_long, aes(fill=Component,x=Sex, y=Growth)) +
  ylim(-1,3)+
  geom_bar(stat="identity", position="stack") +
  labs(y="Contribution to the change in population mean age") +
  theme_light()+
  facet_grid(~Country)+scale_fill_manual(values=c("paleturquoise3", "plum3", "palevioletred"), 
                                         name  ="Component",
                                         breaks=c("Births", "Survivorship", "Migration"),
                                         labels=c("Births", "Survivorship", "Migration")) +
  theme(axis.title = element_text(face="bold"))

Figure_4


