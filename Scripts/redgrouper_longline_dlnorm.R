#Red Grouper Longline Model

library(tidyverse)
library(haven)
library(magrittr)
library(lsmeans)
library(car)


#Set Working Directory to where data is stored
setwd("D:/SASwork/Red Grouper Data and Code for AK/Walter")
mydata <- read_sas("Longline/Data/longline.sas7bdat")

#load bias correction function
source("Y:/Adam Kemberling/FunctionLibrary/LoBiasCorrectionFunction.R")

#Load functions for lognormal histogram of residuals and qqplot
source("Y:/Adam Kemberling/FunctionLibrary/Lognormal_diagnostics.R")

#Load function for joining models together and incorporating bias correction
source("Y:/Adam Kemberling/FunctionLibrary/dlnorm_lomethod_lsmeans.R")

#Format ggplot for rest of document
theme_set(theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))


#Insert any code that would subset the dataset for any reason
species <- "Red Grouper"
ystart <- "2001"
yend <- "2017"
season_select <- "Fall"
age_select <- "all"
region_select <- "East"
cpue_column <- "N70" 
year_column <- "YEAR"

#rename count/cpue column name to template name
mydata <- mydata %>% rename(cpue_dat = paste(cpue_column))

#Need to rename year_column to be exactly "Year"
mydata <- mydata %>% rename(Year = paste(year_column))


#create title from selections
p_title <- str_c("NMFS Bottom Longline", species, region_select, str_c(ystart,yend, sep = "-"), sep = " ")


#Select column names of factors used in analysis
fact_list <- c("Year", "subarea", "TOD") #will be used to set formatting for summary tables with if statements
con_vars <- c("depth")

#make vector of survey years for plot scales
ynames <- lubridate::year(seq.Date(as.Date(ystart, "%Y"), as.Date(yend, "%Y"), by = "year"))
ynames <- as.character(ynames)


#Make any changes to dataset, and calculate any derived values here
mydata <- mydata %>% dplyr::filter(Year >= as.numeric(ystart),
                        Year <= as.numeric(yend),
                        Year != "2002",
                        GOM_AREA == "East Gulf",
                        MONTH != 10 | 11,
                        STARTLON > -87)%>% 
  mutate(success = ifelse(cpue_dat > 0, 1, 0),
         lgcpue = ifelse(cpue_dat > 0, log(cpue_dat), NA),
         depth = (STARTDEPTH + ENDDEPTH) / 2,
         subarea = "southern",
         subarea = ifelse(STARTLAT > 27, "central", subarea),
         subarea = ifelse(STARTLAT > 29, "northern", subarea))



#get counts by year
SamplesByYear <- group_by(mydata, Year, add=FALSE) %>% 
                 summarise (N = n(),
                            NominalPPOS = mean(success, na.rm = T)) %>% 
                rename(Year_f = Year) %>% 
                as.data.frame()




########### Factor Summary #################
#Loop through factors names in fact_list and create a table of observations
t1 <- fact_table(mydata = mydata, fact_list = fact_list)
t1

############  Figures  ######################

### Figure 1. CPUE timeline

mydata %>% 
  group_by(Year) %>% 
  summarise(MEAN_CPUE = mean(cpue_dat)) %>% 
  ggplot(aes(Year, MEAN_CPUE)) + 
  geom_line(aes(group = 1)) +
  geom_point() +
  ylim(0,3) + 
  scale_x_discrete(limits = c(as.numeric(ystart:yend)), labels = ynames) + 
  xlab("Year") +
  ylab("Observed CPUE")



### Figure 2. Observed proportion positive timeline

mydata %>% 
  group_by(Year) %>% 
  summarise(obpos = mean(success, na.rm = T)) %>% 
  ggplot(aes(Year, obpos)) + 
  geom_line(aes(group = 1)) + 
  geom_point() +
  scale_x_discrete(limits = c(as.numeric(ystart:yend)), labels = ynames) + 
  xlab("Year") +
  ylab("Observed Proportion Positive Catch")
  


### Figure 3. Frequency distribution log CPUE of positive catch by year
#Make a positive catch subset
pos_data <- mydata %>% filter(success == 1)

ggplot(pos_data, aes(x = lgcpue)) + 
  geom_histogram(aes(y = stat(density)), bins = 8, fill = "gray80", color = "gray20") +
  stat_function(
    data = pos_data,
    fun = dnorm, 
    args = list(mean = mean(mydata$lgcpue, na.rm = T), sd = sd(mydata$lgcpue, na.rm = T)),
    col = 'gray20') + 
  ylab("Percent") #+ facet_wrap(~Year)



### Figure 4.
mydata %>% group_by(.dots = fact_list) %>% 
  summarise(`Percent Positive` = mean(success)) %>% 
  ggplot(aes(x = `Percent Positive`)) + 
  geom_histogram(aes(y = stat(density)), bins = 10, fill = "gray80", color = "gray20") 

mydata %>% group_by(.dots = fact_list) %>% summarise(N_Obs = n(),
                                                           `Percent Positive` = mean(success),
                                                           `Std Dev` = sd(success))





######################  Models  #########################

#Set contrasts to match SAS
options(contrast=c("contr.SAS", "contr.poly"))
options(contrasts=c("contr.SAS", "contr.poly"))




######  Binomial Model  ##############

#Setup factors for the model automatically
mod_factors <- mydata %>% select(one_of(fact_list))                                           #select the columns of interest
colnames(mod_factors) <- lapply(colnames(mod_factors), function(x) paste(x, "_f", sep = ""))  #rename them with a _f on the end
mod_factors <- data.frame(sapply(mod_factors, factor))
names(mod_factors)

#Add them as new columns to mydata
mydata <- bind_cols(mydata, mod_factors)
pos_data <- filter(mydata, success == 1)

#Set up function call automatically from mod_factors and con_vars
b <- paste(colnames(mod_factors), collapse =" + ")                      #Paste the factors separated by a +
b <- paste(b, "+", paste(con_vars, collapse = " + "))       #Do the same for any continuous variables
b <- paste("success ~ ", b, sep = "")                       #Add the response to it
b_form <- as.formula(b)                                     #coerce to a formula


#Write the model
m1 <- glm(b_form, 
          data = mydata, 
          family = quasibinomial(link = "logit"))



#Print the model Formula
m1$call
b_form

#Print the Coefficient values in a table
#kable(m1$coefficients)

#examine type III anova with car
Anova(m1, type=3)

#equamine reference grid for m1
#ref.grid(m1)



#######  Lognormal model  ###############

## Lognormal glm on positive catches

#And a year summary of the mean catch
pos_timeline <- pos_data %>% group_by(Year_f) %>% summarise(obcppos = mean(cpue_dat))


#Set up function call automatically from mod_factors and con_vars
b <- paste(colnames(mod_factors), collapse =" + ")                      #Paste the factors separated by a +
b <- paste(b, "+", paste(con_vars, collapse = " + "))       #Do the same for any continuous variables
b <- paste("lgcpue ~ ", b, sep = "")                       #Add the response to it
ln_form <- as.formula(b)   



m2 <- glm(ln_form, 
          data = pos_data, 
          family = "gaussian")

#Print model call
m2$call

#Put coefficient estimates in a table
#kable(m2$coefficients)


ln_hist(ln_model = m2)
ln_qqplot(ln_model = m2)





#####  Bias correction  #####

estimate <- dlnorm_lo_method(bin_model = m1, Ln_model = m2)


#Pull out important columns
short <- estimate %>% select(Year_f, Index, LCI, UCI, StdIndex, CV_Index, StdLCI, StdUCI)
DLNIndex <- merge(SamplesByYear,short, by = "Year_f")
rm(short)

#add missing years
YearRange <- as.data.frame(seq(min(as.numeric(DLNIndex$Year_f)), max(as.numeric(DLNIndex$Year_f))))
names(YearRange) <- c("Year_f")
DLNIndexMissing <- merge(YearRange,DLNIndex,by="Year_f",all.x=T)


#Make dataframe of the observed cPUE
obs_df <- mydata %>% group_by(Year) %>% dplyr::summarise(N_OBS = n(),
                                                         N_PP = sum((cpue_dat > 0) == TRUE, na.rm = T),
                                                         PP_OBS = N_PP/N_OBS,
                                                         MEAN_CPUE = mean(cpue_dat, na.rm = T))




#### Plot 1  ########

estimate <- estimate %>% mutate(
  Year_n = as.numeric(as.character(Year_f)))


overall_meancpue <- mean(mydata$cpue_dat)
obscpue_df <- mydata %>% group_by(Year_f) %>%  summarise(ob_scpue = mean(cpue_dat)/overall_meancpue) %>% 
  mutate(Year_n = as.numeric(as.character(Year_f)))

ggplot(data = DLNIndex, aes(Year_f, StdIndex)) + 
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_line(aes(Year_f, StdUCI, group = 1),color = "blue", lty = 2) +
  geom_line(aes(Year_f, StdLCI, group = 1),color = "blue", lty = 2) +
  geom_line(data = obscpue_df, aes(Year_n, ob_scpue, group = 1), color = "red") + 
  geom_point(data = obscpue_df, aes(Year_n, ob_scpue, group = 1), color = "red") + 
  scale_x_discrete(limits = c(as.numeric(ynames)),  labels = ynames) + 
  ylim(0,6) +xlab("Year") + ylab("Estimated CPUE") +
  ggtitle(p_title)




####  Plot 2  #####

obppos <- mydata %>% group_by(Year_f) %>% summarise(n = n(),
                                                    obppos = sum(success)/n) 

ggplot(estimate, (aes(Year_f, LSM_prob))) +
  geom_line(aes(group = 1), color = "blue", lty = 2) + 
  geom_point(color = "blue") +
  geom_line(data = obppos, aes(Year_f, obppos, group = 1), color = "red") + 
  geom_point(data = obppos, aes(Year_f, obppos), color = "red") +
  ylim(0,0.4) + 
  xlab("Year") +
  ylab("Proportion of Samples Catch > 0") +
  ggtitle(p_title)





#### Plot 3  #####3

estimate$bccpue <- estimate$LSM_bcpos * estimate$gc_pos

ggplot(pos_timeline, aes(Year_f, obcppos)) +
  geom_line(aes(group = 1),color = "red") +
  geom_point(color = "red") +
  geom_line(data = estimate, aes(Year_f, bccpue, group = 1), lty = 2, color = "blue") +
  geom_point(data = estimate, aes(Year_f, bccpue), color = "blue") +
  ylim(0,9) +
  scale_y_continuous(limits =c(0,9), breaks = 0:9) +
  xlab("Year") +
  ylab("obcppos") +
  ggtitle(p_title)




#####  Plot 4  #####

ggplot(data = obs_df, aes(Year, MEAN_CPUE)) +
  geom_line(color = "red")  +
  geom_line(data = DLNIndex, aes(Year_f, Index), color = "blue", lty = 2) +
  geom_point(color = "red")  +
  geom_point(data = DLNIndex, aes(Year_f, Index), color = "blue") +
  scale_x_discrete(limits = c(as.numeric(ystart:yend)), labels = ynames) + 
  xlab("Year") + ylab("Estimated CPUE") +
  ggtitle(p_title)



