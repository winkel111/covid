#Libraries
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
library(nls2)
library(nlstools) #Tools for Nonlinear Regression Analysis

#Plot current COVID-19 deaths in US
#(c) Alexander Johs
#Last updated 1/8/2021

#Clear global environment
rm(list = ls(all.names = TRUE))

#Set path
path <- "~/R/covid/"

#Set working directory to current path
setwd(path)

#Function to calculate moving average
ma <- function(x, n = 9){stats::filter(x, rep(1 / n, n), sides = 2)}

#Initialize perc_states
perc_states <-c()

#Loop through states
for (val in state.name)
{
  #Clear plot window
  graphics.off()
  
  #Select country
  country <- "US"
  state <- val
  
#Population data
if (!(exists("popul_states"))) {
   #popul <- read_csv(url("https://pkgstore.datahub.io/core/population/population_csv/data/ead5be05591360d33ad1a37382f8f8b1/population_csv.csv"), col_types = cols())
   popul_states <- read_csv(url("http://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv"))

   #Get US population data
   #pop <- population_states %>%
   #  mutate_all(~replace(., is.na(.), 0)) %>%
   #  dplyr::filter(`Country Name` == "United States") %>%
   #  dplyr::filter(`Year` == 2016)
   #popp <- as.numeric(pop[1,4])

   pop <- popul_states %>%
     dplyr::select('NAME','POPESTIMATE2019') %>%
     dplyr::filter(`NAME` == "United States")
   popp <- as.numeric(pop[1,2])
}

#pop <- popul %>%
#  mutate_all(~replace(., is.na(.), 0)) %>%
#  dplyr::filter(`Country Name` == country) %>%
#  dplyr::filter(`Year` == 2016)
#popo <- as.numeric(pop[1,4])

#Trim state population data
popstate <- popul_states %>%
  dplyr::select('NAME','POPESTIMATE2019') %>%
  dplyr::filter(`NAME` == state)
popo <- as.numeric(popstate[1,2])

#Get case data
if (!(exists("casus"))) {
   #case <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"), col_types = cols())
   #case <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"), col_types = cols())
   #case <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"), col_types = cols())
   #casus <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"), col_types = cols())
   casus <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"), col_types = cols())

   #Fix data for US
   cases <- casus %>%
     mutate_all(~replace(., is.na(.), 0)) %>%
     dplyr::filter(`Country_Region` == "US") %>%
     dplyr::filter(`Province_State` != "Diamond Princess") %>%
     dplyr::filter(`Province_State` != "Grand Princess") %>%
     #dplyr::filter(`Province_State` %in% state.name) %>%
     select(13:ncol(casus)) %>%
     colSums()
   
   #Create data frame for US deaths
   pdata <- data.frame(date = names(cases), cases = cases)
   rownames(pdata) <- c()
   pdata$date <- as.Date(pdata$date, format = "%m/%d/%y")
   
   #Model fit
   Xp <- as.numeric(rownames(pdata))
   Yp <- pdata[,2]
   spdata <- cbind.data.frame(Xp,Yp)
   
   #Calculate differences
   dsp <- spdata %>% mutate(Diff = Yp - lag(Yp))
   dspmavg <- as.vector(ma(dsp[,3]))
   dpdata <- data.frame(pdata[,1],dsp[,3],dspmavg)
   names(dpdata) <- c("date","diff","dspmavg")
   
   #Linear model fit over the 4 most recent days
   fstartp <- nrow(spdata)-3
   lmodelp <- lm(formula = Yp ~ Xp,  data = na.omit(spdata[fstartp:nrow(spdata),]))
   qMp <- summary(lmodelp)
   qkp <-summary(lmodelp)$coefficients[2, 1]
   print("United States:")
   print(qMp)
   
   #Start for fit
   qfp <- tail(pdata[,1],1)-3
   
   #Latest number of deaths
   latestp <- tail(pdata[,2],1)
   
   #Latest number of new deaths
   recentp <- diff(tail(pdata[,2],2))
   
   #Percent penetration
   percp <- round(tail(pdata[,2],1)/popp*100,2)
   
   #Plot parameters
   fsize <- 22
   psize <- 5
   lsize <- 2
   
   #Plot cumulative deaths in US
   qp1 <- ggplot(pdata, aes(x=date, y=cases))
   qp1 <- qp1 + theme_bw(base_size = fsize) #+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
   qp1 <- qp1 + geom_point(color="gray60", size=psize)
   qp1 <- qp1 + stat_smooth(data=subset(pdata, date >= qfp),method="lm", color="red", size=lsize, se = FALSE, fill="red", level = 0.95)
   #qp1 <- qp1 + stat_function(fun = function(x) fModel(x, k=coef(lmodelp)["Xp"], d=coef(lmodelp)["(Intercept)"]), size=lsize, color="gray40")
   #qp1 <- qp1 + geom_line(data = modelfit, aes(date, y=cases), color="firebrick", size=lsize)
   #qp1 <- qp1 + stat_function(fun = function(x) fModel(x, a=qa, b=qb, c=qc), size=lsize, color="firebrick")
   qp1 <- qp1 + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m/%d") # + scale_y_log10()
   qp1 <- qp1 + scale_y_continuous(breaks = scales::pretty_breaks(n = 6),labels = comma)
   qp1 <- qp1 + theme(axis.text.x = element_text(angle = 30, hjust = 1))
   qp1 <- qp1 + xlab(expression("Date")) #+ scale_y_log10()
   qp1 <- qp1 + ylab(expression("US deaths"))
   #qp1 <- qp1 + expand_limits(x=c(pdata[1,1], as.Date("2020-04-08")))
   qp1 <- qp1 + labs(caption=paste(c(formatC(latestp, format="f", big.mark = ",", digits=0)," deaths\n",percp,"% of population (",round(popp/1000000,1)," M)","\n",formatC(qkp, format="f", big.mark = ",", digits=0)," deaths per day","\nLast update: ",as.character(tail(pdata[,1],1))," - New deaths: ",comma(recentp),"\nData source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)"), collapse = ""))
   qp1 <- qp1 + theme(plot.caption=element_text(size=fsize/2, hjust=0, margin=margin(12,0,0,0)))
   
   #Plot daily deaths in US
   qp3 <- ggplot(dpdata, aes(x=date, y=diff))
   qp3 <- qp3 + theme_bw(base_size = fsize) #+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
   qp3 <- qp3 + geom_point(color="gray60", size=psize)
   qp3 <- qp3 + geom_line(aes(x=date, y=dspmavg), color="red", fill="pink", size=lsize)
   #qp3 <- qp3 + geom_smooth(color="red", method="gam", fill="pink", size=lsize)
   #qp3 <- qp3 + stat_smooth(data=subset(pdata, date >= qfp),method="lm", color="gray40", size=lsize, se = FALSE, level = 0.95)
   #qp3 <- qp3 + stat_function(fun = function(x) fModel(x, k=coef(lmodelp)["Xp"], d=coef(lmodelp)["(Intercept)"]), size=lsize, color="gray40")
   #qp3 <- qp3 + geom_line(data = modelfit, aes(date, y=cases), color="firebrick", size=lsize)
   #qp3 <- qp3 + stat_function(fun = function(x) fModel(x, a=qa, b=qb, c=qc), size=lsize, color="firebrick")
   qp3 <- qp3 + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m/%d") # + scale_y_log10()
   qp3 <- qp3 + scale_y_continuous(breaks = scales::pretty_breaks(n = 6),labels = comma)
   qp3 <- qp3 + theme(axis.text.x = element_text(angle = 30, hjust = 1))
   qp3 <- qp3 + xlab(expression("Date")) #+ scale_y_log10()
   qp3 <- qp3 + ylab(expression("US daily deaths"))
   #qp3 <- qp3 + expand_limits(x=c(pdata[1,1], as.Date("2020-04-08")))
   #qp3 <- qp3 + labs(caption=paste(c(latestp," deaths\n",percp,"% of population","\n",qkp," deaths per day","\nLast update: ",as.character(tail(pdata[,1],1)),"\nData source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)"), collapse = ""))
   #qp3 <- qp3 + theme(plot.caption=element_text(size=8, hjust=0, margin=margin(12,0,0,0)))
   
}

cases_other <- casus %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::filter(`Country_Region` == country) %>%
  # dplyr::filter(`Province_State` == state) %>%
  dplyr::filter(str_detect(`Province_State`, state)) %>%
  select(13:ncol(casus)) %>%
  colSums()
country <- state

#Create data frame for deaths from states
odata <- data.frame(date = names(cases_other), cases = cases_other)
rownames(odata) <- c()
odata$date <- as.Date(odata$date, format = "%m/%d/%y")

#Model fit for states
Xo <- as.numeric(rownames(odata))
Yo <- odata[,2]
sodata <- cbind.data.frame(Xo,Yo)

#Calculate differences
dso <- sodata %>% mutate(Diff = Yo - lag(Yo))
dsomavg <- as.vector(ma(dso[,3]))
dodata <- data.frame(odata[,1],dso[,3],dsomavg)
names(dodata) <- c("date","diff","dsomavg")

#Linear model fit over the 4 most recent days
fstarto <- nrow(sodata)-3
lmodelo <- lm(formula = Yo ~ Xo,  data = na.omit(sodata[fstarto:nrow(sodata),]))
qMo <- summary(lmodelo)
qko <- summary(lmodelo)$coefficients[2, 1]
print(paste(c(country,":"), collapse = ""))
print(qMo)

#Define function for plots
fModel <- function(x, k, d) {
  (k*x+d)
}

#Start for fit
qfo <- tail(odata[,1],1)-3

#Latest number of deaths
latesto <- tail(odata[,2],1)

#Latest number of new deaths
recento <- diff(tail(odata[,2],2))

#Percent penetration
perco <- round(tail(odata[,2],1)/popo*100,2)
perc_states <- c(perc_states, perco)

#Function to create minor ticks
#insert_minor <- function(major_labs, n_minor) {labs <- 
#  c( sapply( major_labs, function(x) c(x, rep("", 4) ) ) )
#labs[1:(length(labs)-n_minor)]}

#Plot cumulative deaths in states
qp2 <- ggplot(odata, aes(x=date, y=cases))
qp2 <- qp2 + theme_bw(base_size = fsize) #+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
qp2 <- qp2 + geom_point(color="gray60", size=psize)
qp2 <- qp2 + stat_smooth(data=subset(odata, date >= qfo),method="lm", color="blue", size=lsize, se = FALSE, fill="blue", level = 0.95)#qp2 <- qp2 + stat_smooth(data=subset(pdata, date >= "2020-03-20"),method="lm", color="gray40", size=lsize, se = FALSE, level = 0.95)
#qp2 <- qp2 + xlim(min(pdata$date),max(pdata$date))
qp2 <- qp2 + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m/%d") # + scale_y_log10()
qp2 <- qp2 + scale_y_continuous(breaks = scales::pretty_breaks(n = 6),labels = comma)
qp2 <- qp2 + theme(axis.text.x = element_text(angle = 30, hjust = 1))
qp2 <- qp2 + xlab(expression("Date")) #+ scale_y_log10()
qp2 <- qp2 + ylab(paste("Deaths in",country))
qp2 <- qp2 + labs(caption=paste(c(formatC(latesto, format="f", big.mark = ",", digits=0)," deaths\n",perco,"% of population (",round(popo/1000000,1)," M)","\n",formatC(qko, format="f", big.mark = ",", digits=0)," deaths per day","\nLast update: ",as.character(tail(odata[,1],1))," - New deaths: ",comma(recento),"\nData source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)"),collapse = ""))
qp2 <- qp2 + theme(plot.caption=element_text(size=fsize/2, hjust=0, margin=margin(12,0,0,0)))

#Plot daily deaths in states
qp4 <- ggplot(dodata, aes(x=date, y=diff))
qp4 <- qp4 + theme_bw(base_size = fsize) #+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
qp4 <- qp4 + geom_point(color="gray60", size=psize)
qp4 <- qp4 + geom_line(aes(x=date, y=dsomavg), color="blue", fill="lightsteelblue", size=lsize)
#qp4 <- qp4 + geom_smooth(color="blue", method="gam", fill="lightsteelblue", size=lsize)
#qp4 <- qp4 + stat_smooth(data=subset(pdata, date >= qfp),method="lm", color="gray40", size=lsize, se = FALSE, level = 0.95)
#qp4 <- qp4 + stat_function(fun = function(x) fModel(x, k=coef(lmodelp)["Xp"], d=coef(lmodelp)["(Intercept)"]), size=lsize, color="gray40")
#qp4 <- qp4 + geom_line(data = modelfit, aes(date, y=cases), color="firebrick", size=lsize)
#qp4 <- qp4 + stat_function(fun = function(x) fModel(x, a=qa, b=qb, c=qc), size=lsize, color="firebrick")
qp4 <- qp4 + scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m/%d") # + scale_y_log10()
qp4 <- qp4 + scale_y_continuous(breaks = scales::pretty_breaks(n = 6),labels = comma)
qp4 <- qp4 + theme(axis.text.x = element_text(angle = 30, hjust = 1))
qp4 <- qp4 + xlab(expression("Date")) #+ scale_y_log10()
qp4 <- qp4 + ylab(paste("Daily deaths in",country))
#qp4 <- qp4 + expand_limits(x=c(pdata[1,1], as.Date("2020-04-08")))
#qp4 <- qp4 + labs(caption=paste(c(latestp," deaths\n",percp,"% of population","\n",qkp," deaths per day","\nLast update: ",as.character(tail(pdata[,1],1)),"\nData source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)"), collapse = ""))
#qp4 <- qp4 + theme(plot.caption=element_text(size=8, hjust=0, margin=margin(12,0,0,0)))

#Generate figure
gridimg <- arrangeGrob(qp1, qp2, qp3, qp4, ncol=2)
grid.draw(gridimg)
gridus <- arrangeGrob(qp1, qp3, ncol=1)
gridother <- arrangeGrob(qp2, qp4, ncol=1)

ggsave(gridimg, file=paste(c(path,"us_states_deaths_",country,".png"), collapse = ""), width = 16, height = 12, dpi=300)
#ggsave(gridus, file=paste(c(path,"deaths_us",".png"), collapse = ""), width = 8, height = 12, dpi=300)
#ggsave(gridother, file=paste(c(path,country,".png"), collapse = ""), width = 8, height = 12, dpi=300)

}

#Collect data on exposed population
df <- data.frame(state.name,perc_states)
pen <- df[order(-perc_states),]
pen$state.name <- factor(pen$state.name, levels = pen$state.name)

#Plot bar graph
qp5 <- ggplot(data=pen, aes(x=state.name, y=perc_states))
qp5 <- qp5 + theme_bw(base_size = fsize-10)
qp5 <- qp5 + geom_bar(stat="identity", fill="firebrick")
qp5 <- qp5 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
qp5 <- qp5 + scale_y_continuous(expand = c(0,0), limits = c(0.0, max(perc_states)*1.05))
qp5 <- qp5 + xlab(expression("State"))
qp5 <- qp5 + ylab(paste("% of population deceased"))

print(qp5)
ggsave(qp5, file=paste(c(path,"us_states_deaths",".png"), collapse = ""), width = 12, height = 8, dpi=300)

