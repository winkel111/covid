#Libraries
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
library(nls2)
library(nlstools) #Tools for Nonlinear Regression Analysis

#Plot current COVID-19 cases in US and other countries
#(c) Alexander Johs
#Last updated 4/6/2020

#Clear plot window
graphics.off()

path <- "~/R/covid/"
#Set working directory to current path
setwd(path)

#Select country
country <- "Australia"
state <- ""

#Population data
population <- read_csv(url("https://pkgstore.datahub.io/core/population/population_csv/data/ead5be05591360d33ad1a37382f8f8b1/population_csv.csv"), col_types = cols())

#case <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"), col_types = cols())
#case <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"), col_types = cols())
case <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"), col_types = cols())
casus <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"), col_types = cols())
#casus <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"), col_types = cols())

#Trim population data
pop <- population %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::filter(`Country Name` == "United States") %>%
  dplyr::filter(`Year` == 2016)
popp <- as.numeric(pop[1,4])

pop <- population %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::filter(`Country Name` == country) %>%
  dplyr::filter(`Year` == 2016)
popo <- as.numeric(pop[1,4])

#Fix data for US
cases <- casus %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::filter(`Country_Region` == "US") %>%
  dplyr::filter(`Province_State` != "Diamond Princess") %>%
  dplyr::filter(`Province_State` != "Grand Princess") %>%
  #dplyr::filter(`Province_State` %in% state.name) %>%
  select(12:ncol(casus)) %>%
  colSums()


if (state != "") {
  cases_other <- casus %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    dplyr::filter(`Country_Region` == country) %>%
    # dplyr::filter(`Province_State` == state) %>%
    dplyr::filter(str_detect(`Province_State`, state)) %>%
    select(12:ncol(casus)) %>%
    colSums()
    country <- state
} else {

#Select data for other countries
cases_other <- case %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::filter(`Country/Region` == country) %>%
  select(5:ncol(case)) %>%
  colSums()
}

#plot(cases)

#Create data frame for US cases
pdata <- data.frame(date = names(cases), cases = cases)
rownames(pdata) <- c()
pdata$date <- as.Date(pdata$date, format = "%m/%d/%y")
#X1 <- as.Date(pdata[,1],format = "%m/%d/%y")

#Create data frame for cases from others
odata <- data.frame(date = names(cases_other), cases = cases_other)
rownames(odata) <- c()
odata$date <- as.Date(odata$date, format = "%m/%d/%y")

#Model fit
Xp <- as.numeric(rownames(pdata))
Yp <- pdata[,2]
spdata <- cbind.data.frame(Xp,Yp)

Xo <- as.numeric(rownames(odata))
Yo <- odata[,2]
sodata <- cbind.data.frame(Xo,Yo)

#Calculate differences
dsp <- spdata %>% mutate(Diff = Yp - lag(Yp))
dso <- sodata %>% mutate(Diff = Yo - lag(Yo))
dpdata <- data.frame(pdata[,1],dsp[,3])
dodata <- data.frame(odata[,1],dso[,3])
names(dpdata) <- c("date","diff")
names(dodata) <- c("date","diff")

#Linear model fit over the 4 most recent days
fstartp <- nrow(spdata)-3
fstarto <- nrow(sodata)-3

lmodelp <- lm(formula = Yp ~ Xp,  data = na.omit(spdata[fstartp:nrow(spdata),]))
lmodelo <- lm(formula = Yo ~ Xo,  data = na.omit(sodata[fstarto:nrow(sodata),]))

qMp <- summary(lmodelp)
qMo <- summary(lmodelo)

qkp <-summary(lmodelp)$coefficients[2, 1]
qko <- summary(lmodelo)$coefficients[2, 1]

print("United States:")
print(qMp)
print(paste(c(country,":"), collapse = ""))
print(qMo)

#Define function for plots
fModel <- function(x, k, d) {
  (k*x+d)
}

#Plot the data
fsize <- 26
psize <- 5
lsize <- 2

#Start for fit
qfp <- tail(pdata[,1],1)-3
qfo <- tail(odata[,1],1)-3

#Latest number of cases
latestp <- tail(pdata[,2],1)
latesto <- tail(odata[,2],1)

#Percent penetration
percp <- round(tail(pdata[,2],1)/popp*100,2)
perco <- round(tail(odata[,2],1)/popo*100,2)

#Function to create minor ticks
#insert_minor <- function(major_labs, n_minor) {labs <- 
#  c( sapply( major_labs, function(x) c(x, rep("", 4) ) ) )
#labs[1:(length(labs)-n_minor)]}

qp1 <- ggplot(pdata, aes(x=date, y=cases))
qp1 <- qp1 + theme_bw(base_size = fsize) #+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
qp1 <- qp1 + geom_point(color="blue", size=psize)
qp1 <- qp1 + stat_smooth(data=subset(pdata, date >= qfp),method="lm", color="gray40", size=lsize, se = FALSE, level = 0.95)
#qp1 <- qp1 + stat_function(fun = function(x) fModel(x, k=coef(lmodelp)["Xp"], d=coef(lmodelp)["(Intercept)"]), size=lsize, color="gray40")
#qp1 <- qp1 + geom_line(data = modelfit, aes(date, y=cases), color="firebrick", size=lsize)
#qp1 <- qp1 + stat_function(fun = function(x) fModel(x, a=qa, b=qb, c=qc), size=lsize, color="firebrick")
qp1 <- qp1 + scale_x_date(date_breaks = "2 weeks", date_labels = "%Y/%m/%d") # + scale_y_log10()
qp1 <- qp1 + scale_y_continuous(breaks = scales::pretty_breaks(n = 6),labels = comma)
qp1 <- qp1 + theme(axis.text.x = element_text(angle = 30, hjust = 1))
qp1 <- qp1 + xlab(expression("Date")) #+ scale_y_log10()
qp1 <- qp1 + ylab(expression("US cases"))
#qp1 <- qp1 + expand_limits(x=c(pdata[1,1], as.Date("2020-04-08")))
qp1 <- qp1 + labs(caption=paste(c(latestp," cases\n",percp,"% of population","\n",qkp," cases per day","\nLast update: ",as.character(tail(pdata[,1],1)),"\nData source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)"), collapse = ""))
qp1 <- qp1 + theme(plot.caption=element_text(size=fsize/2, hjust=0, margin=margin(12,0,0,0)))

qp2 <- ggplot(odata, aes(x=date, y=cases))
qp2 <- qp2 + theme_bw(base_size = fsize) #+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
qp2 <- qp2 + geom_point(color="red", size=psize)
qp2 <- qp2 + stat_smooth(data=subset(odata, date >= qfo),method="lm", color="gray40", size=lsize, se = FALSE, level = 0.95)#qp2 <- qp2 + stat_smooth(data=subset(pdata, date >= "2020-03-20"),method="lm", color="gray40", size=lsize, se = FALSE, level = 0.95)
#qp2 <- qp2 + xlim(min(pdata$date),max(pdata$date))
qp2 <- qp2 + scale_x_date(date_breaks = "2 weeks", date_labels = "%Y/%m/%d") # + scale_y_log10()
qp2 <- qp2 + scale_y_continuous(breaks = scales::pretty_breaks(n = 6),labels = comma)
qp2 <- qp2 + theme(axis.text.x = element_text(angle = 30, hjust = 1))
qp2 <- qp2 + xlab(expression("Date")) #+ scale_y_log10()
qp2 <- qp2 + ylab(paste("Cases in",country))
qp2 <- qp2 + labs(caption=paste(c(latesto," cases\n",perco,"% of population","\n",qko," cases per day","\nLast update: ",as.character(tail(odata[,1],1)),"\nData source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)"),collapse = ""))
qp2 <- qp2 + theme(plot.caption=element_text(size=fsize/2, hjust=0, margin=margin(12,0,0,0)))

qp3 <- ggplot(dpdata, aes(x=date, y=diff))
qp3 <- qp3 + theme_bw(base_size = fsize) #+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
qp3 <- qp3 + geom_point(color="blue", size=psize)
qp3 <- qp3 + geom_smooth(color="gray50", method="gam", size=lsize)
#qp3 <- qp3 + stat_smooth(data=subset(pdata, date >= qfp),method="lm", color="gray40", size=lsize, se = FALSE, level = 0.95)
#qp3 <- qp3 + stat_function(fun = function(x) fModel(x, k=coef(lmodelp)["Xp"], d=coef(lmodelp)["(Intercept)"]), size=lsize, color="gray40")
#qp3 <- qp3 + geom_line(data = modelfit, aes(date, y=cases), color="firebrick", size=lsize)
#qp3 <- qp3 + stat_function(fun = function(x) fModel(x, a=qa, b=qb, c=qc), size=lsize, color="firebrick")
qp3 <- qp3 + scale_x_date(date_breaks = "2 weeks", date_labels = "%Y/%m/%d") # + scale_y_log10()
qp3 <- qp3 + scale_y_continuous(breaks = scales::pretty_breaks(n = 6),labels = comma)
qp3 <- qp3 + theme(axis.text.x = element_text(angle = 30, hjust = 1))
qp3 <- qp3 + xlab(expression("Date")) #+ scale_y_log10()
qp3 <- qp3 + ylab(expression("US daily cases"))
#qp3 <- qp3 + expand_limits(x=c(pdata[1,1], as.Date("2020-04-08")))
#qp3 <- qp3 + labs(caption=paste(c(latestp," cases\n",percp,"% of population","\n",qkp," cases per day","\nLast update: ",as.character(tail(pdata[,1],1)),"\nData source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)"), collapse = ""))
#qp3 <- qp3 + theme(plot.caption=element_text(size=8, hjust=0, margin=margin(12,0,0,0)))

qp4 <- ggplot(dodata, aes(x=date, y=diff))
qp4 <- qp4 + theme_bw(base_size = fsize) #+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
qp4 <- qp4 + geom_point(color="red", size=psize)
qp4 <- qp4 + geom_smooth(color="gray50", method="gam", size=lsize)
#qp4 <- qp4 + stat_smooth(data=subset(pdata, date >= qfp),method="lm", color="gray40", size=lsize, se = FALSE, level = 0.95)
#qp4 <- qp4 + stat_function(fun = function(x) fModel(x, k=coef(lmodelp)["Xp"], d=coef(lmodelp)["(Intercept)"]), size=lsize, color="gray40")
#qp4 <- qp4 + geom_line(data = modelfit, aes(date, y=cases), color="firebrick", size=lsize)
#qp4 <- qp4 + stat_function(fun = function(x) fModel(x, a=qa, b=qb, c=qc), size=lsize, color="firebrick")
qp4 <- qp4 + scale_x_date(date_breaks = "2 weeks", date_labels = "%Y/%m/%d") # + scale_y_log10()
qp4 <- qp4 + scale_y_continuous(breaks = scales::pretty_breaks(n = 6),labels = comma)
qp4 <- qp4 + theme(axis.text.x = element_text(angle = 30, hjust = 1))
qp4 <- qp4 + xlab(expression("Date")) #+ scale_y_log10()
qp4 <- qp4 + ylab(paste("Daily cases in",country))
#qp4 <- qp4 + expand_limits(x=c(pdata[1,1], as.Date("2020-04-08")))
#qp4 <- qp4 + labs(caption=paste(c(latestp," cases\n",percp,"% of population","\n",qkp," cases per day","\nLast update: ",as.character(tail(pdata[,1],1)),"\nData source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)"), collapse = ""))
#qp4 <- qp4 + theme(plot.caption=element_text(size=8, hjust=0, margin=margin(12,0,0,0)))


gridimg <- arrangeGrob(qp1, qp2, qp3, qp4, ncol=2)
grid.draw(gridimg)
gridus <- arrangeGrob(qp1, qp3, ncol=1)
gridother <- arrangeGrob(qp2, qp4, ncol=1)

ggsave(gridimg, file=paste(c(path,"cases_us_",country,".png"), collapse = ""), width = 16, height = 12, dpi=300)
ggsave(gridus, file=paste(c(path,"cases_us",".png"), collapse = ""), width = 8, height = 12, dpi=300)
ggsave(gridother, file=paste(c(path,country,".png"), collapse = ""), width = 8, height = 12, dpi=300)


