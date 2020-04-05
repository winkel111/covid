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
#Last updated 4/5/2020

#Clear plot window
graphics.off()

path <- "~/R/covid/"
#Set working directory to current path
setwd(path)

#Select country
country <- "US"
state <- "Kentucky"

#case <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"), col_types = cols())
#case <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"), col_types = cols())
case <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"), col_types = cols())
casus <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"), col_types = cols())
#casus <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"), col_types = cols())

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
sdata <- cbind.data.frame(Xp,Yp)

Xo <- as.numeric(rownames(odata))
Yo <- odata[,2]
sodata <- cbind.data.frame(Xo,Yo)

#Linear model fit over the 4 most recent days
fstartp <- nrow(sdata)-3
fstarto <- nrow(sodata)-3

lmodelp <- lm(formula = Yp ~ Xp,  data = na.omit(sdata[fstartp:nrow(sdata),]))
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
fsize <- 24
psize <- 5
lsize <- 2

#Start for fit
qfp <- tail(pdata[,1],1)-3
qfo <- tail(odata[,1],1)-3

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
qp1 <- qp1 + labs(caption=paste(qkp,"cases per day","\nLast update:",tail(pdata[,1],1),"\nData source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)"))
qp1 <- qp1 + theme(plot.caption=element_text(size=8, hjust=0, margin=margin(12,0,0,0)))

qp2 <- ggplot(odata, aes(x=date, y=cases))
qp2 <- qp2 + theme_bw(base_size = fsize) #+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
qp2 <- qp2 + geom_point(color="firebrick", size=psize)
qp2 <- qp2 + stat_smooth(data=subset(odata, date >= qfo),method="lm", color="gray40", size=lsize, se = FALSE, level = 0.95)#qp2 <- qp2 + stat_smooth(data=subset(pdata, date >= "2020-03-20"),method="lm", color="gray40", size=lsize, se = FALSE, level = 0.95)
#qp2 <- qp2 + xlim(min(pdata$date),max(pdata$date))
qp2 <- qp2 + scale_x_date(date_breaks = "2 weeks", date_labels = "%Y/%m/%d") # + scale_y_log10()
qp2 <- qp2 + scale_y_continuous(breaks = scales::pretty_breaks(n = 6),labels = comma)
qp2 <- qp2 + theme(axis.text.x = element_text(angle = 30, hjust = 1))
qp2 <- qp2 + xlab(expression("Date")) #+ scale_y_log10()
qp2 <- qp2 + ylab(paste("Cases in",country))
qp2 <- qp2 + labs(caption=paste(qko,"cases per day","\nLast update:",tail(odata[,1],1),"\nData source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)"))
qp2 <- qp2 + theme(plot.caption=element_text(size=8, hjust=0, margin=margin(12,0,0,0)))

gridimg <- arrangeGrob(qp1, qp2, ncol=2)
grid.draw(gridimg)
ggsave(gridimg, file=paste(c(path,"cases",".png"), collapse = ""), width = 16, height = 8, dpi=600)
ggsave(qp1, file=paste(c(path,"cases_us",".png"), collapse = ""), width = 8, height = 8, dpi=600)
ggsave(qp2, file=paste(c(path,country,".png"), collapse = ""), width = 8, height = 8, dpi=600)
