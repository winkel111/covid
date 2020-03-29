#Libraries
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
library(nls2)
library(nlstools) #Tools for Nonlinear Regression Analysis

#Plot current COVID-19 cases in US and other countries
#(c) Alexander Johs
#Last updated 3/26/2020

#Clear plot window
graphics.off()

path <- "~/R/covid/"
#Set working directory to current path
setwd(path)

#Select country
country <- "Germany"
state <- ""

#case <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"), col_types = cols())
#case <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"), col_types = cols())
case <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"), col_types = cols())

#Fix data for US
cases_us1 <- case %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::filter(`Country/Region` == "US") %>%
  dplyr::filter(`Province/State` != "Diamond Princess") %>%
  dplyr::filter(`Province/State` != "Grand Princess") %>%
  select(5:52) %>%
  colSums()

cases_us2 <- case %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::filter(`Country/Region` == "US") %>%
  dplyr::filter(`Province/State` != "Diamond Princess") %>%
  dplyr::filter(`Province/State` != "Grand Princess") %>%
  #dplyr::filter(`Province/State` %in% state.name) %>%
  select(53:ncol(case)) %>%
  colSums()

cases <- c(cases_us1, cases_us2)

if (state != "") {
  cases_other <- case %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    dplyr::filter(`Country/Region` == country) %>%
    dplyr::filter(`Province/State` == state) %>%
    select(52:ncol(case)) %>%
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

#Plot the data
fsize <- 24
psize <- 5
lsize <- 2

#Function to create minor ticks
#insert_minor <- function(major_labs, n_minor) {labs <- 
#  c( sapply( major_labs, function(x) c(x, rep("", 4) ) ) )
#labs[1:(length(labs)-n_minor)]}

qp1 <- ggplot(pdata, aes(x=date, y=cases))
qp1 <- qp1 + theme_bw(base_size = fsize) #+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
qp1 <- qp1 + geom_point(color="blue", size=psize)
qp1 <- qp1 + stat_smooth(data=subset(pdata, date >= "2020-03-20"),method="lm", color="gray40", size=lsize, se = FALSE, level = 0.95)
#qp1 <- qp1 + geom_line(data = modelfit, aes(date, y=cases), color="firebrick", size=lsize)
#qp1 <- qp1 + stat_function(fun = function(x) fModel(x, a=qa, b=qb, c=qc), size=lsize, color="firebrick")
qp1 <- qp1 + scale_x_date(date_breaks = "2 weeks", date_labels = "%Y/%m/%d") # + scale_y_log10()
qp1 <- qp1 + scale_y_continuous(breaks = scales::pretty_breaks(n = 6))
qp1 <- qp1 + theme(axis.text.x = element_text(angle = 30, hjust = 1))
qp1 <- qp1 + xlab(expression("Date")) #+ scale_y_log10()
qp1 <- qp1 + ylab(expression("Number of cases in the US"))
qp1 <- qp1 + labs(caption=paste("Last update:",tail(pdata[,1],1),"    Data source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)"))
qp1 <- qp1 + theme(plot.caption=element_text(size=8, hjust=0, margin=margin(16,0,0,0)))

qp2 <- ggplot(odata, aes(x=date, y=cases))
qp2 <- qp2 + theme_bw(base_size = fsize) #+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
qp2 <- qp2 + geom_point(color="firebrick", size=psize)
#qp2 <- qp2 + stat_smooth(data=subset(pdata, date >= "2020-03-20"),method="lm", color="gray40", size=lsize, se = FALSE, level = 0.95)
#qp2 <- qp2 + xlim(min(pdata$date),max(pdata$date))
qp2 <- qp2 + scale_x_date(date_breaks = "2 weeks", date_labels = "%Y/%m/%d") # + scale_y_log10()
qp2 <- qp2 + scale_y_continuous(breaks = scales::pretty_breaks(n = 6))
qp2 <- qp2 + theme(axis.text.x = element_text(angle = 30, hjust = 1))
qp2 <- qp2 + xlab(expression("Date")) #+ scale_y_log10()
qp2 <- qp2 + ylab(paste("Number of cases in",country))
qp2 <- qp2 + labs(caption=paste("Last update:",tail(pdata[,1],1),"    Data source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)"))
qp2 <- qp2 + theme(plot.caption=element_text(size=8, hjust=0, margin=margin(16,0,0,0)))

gridimg <- arrangeGrob(qp1, qp2, ncol=2)
grid.draw(gridimg)
ggsave(gridimg, file=paste(c(path,"cases",".png"), collapse = ""), width = 16, height = 8, dpi=600)
ggsave(qp1, file=paste(c(path,"cases_us",".png"), collapse = ""), width = 8, height = 8, dpi=600)
ggsave(qp2, file=paste(c(path,country,".png"), collapse = ""), width = 8, height = 8, dpi=600)
