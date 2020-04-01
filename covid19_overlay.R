#Libraries
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
library(nls2)
library(nlstools) #Tools for Nonlinear Regression Analysis

#Plot current COVID-19 cases in US and other countries
#(c) Alexander Johs
#Last updated 3/30/2020

#Clear plot window
graphics.off()

path <- "~/R/covid/"
#Set working directory to current path
setwd(path)

#Select country
country <- "Italy"
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

#Model fit
Xp <- as.numeric(rownames(pdata))
Yp <- pdata[,2]
spdata <- cbind.data.frame(Xp,Yp)

Xo <- as.numeric(rownames(odata))
Yo <- odata[,2]
sodata <- cbind.data.frame(Xo,Yo)

modelfitp <- nls2(formula = Yp ~ I(a*exp(b*(Xp-c))),  data = na.omit(spdata[64:ncol(case),]), start = list(a = 0.013, b = 0.15, c = 1), control = nls.control(maxiter = 1000, tol = 1e-02, minFactor = 1/1024, printEval = TRUE, warnOnly = TRUE), algorithm = "port")
modelfito <- nls2(formula = Yo ~ I(a*exp(b*(Xo-c))),  data = na.omit(sodata[64:ncol(case),]), start = list(a = 0.013, b = 0.15, c = 1), control = nls.control(maxiter = 1000, tol = 1e-02, minFactor = 1/1024, printEval = TRUE, warnOnly = TRUE), algorithm = "port")

qp <- summary(modelfitp)
print(qp)

qo <- summary(modelfito)
print(qo)

qpa <- qp$coefficients[1,1]
qpb <- qp$coefficients[2,1]
qpc <- qp$coefficients[3,1]

qoa <- qo$coefficients[1,1]
qob <- qo$coefficients[2,1]
qoc <- qo$coefficients[3,1]

#exponential.model <- lm(log(pdata$cases)~ pdata$date)
#print(summary(exponential.model))
#modelfit <- data.frame(date = pdata[,1], cases = exp(predict(exponential.model,list(Time=pdata[,1]))))

#Define function for plots
fModel <- function(x, a, b, c) {
  (a*exp(b*(x-c)))
}

#Plot the data
fsize <- 24
psize <- 5
lsize <- 2

#Predictions
tdays <- 77 #Last day
tdate <- pdata [1,1] + tdays

predcasesp <- trunc(fModel(tdays, a=qpa, b=qpb, c=qpc))
predcaseso <- trunc(fModel(tdays, a=qoa, b=qob, c=qoc))

qp1 <- ggplot(spdata, aes(x=Xp, y=Yp))
qp1 <- qp1 + geom_point(data = spdata, aes(x=Xp, y=Yp, color='US'), color="dodgerblue", size=psize)
qp1 <- qp1 + geom_point(data = sodata, aes(x=Xo, y=Yo, color='Italy'), color="red", size=psize)
qp1 <- qp1 + stat_function(fun = function(x) fModel(x, a=qpa, b=qpb, c=qpc), size=lsize, color="dodgerblue4")
qp1 <- qp1 + stat_function(fun = function(x) fModel(x, a=qoa, b=qob, c=qoc), size=lsize, color="firebrick")
qp1 <- qp1 + labs(color="Country")
qp1 <- qp1 + theme_bw(base_size = fsize) + theme(legend.position="bottom")#+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
qp1 <- qp1 + scale_y_continuous(breaks = scales::pretty_breaks(n = 10), sec.axis = sec_axis(~., name=paste("Number of cases in",country)))
#qp1 <- qp1 + scale_y_continuous(breaks = scales::pretty_breaks(n = 8))
qp1 <- qp1 + theme(axis.text.x = element_text(angle = 30, hjust = 1))
qp1 <- qp1 + xlab(paste(c("Extrapolation to ", format(tdate, format = "%m/%d/%y")), collapse="")) #+ scale_y_log10()
qp1 <- qp1 + ylab(expression("Number of cases in the US"))
qp1 <- qp1 + labs(caption=paste("Last update:",tail(pdata[,1],1),"    Data source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)"))
qp1 <- qp1 + theme(plot.caption=element_text(size=8, hjust=0, margin=margin(16,0,0,0)))
qp1 <- qp1 + expand_limits(x=c(0,tdays))
qp1 <- qp1 + scale_x_continuous(breaks=NULL)
qp1 <- qp1 + annotate("text", label = paste(c("US cases: ",predcasesp," -"), collapse = ""), x = tdays-16, y = predcasesp, size=fsize*0.25)

gridimg <- arrangeGrob(qp1,ncol=1)
grid.draw(gridimg)
ggsave(gridimg, file=paste(c(path,"cases_overlay",".png"), collapse = ""), width = 16, height = 8, dpi=600)
