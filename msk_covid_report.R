#' ---
#' title: "Dataset, MSK COVID-19 12 July 2021"
#' author: "Vladislav Borkus"
#' date: 2021-07-12T10:10:00-00:00
#' output:
#'   blogdown::html_page: 
#'     self_contained: no
#'     toc: yes
#'     keep_md: yes
#'     preserve_yaml: yes
#'   bookdown::html_document2: 
#'     css: style.css
#'     keep_md: yes
#'     self_contained: no
#'     preserve_yaml: yes
#' categories: ["R", "RStudio"]
#' tags: ["R", "RStudio", "COVID19", "Dataset"]
#' ---

#
#' This is a daily report on COVID-19 Moscow dataset.
#' Code source for this report is [here](https://github.com/scox3/msk_covid_report).
## -----

knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message=FALSE)


#Usual libraries
library(purrr)
library(stringi)
library(stringr)
library(data.table)
library(ggplot2)
library(lubridate)
library(tidyr)
library(zoo)


#For ACF plots
library(forecast)

#Regression trees
library(rpart)

#Find project root folder
library(rprojroot)
global.root.folder <- rprojroot::is_rstudio_project$find_file()

source(paste0(global.root.folder, "/module_parse_telegram.R"))

##PARAMS
f.rebuild.db <- FALSE
f.update.db <- FALSE

filename.data.path <- paste0(global.root.folder, "/data/")
filename.updated.db <- paste0(filename.data.path, 
                              "covid-moscow-dataset.csv")


#
#Read data
## -----------------------------------------------------------------------------------

dt.data <- read.csv(filename.updated.db,
                    stringsAsFactors = FALSE)
dt.data <- as.data.table( dt.data[ , -1])
dt.data[ , date := as_date(date)]


#' # Cyclicity estimates
dt.data[, ix := date- min(date)]
dt2 <- data.table( ix=c(1:dt.data[, max(ix)]))
dt2 <- merge( dt2, dt.data, on="ix", all.x=TRUE)
dt2[ is.na(date), date := min(dt.data$date)+ix]

dt2.fill <- tidyr::fill(dt2, "new_diag","new_hosp", .direction = "down")
rm(dt2)
##from monday
cyclic.comp.st.date <- make_date(2021,1,18)

ts.new_diag <- ts(as.data.table(dt2.fill)[ date >= cyclic.comp.st.date, 
                                           new_diag], frequency=7)
stl.log.new_diag <- stl(log(ts.new_diag), s.window = "periodic")

# weekdays(cyclic.comp.st.date)

vec.weekday_coef <- exp( stl.log.new_diag$time.series[1:7,1])
dt.data[ , new_diag_corr := new_diag/vec.weekday_coef[wday(date, week_start=1)]]

#Weekly cyclicity
cat("Weekly diag. cyclicity coeffs:", sprintf("%.2f", vec.weekday_coef), sep="; ")


ts.new_hosp <- ts(as.data.table(dt2.fill)[ date >= cyclic.comp.st.date, 
                                           new_hosp], frequency=7)
stl.log.new_hosp <- stl(log(ts.new_hosp), s.window = "periodic")
vec.weekday_coef_hosp <- exp( stl.log.new_hosp$time.series[1:7,1])
dt.data[ , new_hosp_corr := new_hosp/vec.weekday_coef_hosp[wday(date, week_start=1)]]

cat("Weekly hosp. cyclicity coeffs:", sprintf("%.2f", vec.weekday_coef_hosp), sep="; ")

ggplot( data.frame( x=c(1:7), cdiag = vec.weekday_coef, chosp = vec.weekday_coef_hosp),
        aes( x= x, y = cdiag, color="New. diags"))+
  geom_path(size=1)+
  geom_path(aes( x= x, y = chosp, color="New. hosp."), size=1)+
  xlab("Day of week")+
  ylab("Coeff")+
  ggtitle("New diag. and new hosp. cyclicity coeffs")+
  scale_color_discrete(name="Weekly cyclicity coeffs. for:")
#' 

#' From cyclicity coeffs we may guess that hospitalizations are really 5 days lagged relative to newly diagnosted.
## --------


#' # Trends
#' 
## ----------
ggplot(dt.data, aes(x=date, y=new_diag, color="Diagnosed"))+
  geom_path(size=1)+
  geom_path(aes(y=new_hosp, color="Hospitalized"), size=1)+
  ggtitle("COVID-19 MSK: Number of newly hospitalized and diagnosed patiens")+
  xlab("Date")+
  scale_color_discrete(name="")+
  ylab("Number of patients")+ 
  theme_bw()+
  scale_x_date(date_labels = "%d%-%m-%Y")
#' 



ggplot(dt.data, aes(x=date, y=new_diag, color="Diagnosed"))+
  geom_path(size=0.5)+
  geom_path(aes(y=new_diag_corr, color="Diagnosed,\nweekday corrected"), size=1)+
  ggtitle("COVID-19 MSK: Week cycle corrected number of newly diagnosed patients")+
  geom_smooth(aes(y=new_diag_corr, color="Decycled, loess"), span=0.1)+
  xlab("Date")+
  ylab("Number of patients")+ 
  theme_bw()+
  scale_x_date(date_labels = "%d%-%m-%Y")+
  scale_color_discrete(name="")
#' 

ggplot(dt.data, aes(x=date, y=new_diag, color="Diagnosed"))+ylim(1,1e4)+
  geom_path(size=0.5)+
  geom_path(aes(y=new_diag_corr, color="Diagnosed,\nweekday corrected"), size=1)+
  ggtitle("COVID-19 MSK: Week cycle corrected number of newly diagnosed patients\n(log scale)")+
  geom_smooth(aes(y=new_diag_corr, color="Decycled, loess"), span=0.1)+
  xlab("Date")+
  ylab("Number of patients")+ 
  theme_bw()+
  scale_x_date(date_labels = "%d%-%m-%Y")+
  scale_color_discrete(name="")+scale_y_log10()
#' 

ggplot(dt.data[ date>=make_date(2021,5,1)], aes(x=date, y=new_diag, color="Diagnosed"))+ylim(1,1e4)+
  geom_path(size=0.5)+
  geom_path(aes(y=new_diag_corr, color="Diagnosed,\nweekday corrected"), size=1)+
  ggtitle("COVID-19 MSK: Week cycle corrected number of newly diagnosed patients\n(zoomed)")+
  geom_smooth(aes(y=new_diag_corr, color="Decycled, loess"), span=0.25)+
  xlab("Date")+
  ylab("Number of patients")+ 
  theme_bw()+
  scale_x_date(date_labels = "%d%-%m-%Y")+
  scale_color_discrete(name="")
#' 


ggplot(dt.data, aes(x=date, y=(new_diag/shift(new_diag,7)-1)), color="Data")+
  geom_path()+
  geom_smooth(span=0.1)+
  ggtitle("COVID-19 MSK: Wow change in the number of newly diagnosed patiens")+
  xlab("Date")+
  scale_color_discrete(name="")+
  ylab("(N / lag(N,7)) -1")+ 
  theme_bw()+
  scale_x_date(date_labels = "%d%-%m-%Y")
#' 


rsum_cumsum <- function(x, n = 7L) {
  tail(cumsum(x) - cumsum(c(rep(0, n), head(x, -n))), -n + 1)
}


rsum_ratio <- function(x) {
  y <- rsum_cumsum(x)
  return( y/shift(y,7))  
}

dt.data[ , length(date[7:length(date)])]
dt.data[ , new_diag_week_av := (cumsum(new_diag)- shift(cumsum(new_diag),7 ))/7]

ggplot(dt.data, aes(x=date, y=new_diag_week_av/shift(new_diag_week_av,7)-1, color="Data"))+
  geom_path()+
  ggtitle("COVID-19 MSK: Wow change in the number of newly diagnosed patiens\n(week over week)")+
  xlab("Date")+
  scale_color_discrete(name="")+
  ylab("N(7d)/lag(N(7d),7)-1")+ 
  theme_bw()+
  scale_x_date(date_labels = "%d%-%m-%Y")
#' 

ggplot(dt.data, aes(x=date, y=new_hosp, color="N. hosp."))+
  geom_path(size=0.5)+
  geom_path(aes(y=new_hosp_corr, color="N. hosp.,\ndecycled"), size=1)+
  ggtitle("COVID-19 MSK: Cycle corrected number of newly hospitalized patients")+
  geom_smooth(aes(y=new_hosp_corr, color="N. hosp., decycled,\nloess (0.1)"), span=0.1, size=1)+
  xlab("Date")+
  ylab("Number of patients")+ 
  theme_bw()+
  scale_x_date(date_labels = "%d%-%m-%Y")+
  scale_color_discrete(name="")
#' 


ggplot(dt.data[date>make_date(2021,5,1)], aes(x=date, y=new_hosp, color="N. hosp."))+
  geom_path(size=0.5)+
  geom_path(aes(y=new_hosp_corr, color="N. hosp.,\ndecycled"), size=1)+
  ggtitle("COVID-19 MSK: Cycle corrected number of newly hospitalized patients")+
  geom_smooth(aes(y=new_hosp_corr, color="N. hosp., decycled,\nloess (0.25)"), span=0.25, size=1)+
  xlab("Date")+
  ylab("Number of patients")+ 
  theme_bw()+
  scale_x_date(date_labels = "%d%-%m-%Y")+
  scale_color_discrete(name="")
#' 



ggplot(dt.data, aes(x=date, y=new_diag, color="Diagnosed"))+
  geom_smooth(span=0.2)+
  geom_smooth(aes(y=new_hosp/0.45, color="Hospitalized / 0.45"), span=0.2)+
  ggtitle("COVID-19 MSK: Number of newly hospitalized (scaled) and\ndiagnosed patiens (loess, span=0.2)")+
  xlab("Date")+
  scale_color_discrete(name="")+
  ylab("Number of patients")+ 
  theme_bw()+
  scale_x_date(date_labels = "%d%-%m-%Y")
#' 

ggplot(dt.data, aes(x=date, y=curr_ivl, color="IVL"))+
  geom_path(size=1)+
  ggtitle("COVID-19 MSK: Number of IVL patiens")+
  xlab("Date")+
  scale_color_discrete(name="")+
  ylab("Number of patients")+ 
  theme_bw()+
  scale_x_date(date_labels = "%d%-%m-%Y")
#' 

#' # Latest data table
tail(dt.data[,.(date, new_diag, new_diag_corr, new_diag_week_av, new_hosp, new_hosp_corr)],7)
#' 




#' # Newly hospitalized vs. newly disagnosted relationship
## ----------

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#' 
#' The ratio of the number of hospitalizations to the number of new cases changes with time. The more sick people skip the smaller the ratio.
#' 
## ----------



dt.data[ , ratio.hosp.diagn := new_hosp/new_diag]
reg.tree <- rpart(ratio.hosp.diagn ~ date, data=dt.data,
                  control=rpart.control(cp = 0.02))
dt.data[ , hosp.rat.tree:= predict(reg.tree, dt.data)]

ggplot(dt.data, aes(x=date, y=new_hosp/new_diag, color="Data"))+
  geom_path(size=1)+
  geom_smooth(aes(color="Lowess"), size=1)+
  ggtitle("COVID-19 MSK: Ratio of the number of new hospitalizations to\n the number of new registered (diagnosed) cases")+
  xlab("Date")+
  ylab("Ratio")+
  geom_path(aes(y=hosp.rat.tree, color="Reg. tree"), size=1)+
  scale_color_discrete(name="") + 
  theme_bw()+
  scale_x_date(date_labels = "%d%-%m-%Y")
#' 


ggplot(dt.data, aes(x=date, y=new_hosp/shift(new_diag, 5, type="lag"), color="Data"))+
  geom_path(size=1)+
  geom_smooth(aes(color="Lowess"), size=1)+
  ggtitle("COVID-19 MSK: Ratio of the number of new hospitalizations to\n the number of new registered (diagnosed) cases, 5d shift")+
  xlab("Date")+
  ylab("Ratio")+
  scale_color_discrete(name="") + 
  theme_bw()+
  scale_x_date(date_labels = "%d%-%m-%Y")
#' 

ggplot(dt.data, aes(x=date, y=new_diag, color="Diagnosed"))+
  geom_path(size=1)+
  geom_path(aes(y=new_hosp/0.45, color="Hospitalized / 0.45"), size=1)+
  ggtitle("COVID-19 MSK: Number of newly hospitalized (scaled) and diagnosed patients")+
  xlab("Date")+
  ylab("Number of patients")+ 
  theme_bw()+
  scale_x_date(date_labels = "%d%-%m-%Y")+
  scale_color_manual(name="", breaks=c("Diagnosed", "Hospitalized / 0.45"),
                     values=cbbPalette[2:3])
#' 

ggplot(dt.data, aes(x=new_diag, y=new_hosp))+
  geom_point()+
  geom_smooth(span=0.2)+
  ggtitle("COVID-19 MSK: Number of newly hospitalized vs diagnosed patiens")+
  xlab("New cases")+
  scale_color_discrete(name="")+
  ylab("New hospitalizations")+ 
  theme_bw()
#' 


ggplot(dt.data, aes(x=log(new_diag), y=log(new_hosp)))+
  geom_point()+
  geom_smooth(span=0.4)+
  ggtitle("COVID-19 MSK: Number of newly hospitalized vs diagnosed patiens")+
  xlab("New cases")+
  scale_color_discrete(name="")+
  ylab("New hospitalizations")+ 
  theme_bw()
#' 

lm1 <- lm(log(new_hosp) ~ log(new_diag),data=dt.data)
summary( lm1)
qqnorm(residuals(lm1))
qqline(residuals(lm1))
#' 

# ggplot(dt.data, aes(x=shift(new_diag, 7, "lag"), y=new_hosp))+
#   geom_point()+
#   geom_smooth(span=0.4)+
#   ggtitle("COVID-19 MSK: Number of newly hospitalized vs diagnosed patiens, lag 7")+
#   xlab("New cases")+
#   scale_color_discrete(name="")+
#   ylab("New hospitalizations")+ 
#   theme_bw()


##' The same is true for deceased time series. 
##' 
ggplot(dt.data, aes(x=date, y=new_diag*0.03, color="Diagnosed*0.03"))+
  geom_path(size=1)+
  geom_path(aes(y=new_dead, color="Deceased"), size=1)+
  ggtitle("COVID-19 MSK: Number of newly deceased and diagnosed patients (scaled) ")+
  xlab("Date")+
  ylab("Number of patients")+ 
  theme_bw()+
  scale_x_date(date_labels = "%d%-%m-%Y")+
  scale_color_manual(name="", breaks=c("Diagnosed*0.03", "Deceased"),
                     values=cbbPalette[2:3])
#' 

ggplot(dt.data, aes(x=date, y=new_hosp*0.067, color="Hospitalized*0.067"))+
  geom_path(size=1)+
  geom_path(aes(y=new_dead, color="Deceased"), size=1)+
  ggtitle("COVID-19 MSK: Number of newly deceased and hospitalized patients (scaled) ")+
  xlab("Date")+
  ylab("Number of patients")+ 
  theme_bw()+
  scale_x_date(date_labels = "%d%-%m-%Y")+
  scale_color_manual(name="", breaks=c("Hospitalized*0.067", "Deceased"),
                     values=cbbPalette[2:3])
#' 



## ----------



#' # Cyclicity decomposition diagnostic plots


plot( stl.log.new_diag, main="New diag. cyclicity decomposition")
sd( stl.log.new_diag$time.series[,3])
#' 

plot( stl.log.new_diag$time.series[1:7,1] / sd( stl.log.new_diag$time.series[,3]),
      main="Relative significance of cyclic component", type="l")
#' 

#Main+random (latest)
tail(exp( stl.log.new_diag$time.series[,2] + stl.log.new_diag$time.series[,3]))
#' 
tail(dt.data[, new_diag_corr])
#' 

#Main component (latest)
tail( exp( stl.log.new_diag$time.series[,2] ))
#' 

#Main+random
plot( exp( stl.log.new_diag$time.series[,2] + stl.log.new_diag$time.series[,3]),
      main="New patient without cycle component")
#' 

plot( density(stl.log.new_diag$time.series[,3]), main="Random component density")
abline(v=0, lty=2)
#' 

qqnorm(stl.log.new_diag$time.series[,3], 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="Random component Normality test") 
qqline(stl.log.new_diag$time.series[,3])
#' 

##
#Check
plot( stl.log.new_hosp, main="New. hosp. cyclic decomposition")
#' 


#' # ACF Plots
#' 
#' No clear indication for a time lag between the two series. However cross correlation function is more biases towards negative lags as it should be: diagnostic sightly precedes hospitalizations.
#' 
## -----------------------------------------------------------------------------------
ccf1 <- ccf(dt2.fill$new_diag, dt2.fill$new_hosp, 
            plot=FALSE)

df.ccf1 <- data.frame(ccf=ccf1$acf, lag=ccf1$lag)

ggplot(data.frame(ccf=ccf1$acf, lag=ccf1$lag), aes(lag, ccf))+
  geom_path(color="blue", size=1)+
  geom_vline(xintercept=0, linetype=2) +
  ggtitle("COVID-19 MSK: CCF new cases number ~ new hospitalizations number")+
  theme_bw()
#' 


#' 
#' No indication for periodicity in the auto correlation function (same is in logs).
#' 
## -----------------------------------------------------------------------------------
ggAcf(dt2.fill$new_diag)+
  ggtitle("MSK Covid-19: ACF of the time series the number of new cases")+ 
  theme_bw()
#' 


acf(dt2.fill$new_diag, type="partial")
#' 

