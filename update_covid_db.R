#' ---
#' title: "Dataset compilation: Telegram and HTML parsing for COVID-19 epidemic data"
#' author: "Vladislav Borkus"
#' date: 2024-03-24T10:10:00-00:00
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
#' tags: ["R", "RStudio", "COVID19", "XML", "Tools", "Howto", "Packages"]
#' ---
#' 
#' The data about hospitalizations with COVID-19 in Moscow are hard to find in public datasets. However these data are daily published in the specialized Telegram channel, so they can be parsed from there to be used in data analysis.
#' 
#' The first step is to download Telegram chat stream. That can be easily done with the desktop [Telegram client](https://winaero.com/export-chat-history-file-telegram-desktop/) so I will skip this step and assume that the necessary data, i.e. a set of files "messages.html" is ready ([here is the copy](https://drive.google.com/drive/folders/1RxlKL6kQ1FtsFyg9v-AutkkwGJbdNnja?usp=sharing)). Below I will explain how to convert them into a dataset.
#' 


#' Load libraries.
#' 
#' I will need the usual stuff plus the package rvest to load HTML data and XML2 to process the HTML DOM tree.
#' 
## ----message=FALSE, warning=FALSE---------------------------------------------------
#Usual libraries
library(purrr)
library(stringi)
library(stringr)
library(data.table)
library(ggplot2)
library(lubridate)

#HTML processing
library(rvest)
library(xml2)

#For ACF plots
library(forecast)

#Regression trees
library(rpart)



# s.wd <- getwd()
# if( str_detect(s.wd, "Update1") ) {
#   setwd("..")
# }



library(rprojroot)
global.root.folder <- rprojroot::is_rstudio_project$find_file()

source(paste0(global.root.folder, "/module_parse_telegram.R"))

##PARAMS
f.rebuild.db <- FALSE
f.update.db <- FALSE


filename.data.path <- paste0(global.root.folder, "/data/")
filename.updated.db <- paste0(filename.data.path, 
                              "covid-moscow-dataset.csv")

cyclic.comp.st.date <- make_date(2021,1,18)

msk_covid_db_rebuild <- function() {
  
}



if( f.rebuild.db ) {
  dt.data2 <- parse_messages_file( paste0(filename.data.path, "messages2.html"))
  dt.data2 <- dt.data2[ date > make_date(2020,07,01) & !is.na(new_diag)]
  
  dt.data3 <- parse_messages_file( paste0(filename.data.path, "messages3.html"))
  dt.data3 <- dt.data3[ !is.na(new_diag) | !is.na(new_dead)]
  dt.data4 <- parse_messages_file( paste0(filename.data.path, "messages4.html"))
  
  
  dt.data0 <- read.csv(paste0(filename.data.path, "covid-moscow-dataset_mar2021.csv"),
                       stringsAsFactors = FALSE)
  dt.data0 <- as.data.table( dt.data0[ , -1])
  dt.data0[ , date := as_date(date)]
  # 
  
  dt.data <- rbindlist( list(dt.data2, dt.data0,dt.data3, dt.data4))
  setorder(dt.data, "date")
  dt.data <- unique( dt.data)
  dt.data[ , .(n=.N), by=date][n>1]
  
  ##Parsing errors
  dt.data <- dt.data[ new_hosp>50 & !is.na(new_diag)]
  write.csv(dt.data, file=filename.updated.db)
} else if(f.update.db) {
  
  dt.data4 <- parse_messages_file( paste0(filename.data.path, "messages_upd.html"))
  
  dt.data.latest <- read.csv(paste0(filename.data.path, "covid-moscow-dataset.csv"),
                             stringsAsFactors = FALSE)
  dt.data.latest <- as.data.table( dt.data.latest[ , -1])
  dt.data.latest[ , date := as_date(date)]
  
  dt.data <- rbindlist( list(dt.data.latest, dt.data4))
  setorder(dt.data, "date")
  dt.data <- unique( dt.data)
  dt.data[ , .(n=.N), by=date][n>1]
  ##Parsing errors
  dt.data <- dt.data[ new_hosp>50 & !is.na(new_diag)]
  write.csv(dt.data, file=filename.updated.db)
  
} else {
  dt.data <- read.csv(filename.updated.db,
                      stringsAsFactors = FALSE)
  dt.data <- as.data.table( dt.data[ , -1])
  dt.data[ , date := as_date(date)]
}

#' 
## -----------------------------------------------------------------------------------
#try_to_extract_info(nodes1[[3]])


