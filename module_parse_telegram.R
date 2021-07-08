# Module parse telegram

try_to_extract_date <- function(node) {
  if( xml_find_first( node, xpath="@class='body details'") )   {
    #this is a date record
    
    loc1 <- Sys.getlocale(category = "LC_TIME")
    Sys.setlocale(category = "LC_TIME")
    Sys.setlocale("LC_TIME", "English") # Win 
    
    date1 <- node %>% xml_text() %>%
      trimws("both") %>%
      str_replace_all("\\s", "") %>%
      as.Date("%d%B%Y")
    
    #print( paste0("AA,", date1, " - ", node %>% xml_text() %>%
    #         trimws("both") )  )
    
    Sys.setlocale("LC_TIME", loc1)
    return(date1)
  } else {
    return(NULL)
  }
}

#try_to_extract_date(nodes1[[1]])



#' 
#' The second function will check the node to contain the data I seek. The following code decisions should be noted:\
#' - The text will be transliterated from Russian with the function stri_trans_general() from the package stingi for regexp to work;\
#' - The extraction itself will be done by the function str_extract() from the package stringr.\
#' - I will use purrr chained operation to streamline the code.
#' 
try_to_extract_info <- function(node, curr_date ) {
  
  #Selection
  
  s <- xml_text( node) %>%
    stri_trans_general("russian-latin/bgn") %>%
    str_replace_all("\\s", "") %>%
    str_to_lower()
  
  
  if( !str_detect(s, "gospitalizirov") ) {
    return(NULL)
  }
  
  #Extraction
  
  new_hosp <- s %>% 
    str_extract("gospitalizirov\\D+\\d{1,6}\\D") %>%
    str_extract("\\d+") %>%
    as.numeric()
  
  if( curr_date <= make_date(2021,06,15)) {
    new_diag <- s %>% 
      str_extract("podtverzhd\\D+\\d{1,6}") %>%
      str_extract("\\d+") %>%
      as.numeric()
    
    curr_ivl <- s %>% 
      str_extract("naivlvbol\\D+\\d{1,6}\\D") %>%
      str_extract("\\d+") %>%
      as.numeric()
    
    # print("old")    
  } else {
    new_diag <- s %>% 
      str_extract("vyyavleno\\d{1,6}novykh") %>%
      str_extract("\\d+") %>%
      as.numeric() 
    
    curr_ivl <- s %>% 
      str_extract("naivl\\D+\\d{1,6}chelovek") %>%
      str_extract("\\d+") %>%
      as.numeric()
    
  }
  
  
  new_dead <- s %>% 
    str_extract("skonch\\D+\\d{1,6}\\D") %>%
    str_extract("\\d+") %>%
    as.numeric()
  
  return( list(new_hosp=new_hosp, new_diag=new_diag,
               curr_ivl=curr_ivl, new_dead=new_dead))     
  
}



parse_messages_file <- function(fname) {
  
  html1 <- read_html(fname)
  #' 
  #' To check if the data are correctly loaded the function `xml_name()` may be used. It will show the name of the root node. It is `html` in this case so everything looks ok.
  #' 
  ## -----------------------------------------------------------------------------------
  # xml_name(html1)
  
  #' The following steps are bases on the knowledge of the DOM structure that can be viewed in the Chrome or Firefox browsers after pressing F12 key.
  #' 
  #' The task is to find the first `div` node in the document that satisfies the search criteria:\
  #' - the element should have the attribute "class"" (that is indicated by `@class` construction in the xpath query);\
  #' - the "class" attribute value should be equal to "history".
  #' 
  #' To indicate that the search must be performed globally, the xpath query starts with "`//`."
  #' 
  #' The resulting query looks like this:
  #' 
  ## -----------------------------------------------------------------------------------
  div_history <- xml_find_first(html1, xpath="//div[@class=\"history\"]")
  
  #' 
  ## -----------------------------------------------------------------------------------
  #capture <- capture.output( print(div_history) )
  #head(capture, 7)
  
  #' 
  #' The structure of the stream is following:\
  #' - The nodes with the class "message service" are the ones that containing the publication date.\
  #' - The messages with the information we seek have the class attribute "message default clearfix joined" (i.e. text with a picture).
  #' 
  #' Below I use the function xml_find_all() with the insane хpath argument to extract both nodes containing dates and nodes with COVID cases information. The symbol "\|" in the `xpath` merges the two search conditions.
  #' 
  ## -----------------------------------------------------------------------------------
  nodes1 <- xml_find_all(div_history, 
                         
                         xpath=".//div[@class='message service']/div[@class='body details'] | .//div[@class='message default clearfix joined']/div[@class='body']/div[@class='text'] |.//div[@class='message default clearfix']/div[@class='body']/div[@class='text']") 
  
  #' 
  #' Now is the time to create the new function `try_to_extract_date()` that tests the node if it contains a date of the publication and in that case it tries to extract the date.
  #' 
  #' The following needs to be noted:\
  #' - I will use the specific form of the argument `xpath` to force `xml_find_first()` to check the node to satisfy the criteria but not to select the node. The returned value will be logical;\
  #' - the function `xml_text()` will be used to extract a date in text format;\
  #' - All whitespaces will be removed from the date string;\
  #' - I will temporarily switch to USA time locale for type conversion in `as.Date().` This is necessary because the dates in Telegram stream are published in the USA locale, and my R is running in Russian locale;\
  #' - `%B` symbol in the `as.Date()` format argument is used to indicate that a month is supplied by a name not by its number.
  #' 
  ## -----------------------------------------------------------------------------------
  
  #' 
  #' Now it is time to iterate over the nodes list extracting either date or data and gluing them into a single data frame.
  #' 
  ## -----------------------------------------------------------------------------------
  data1 <- vector( length(nodes1), mode="list")
  i <- 1
  n0 <- 0
  n <- length(nodes1)
  curr_date <- NULL
  
  while( i <= n ) {
    
    s <-try_to_extract_date( nodes1[[i]]) 
    if( !is.null(s)) {
      curr_date <- s 
    } else {
      # print(curr_date)
      s <-try_to_extract_info( nodes1[[i]], curr_date) 
      if( !is.null(s)) {
        n0 <- n0+1
        data1[[n0]] <- data.frame(s)
        data1[[n0]]$date <- curr_date
      }
    }
    i <- i+1
  }
  
  dt.data <- rbindlist( data1[1:n0])
  return( dt.data)
}



