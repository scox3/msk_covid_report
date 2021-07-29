# Module parse telegram



#' 
#' The second function will check the node to contain the data I seek. The following code decisions should be noted:\
#' - The text will be transliterated from Russian with the function stri_trans_general() from the package stingi for regexp to work;\
#' - The extraction itself will be done by the function str_extract() from the package stringr.\
#' - I will use purrr chained operation to streamline the code.
#' 
try_to_extract_info_json <- function(text_node, curr_date ) {
  
  #Selection
  
  s <- text_node %>%
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




library(jsonlite)

parse_telegram_json_file <- function(fname) {
  
  json1 <- read_json(fname ) 
  

  n <- length(json1$messages)
  data1 <- vector( n, mode="list")
  i <- 1
  n0 <- 0
  curr_date <- NULL
  
  while( i <= n ) {
    
    curr_date <- as.Date( json1$messages[[i]]$date)
    
    text1 <- unlist( json1$messages[[i]]$text, use.names = FALSE ) %>% 
      str_subset("bold", negate=TRUE) %>% paste(collapse = "") 
    
    
    s <- try_to_extract_info_json(text1, curr_date)
    if( !is.null(s)) {
        n0 <- n0+1
        data1[[n0]] <- data.frame(s)
        data1[[n0]]$date <- curr_date
    }
    
    i <- i+1
  }
  
  dt.data <- rbindlist( data1[1:n0])
  return( dt.data)
  
}


