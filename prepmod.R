library(dplyr)
library(readr)
library(stringr)
library(rvest)
library(rlang)
library(aws.s3)


###set a regional or broad base of clinics to scrape

 base_url <-  'https://cw2-alaska-production.herokuapp.com/clinic/search?location=&search_radius=All&search_name=&search_date=&vaccine_name=&commit=Search'

 
 ####################################################################
 ##create URLS from the number of pages
 ####################################################################

getUrls <- function(base_url) {
  
base_url_html <- read_html(base_url) 
nav <- base_url_html %>% html_nodes('nav.pagy-nav span.page')
nav_data <- nav %>% html_text()
nav_data_num <- as.numeric(nav_data)
 
max_pages <- max(nav_data_num, na.rm = T)
pages_vector <- 1:max_pages ###need to loop over this.!!

createUrlStrings <- function(page)  {
  return(paste0(base_url, "&page=",page, "#search_results")) 
  
}

urls <- lapply(pages_vector, FUN=createUrlStrings)
return(urls)###this is a list of urls to looop over.
}

pmUrls <- getUrls(base_url)### need to loop over these.  - all the URLS of the pages


####################################################################
##scrape and generate a dataframe 
####################################################################


readHtml <- function(url) { ####looping through each url - each different page of results. 

  prepmod_divs <- read_html(url) %>% html_nodes('div.field-fullwidth div.flex-1')

    text <- prepmod_divs %>% html_text() ###this is not used 

  createDf <- function (rawDiv) { ####loops through each DIV - each clinic listing.
    
    link <- rawDiv %>% html_nodes('a.button-primary') %>%  html_attr(name="href")
    clinic_url <- ifelse(!is_empty(link), paste0("https://cw2-alaska-production.herokuapp.com",link), "none")

    rawTextEach <- rawDiv %>% html_text()
    
    hasAppts <- ifelse(str_detect(rawTextEach, "Available Appointments:"), "has_appointments_listed", "no_appointments_listed")
    appt_available <- ifelse(hasAppts == "has_appointments_listed", substr(rawTextEach, regexpr("Available Appointments: ", rawTextEach)+23, regexpr("Available Appointments: ", rawTextEach)+26) ,"nothing listed" )
    
    appt_available <- str_remove(appt_available, "\n")
    appt_available <- str_trim(appt_available)
    
    delim <- read_delim(rawTextEach, delim = "\n", col_names =F, skip=0, n_max = 4) ## only 4 rows.
    
    df <- data.frame(delim)
    names(df) <- c("text")
    df <-  df %>% mutate_if(is.character, str_trim)
    df <- t(df)
    df <- data.frame(df)
    names(df) <- c("location", "address", "offered", "ages")
    df <- df %>% mutate(appts_available = appt_available)
    df <- df %>% mutate(clinic_registration_url = clinic_url)
    return (df)
  }  
  
  df_output <- lapply(prepmod_divs, FUN= createDf) %>% bind_rows()
  return(df_output) 

  Sys.sleep(5)
  
}###end of the read HTML fucntion. 

####################################################################
##Generate a full data frame from the scraped data by looping through urls
####################################################################

complete_df <- lapply(pmUrls, FUN=readHtml) %>% bind_rows()

complete_df <- complete_df %>% mutate(time_downloaded = Sys.time())
complete_df <- complete_df %>% mutate(offered = str_remove(offered, "Vaccinations offered:"))
complete_df <- complete_df %>% arrange(desc(appts_available))
complete_df <- complete_df %>% select(-ages)
complete_df <- complete_df %>% mutate (date = str_sub(location, nchar(location)-10, nchar(location)))
complete_df <- complete_df %>% mutate (location = str_sub(location, 1,nchar(location)-14 ))


####################################################################
##filter to find only clinics with appointments
####################################################################


complete_df <- complete_df %>% relocate(date, .before = location)

only_available <- complete_df %>% filter (appts_available > 0)

anchorage <- complete_df %>% filter (appts_available > 0 & clinic_registration_url !="none" & str_detect(address, "Anchorage"))


write_csv(complete_df, "complete_prepmod.csv")
write_csv(only_available, "only_available.csv")
write_csv(anchorage, "anchorage_prepmod.csv")




####################################################################
##PUSH TO AWS S3
####################################################################

my_key <- "YOUR AWS KEY"
key_secret <- "YOUR AWS SECRET"

Sys.setenv("AWS_ACCESS_KEY_ID" = my_key,
           "AWS_SECRET_ACCESS_KEY" = key_secret,
           "AWS_DEFAULT_REGION" = "YOUR REGION")

put_object(file="complete_prepmod.csv", object ="complete_prepmod.csv",  bucket = "YOUR BUCKET")
put_object(file="only_available.csv", object ="only_available.csv",  bucket = "YOUR BUCKET")
put_object(file="anchorage_prepmod.csv", object ="anchorage_prepmod.csv",  bucket = "YOUR BUCKET")



 


 