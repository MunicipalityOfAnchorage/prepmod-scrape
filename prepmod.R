library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(rvest)
library(rlang)
library(aws.s3)
library(httr)
library(jsonlite)



full_scrape_publish <- function (){

  
 base_url <-  'https://myhealth.alaska.gov/clinic/search?location=&search_radius=All&search_name=&search_date=&vaccine_name=&commit=Search'




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

pmUrls <- getUrls(base_url)###need to loop over these.  - all the URLS of the pages


readHtml <- function(url) { ####looping through each url - each different page. 

  

  prepmod_divs <- read_html(url) %>% html_nodes('div.mt-24 div.border-gray-200')
 
  
  text <- prepmod_divs %>% html_text() ###this is a vector of the DIVS. -



  createDf <- function (rawDiv) { ####loops through each DIV!!!

    
    link <- rawDiv %>% html_nodes('a.button-primary') %>%  html_attr(name="href")
    clinic_url <- ifelse(!is_empty(link), paste0("https://myhealth.alaska.gov",link), "none")

    rawTextEach <- rawDiv %>% html_text()
    
    hasAppts <- ifelse(str_detect(rawTextEach, "Available Appointments"), "has_appointments_listed", "no_appointments_listed")
    
    appt_available <- ifelse(hasAppts == "has_appointments_listed", substr(rawTextEach, regexpr("Available Appointments", rawTextEach)+33, regexpr("Available Appointments", rawTextEach)+36) ,"nothing listed" )
    
    appt_available <- str_remove(appt_available, "\n")
    appt_available <- str_trim(appt_available)
    

    
    
    delim <- read_delim(rawTextEach, delim = "\n", col_names =F, skip=0, n_max = 5) ## only like 4 rows.
    # print(delim)
    
    df <- data.frame(delim)
    names(df) <- c("text")
    df <-  df %>% mutate_if(is.character, str_trim)
    df <- t(df)
    df <- data.frame(df)
    names(df) <- c("location", "address", "vaccines", "offered", "ages")
    # df <- df %>% mutate(appt_listed = hasAppts)
    df <- df %>% mutate(appts_available = appt_available)
    df <- df %>% mutate(clinic_registration_url = clinic_url)
    df <- df %>% select(-vaccines)
    return (df)
  }  
  
  df_output <- lapply(prepmod_divs, FUN= createDf) %>% bind_rows()
  return(df_output) 
 ##this is the full df of a page. 
Sys.sleep(1)
  
}###end of the read HTML fucntion. 


complete_df <- lapply(pmUrls, FUN=readHtml) %>% bind_rows()

complete_df <- complete_df %>% mutate(time_downloaded = Sys.time())
complete_df <- complete_df %>% mutate(offered = str_remove(offered, "Vaccinations offered:"))
complete_df <- complete_df %>% arrange(desc(appts_available))
complete_df <- complete_df %>% select(-ages)
complete_df <- complete_df %>% mutate (date = str_sub(location, nchar(location)-10, nchar(location)))
complete_df <- complete_df %>% mutate (location = str_sub(location, 1,nchar(location)-14 ))

complete_df <- complete_df %>% relocate(date, .before = location)

only_available <- complete_df %>% filter (appts_available > 0)

anchorage <- complete_df %>% filter (appts_available > 0 & clinic_registration_url !="none" & str_detect(address, "Anchorage"))

write_csv(complete_df, "complete_prepmod.csv")

##################################################
##formatting!!!
#############################################

complete_df_format <- complete_df %>% filter (appts_available > 0 & clinic_registration_url !="none")

complete_df_format <- complete_df_format %>% mutate(date = str_sub(date, 1, nchar(date)-5) )

complete_df_format <- complete_df_format %>% mutate(date = str_sub(date, 3, nchar(date)) )


complete_df_format <- complete_df_format %>% mutate(offered = str_remove(offered," COVID-19 Vaccine"))
complete_df_format <- complete_df_format %>% mutate(address = str_remove(address," AK,"))
complete_df_format <- complete_df_format %>% mutate(address = str_remove(address," Alaska,"))

complete_df_format <- complete_df_format %>% mutate(location = paste0("<a href='",clinic_registration_url,"' target='_blank'><strong>",location, "</strong></a>" ))

complete_df_format <- complete_df_format %>% rename(appts = appts_available)
complete_df_format <- complete_df_format %>% rename(vaccine =offered)
complete_df_format <- complete_df_format %>% mutate(vaccine =str_remove(vaccine, "-BioNTech"))

complete_df_format <- complete_df_format %>% mutate(vaccine =str_replace(vaccine, "Janssen", "Johnson & Johnson"))


complete_df_format <- complete_df_format %>% select(-clinic_registration_url, -time_downloaded)
complete_df_format <- complete_df_format %>% relocate(appts, .before=vaccine)



###### remove duplicates
complete_df_format <- complete_df_format %>% distinct()
complete_df_format <- complete_df_format %>% filter(appts !="nothing listed")


########################################################################
########################################################################






####create a sum
# 
total_appointments <- sum(as.numeric(complete_df_format$appts))
total_appointments_string = paste0(total_appointments)
write_lines(total_appointments_string, "total_appointments.js")

##########################################


write_csv(complete_df_format, "complete_prepmod_format.csv")

write_csv(only_available, "only_available.csv")
write_csv(anchorage, "anchorage_prepmod.csv")



###aws stuff

my_key <- "AWS KEY"
key_secret <- "KEY SECRET"

Sys.setenv("AWS_ACCESS_KEY_ID" = my_key,
           "AWS_SECRET_ACCESS_KEY" = key_secret,
           "AWS_DEFAULT_REGION" = "YOUR REGION")

put_object(file="complete_prepmod.csv", object ="complete_prepmod.csv",  bucket = "YOUR BUCKET")

put_object(file="complete_prepmod_format.csv", object ="complete_prepmod_format.csv",  bucket = "YOUR BUCKET")

put_object(file="only_available.csv", object ="only_available.csv",  bucket = "YOUR BUCKET")

put_object(file="anchorage_prepmod.csv", object ="anchorage_prepmod.csv",  bucket = "YOUR BUCKET")


# 


#########################################################
current_time <- (Sys.time()-32400)

current_time_format <- strftime(current_time, format = "%b %d %I:%M %p" )
current_time_string = paste0("var time_updated = ","'",current_time_format,"'")
write_lines(current_time_string, "time_updated.js")
#############################################################

put_object(file="time_updated.js", object ="time_updated.js",  bucket = "YOUR BUCKET")




put_object(file="total_appointments.js", object ="total_appointments.js",  bucket = "YOUR BUCKET")




}

full_scrape_publish()
Sys.sleep(45)
print(paste0("end of sleep 1", Sys.time()))

