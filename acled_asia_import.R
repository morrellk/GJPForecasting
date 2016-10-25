# Script to process ACLED Asia data.  Start with ACLED importer and go from there
# on hopes that not too different?  
#
# Except - all xlsx files, so need to deal with that
#
#    Asia data files:
#         - ACLED-Asia-Running-file-January-to-December-2015-V2.xlsx 
#         - monthly data files named like:
#              ACLED-Asia-August-2016-Complete.xlsx
#
#    Will need to get the 2015 data file and all of the monthly data files
#    Probably a good idea to save what get and add to it as more comes out later.
#    
#    


#  This gave an error for a few items in latitude - had spaces after the entry.  
#    Manually cleaned for August 2016 file, but may need programmatic way to deal
#asia <- read_xl("ACLED-Asia-August-2016-Complete.xlsx", col_names=TRUE)


# See https://dartthrowingchimp.wordpress.com/2015/07/19/acled-in-r/ for a blog post discussing this script.

# This script will download, unzip, prep, and merge ACLED's Version 5 historical data (1997-2014) and its realtime
# data. It is designed to keep working as ACLED posts weekly updates; instead of calling fixed addresses and file names,
# it scrapes the relevant link addresses from ACLED's site and parses the names of those .zip files to guess at the correct
# name of the .csv files they contain. As such, it should keep working throughout 2015, as long as ACLED does not change
# the layout or structure of their web site or the conventions by which they name those files. It will almost certainly need
# to be updated in early 2016 when the URL for ACLED's Realtime page changes and then again when the next annual update of
# the historical data is completed and posted. If any of those things do change, the script will fail at the "Data fetching"
# step, if not sooner. If that happens, you should be able to work around the problem by hard-coding the past.url, past.file,
# realtime.url, and realtime.file objects according to the steps given in the comments at the bottom of the script and then
# re-running the script starting at the "Data fetching" step.

# This script creates three data sets in the R environment:
#    1. ACLED: an event file containing all records through the most recent posted update, usually last Monday
#    2. ACLED.cm.types: a country-month data frame with counts of events by type and one for all battles of any type
#    3. ACLED.cm.deaths: a country-month data frame with counts of deaths from all event types

# Also note that the csv of the historical data does not include the Notes field because special characters in those fields
# make it difficult to read cleanly. If you want to see the notes, you need to download the .xslx version, which includes
# that column. Once downloaded, you can read that spreadsheet into R with these lines:
# library(readxl)
# ACLED.v5.xlsx <- read_excel("ACLED-Version-5-All-Africa-1997-2014_dyadic_Update.xlsx", 1,
#  col_types = c("numeric", "text", "numeric", "date", rep("numeric",2), rep("text",3), "numeric", rep("text",2),
#  rep("numeric",2), rep("text",5), rep("numeric",3), rep("text",2), "numeric")) 

# Modified 21-Mar-2016 by morrellk  
#    Updated for 2016 -  - required changes to some hardcoded values
#                        - changed to reading with read_csv (from readr package)
#                             to accomodate Notes which is now in the historical csv
#                        - changed from html() to read_html() (html()) deprecated)
#                        - ensure that get the correct realtime data file (not just Jan)
#                        - remove renaming of GEO_PRECIS (variable currently same name)
#                        - remove 5 columns named NA, full of NA's that appeared in realtime data
#                        - changed left_join(expand(...)) to complete() because 
#                                  expand() not working as expected


# Load required packages

library(rvest)
library(stringr)
library(dplyr)
library(tidyr)
library(countrycode)
library(ggplot2)
library(readr) # used for read_csv, had trouble with read.csv and notes
library(readxl)
#asia <- read_xl("ACLED-Asia-August-2016-Complete.xlsx", col_names=TRUE)

# Create function to get file and read csv 

getfile <- function(vector) {
     #temp <- tempfile()
     download.file(vector[1], vector[2])
     df <- read_excel(vector[2], col_names=TRUE)
     #unlink(temp)
     return(df)
}

# This block of code pulls the link address for the historical data from the ACLED web site and directs the download there.
# Unfortunately, the name of the .csv file in that zip archive is not a direct derivation of the link address, so I am leaving
# that part hard-coded for now. That means it should work for the rest of 2015, as long as ACLED doesn't rearrange or rename
# the page, but the script will need to be updated in 2016. This block and the one that follow depend on 'rvest'.
# 21-Mar-2016 Updated for 2016 data - version and year change, change in urls for 
#              realtime data.

url <- "http://www.acleddata.com/asia-data/"  # url for acled's Asia data page

#    Instead of trying to automate, get the user to enter the links to read
#
get_filelist <- function(){
     i <- 1
     datafile.url <-readline("Enter the url link for file to read: ")
     while(!(datafile.url[i]=="")){
          datafile.url <- c(datafile.url, readline("Enter the url link for file to read: "))
          i <- i+1
     }
     return(datafile.url[1:i-1])
}

datafile.url <- get_filelist()
# build name of realtime file to extract from realtime .zip
datafile.name <- lapply( c(1:length(datafile.url)), function(x) 
     {
     datafile.url[x] %>% 
     str_split(., "\\/") %>%         # split the url at the forward slashes
     unlist(.) %>%                   # unlist the result
     .[length(.)]                  # take the last item on that list, the file name
     } )             

# Create a list of files to download and their local names
datafile.list <- lapply(c(1:length(datafile.url)), function(x) 
     { 
     unlist(c(datafile.url[x], datafile.name[x]))
     })

# Fetch and merge the past and current-year files
#Use the function created above download and read files into a list
asia.list <- lapply(datafile.list, getfile)

# Remove excess columns if they are there
n1 <- 1+min(sapply(c(1:length(asia.list)), function(x) {length(names(asia.list[[x]]))}))

asia.list <- lapply(c(1:length(asia.list)),function(x){
     if (length(names(asia.list[[x]])) >= n1){
          asia.list[[x]] <- asia.list[[x]][,-(n1:length(names(asia.list[[x]])))]
     }
     else{
          asia.list[[x]] <- asia.list[[x]]
     }
     })
# Merge all files in the list, keep non-duplicate rows
asia <- Reduce(function(...) merge(..., all=TRUE), asia.list) 

names(asia) <- tolower(names(asia)) # Convert var names in merged file to lower case

#********Next thing to do is re-create acled_country_mk.R for asia

# Remove interim objects from workspace
rm(asia.list)


# If the URLs for ACLED's data site change, as they probably will each year, it may be necessary to hard-code the link
# addresses and file names for the historical and realtime data downloads before the "Data fetching" step. Here's how
# I did that on a Windows PC:
# 1. Pointed my browser to the ACLED home page: http://www.acleddata.com/
# 2. Clicked on the Data tab, which took me to: http://www.acleddata.com/data/
# 3. Clicked on "ACLED Version 5 (1997-2014)" under Africa Data, which took me to: http://www.acleddata.com/data/version-5-data-1997-2014/
# 4. Right-clicked on the (csv) option for 'ACLED Version 5 (1997 – 2014) standard file' and selected 'Copy link address'
# 5. Used Ctrl-V to paste that in the past.url slot, .e.g, past.url <- "[paste here]"
# 6. Left-clicked on that same link to download the .zip file
# 7. Double-clicked on the downloaded .zip file to inspect the contents
# 8. Right-clicked on the .csv in the resulting window, selected 'Properties', and used Ctrl-C to copy the csv file's name
# 9. Used Ctrl-V to paste that file name in the past.file slot, e.g., past.file <- "[paste here]"
# 10. Back on the ACLED site, clicked on 'Realtime Data (2015)'
# 11. Repeated steps 4-9 for 'Realtime 2015 All Africa File (updated 11th July 2015)(csv)' and pasted the results in
#     the realtime.url and realtime.file slots
