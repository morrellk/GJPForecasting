# Script to read and process Burundi local data
#    after save to csv from xslx
# (So: steps are - download file, save as csv, run this, load the output file)

Burundi.local <- read_csv("burundi-file_26042015-to-21082016-1.csv")

# Realtime file finishing with 5 columns of NA, all named NA - get rid of 
Burundi.local <- Burundi.local[,-(26:32)]
names(Burundi.local) <- tolower(names(Burundi.local)) # Convert var names in merged file to lower case

# Create country-month summary data frames

# Counts of events by type
Burundi.cm.types <- Burundi.local %>%
     mutate(event_type = make.names(tolower(event_type))) %>% # Change event type labels for use as proper var names, and to deal with "Remote Violence", "Remote violence"
     mutate(month = as.numeric(substr(event_date, 4, 5))) %>%  # Create month var to use in grouping
     group_by(year, month, event_type) %>%  # Define groupings from highest to lowest level; data are automatically ordered accordingly
     tally(.) %>%  # Get counts of records in each group (i.e., each country/year/month/type subset)
     spread(., key = event_type, value = n, fill = 0) %>% # Make data wide by spreading event types into columns
     #  expand didn't seem to be doing what was expected. Use complete instead, hardcoded end year
     #left_join(expand(., gwno, year, month), .) %>% # Expand data frame to cover all possible country-months by left-joining tallies to complete series created with expand() from tidyr
     complete(year=2015:2016, month=1:12) %>%
     replace(is.na(.), 0) %>%  # Replace all NAs created by that last step with 0s
     mutate(., battles = rowSums(select(., contains("battle")))) %>% # Create vars summing counts of all battle types
     filter(., year < as.numeric(substr(Sys.Date(), 1, 4)) | (year == as.numeric(substr(Sys.Date(), 1, 4)) & month < as.numeric(substr(Sys.Date(), 6, 7)))) %>% # Drop rows for months that haven't happened yet
     mutate(., country = countrycode(gwno, "cown", "country.name", warn = FALSE)) # Use 'countrycode' to add country names based on COW numeric codes

# Death counts
Burundi.cm.deaths <- Burundi.local %>%
     mutate(month = as.numeric(substr(event_date, 4, 5))) %>%  # Create month var to use in grouping
     group_by(year, month) %>%  # Define groupings from highest to lowest level; data are automatically ordered accordingly
     summarise(., deaths = sum(fatalities, na.rm=TRUE)) %>%  # get monthly death counts
     # As above, replace the left_join(expand( )) with complete()
     #left_join(expand(., gwno, year, month), .) %>% # Expand data frame to cover all possible country-months by left-joining sums to complete series created with expand() from tidyr
     complete(year=2015:2016, month=1:12) %>%  
     replace(is.na(.), 0) %>%  # Replace all NAs created by that last step with 0s
     filter(., year < as.numeric(substr(Sys.Date(), 1, 4)) | (year == as.numeric(substr(Sys.Date(), 1, 4)) & month < as.numeric(substr(Sys.Date(), 6, 7)))) %>% # Drop rows for months that haven't happened yet
     mutate(., country = countrycode(gwno, "cown", "country.name", warn = FALSE)) # Use 'countrycode' to add country names based on COW numeric codes

acled_local_country_mk(Burundi.local, c="Burundi")
