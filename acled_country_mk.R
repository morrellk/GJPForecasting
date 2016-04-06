#  acled_country_mk.R
#
#    Script to take output from acled.importer.R and pull data for a specific 
#    country broken down by actor.  File with acled.importer.R can be found at 
#    www.gitub.com/morrellk/dart-throwing-chimp, or at the master directory that
#    was forked from: www.github.com/ulfelder/dart-throwing-chimp.  
#
#    This code is intended to gain insight for
#    Early Warning Project questions at Good Judgment Open, so focus is on
#    detection of episodes of mass killing.  See EWP definitions page
#    at http://www.earlywarningproject.com/definitions 
#    As a result, subsequent steps extract only recent events which result in
#    fatalities and which do not have "battle" in the event type.  Further
#    filtering is done to remove events associated with on-going episodes. Data
#    is saved to a file for later import - use load(filename) to load the
#    data into workspace.
#
#    Functions:
#         Main:
#         acled_country_mk(events_df, c="Nigeria", rem_actor_name=c("Boko Haram","Boko Haram"),
#                             rem_actor_num=c(1,2))
#         Supporting:
#         acled_country(events_df, c="Nigeria")
#         acled_cntry_recent_by_actor(country_tbl)
#         acled_cntry_remove_ongoing(country_tbl, actor, actor_num)
#
#    
#    Input: 
#         events_df: dataframe in form output by acled.importer.R 
#         c: country name as a string.  Needs to be in form recognized by 
#              countrycode package
#         rem_actor_name: list of strings, names of actors to eliminate from results due
#              to being part of on-going episode.  Can be partial, eg. "Boko 
#              Haram" will remove all which contain that string.
#         rem_actor_num: list indicating associated actor number (1 or 2) for 
#              each item in the rem_actor_names list.  If want to remove an actor
#              from both actor positions, enter twice.
#
#    Output:
#         File containing datatables formed at several steps along the way. Name
#              formed from country name + today's date (from today()).  
#         File contains: 
#              events.country:  All events for country c.  
#              events.country.fatal: Fatal events that were not battles 
#                                       from events.country
#              events.recent: Just events in the last year from events.country.fatal
#              events.wo: Events with actors from on-going episodes removed
#              last_year_monthly.actor1: fatalities from events.wo grouped by 
#                             actor 1 and month
#              last_year_monthly.actor2: fatalities grouped by actor 2, monthly
#              last_year_tot.actor1: total fatalities in last year, by actor 1
#              last_year_tot.actor2: total fatalities in last year by actor 2
#    
#    Explanation of the meaning of variables in these tables can be found in the 
#    ACLED code book and user guide:  
#    http://www.acleddata.com/wp-content/uploads/2016/01/ACLED_Codebook_2016.pdf
#    http://www.acleddata.com/wp-content/uploads/2016/01/ACLED_User-Guide_2016.pdf
#
#
#    Note: 
#         (1)  Despite filtering done, user should review data, including upstream to
#              make sure that what is filtered out makes sense.  Probably will 
#              need to refine the call to reflect specific situation.
#         (2)  Functions expected to work through 2016, but may need update for
#              future years.
#
#
library(stringr)
library(dplyr)
library(tidyr)
library(countrycode)
library(lubridate)
#
#
#    events_df should be of the form of ACLED created by acled.importer.R
#
acled_country <- function(events_df, c = "Nigeria"){
     # Get country code for the name
     c_code <- countrycode(c, "country.name", "cown", warn=FALSE)
     
     # Extract fatal events for the country of interest
     events.country <- events_df %>%
          filter(., gwno == c_code, fatalities > 0) %>% # just that country, 
          # Remove events that are battles, include only events with fatalities
          filter(., !grepl("Battle", event_type, ignore.case=TRUE)) %>%
          mutate(month = as.numeric(substr(event_date, 4, 5))) %>%
          mutate(day = as.numeric(substr(event_date, 1, 2))) %>%
          select(., event_id_no_cnty, year, month, day, event_type, actor1, 
                 ally_actor_1, actor2, ally_actor_2, country, location, latitude, 
                 longitude, source, notes, fatalities) 
     return(events.country)
}



# acled_cntry_recent_by_actor takes output from acled_country and
#    - filters down to just events in the past year
#    - groups by actor
acled_cntry_recent_by_actor <- function(country_tbl) {
     # Given a table of the form output from acled_country, extract only the last
     # year's worth of data and only those 
     # events with fatalities. Plot, grouped by actor1 and return
     start <- today("GMT") - years(1)
     
     events.new_a1 <- country_tbl %>%
          mutate(., event_date=paste(year,month, day, sep="-")) %>%
          filter(., event_date >= start) %>%
          group_by(., actor1, year, month) 
     
     return(events.new_a1)
}

# acled_cntry_remove_ongoing takes output from acled_country (or recent_by_actor)
# and removes events corresponding to on-going mass killings
# actor should be a string, actor_num should be 1 or 2.  
# events for which that actor_num (actor1 or actor2) contains actor string will
# be filtered out.
acled_cntry_remove_ongoing <- function(country_tbl, actor, actor_num){
     events.no_ongoing <- country_tbl
     for (i in 1:length(actor_num)){
          if (actor_num[i] == 1){
               events.no_ongoing <- events.no_ongoing %>%
                    ungroup(.) %>%
                    filter(., !grepl(actor[i], actor1)) %>%
                    filter(., !grepl(actor[i], ally_actor_1)) 
          }
          else {
               events.no_ongoing <- events.no_ongoing %>%
                    ungroup(.) %>%
                    filter(., !grepl(actor[i], actor2)) %>%
                    filter(., !grepl(actor[i], ally_actor_2)) 
          }
     }
     return(events.no_ongoing)     
}

# acled_country_mk calls various other functions in order to
# generate the different dataframes and tables for the country.
# These are then saved to a file and can be loaded into the workspace.
#  One issue with this function is that it defines recent as within a year
#  of the current date - file may have been collected earlier so may result in
#  some data left out.  Haven't decided whether would be better to do something else
#  and don't want to change acled.importer.R to pass in filename - that may
#  be best choice though, or not deleting it from workspace.

acled_country_mk <- function(events_df, c="Nigeria", rem_actor_name="",
                             rem_actor_num=0){
     # Get country code for the name
     c_code <- countrycode(c, "country.name", "cown", warn=FALSE)
     
     # Extract all events for the country specified
     events.country <- events_df %>%
          filter(., gwno == c_code) %>%
          mutate(month = as.numeric(substr(event_date, 4, 5))) %>%
          mutate(day = as.numeric(substr(event_date, 1, 2))) %>%
          # Drop some of the columns
          select(., event_id_no_cnty, year, month, day, time_precision, event_type, 
                 actor1, ally_actor_1, actor2, ally_actor_2, country, location, 
                 latitude, longitude, geo_precision, source, notes, fatalities) 
     
     # Extract fatal events that were not battles
     events.country.fatal <- events.country %>%
          filter(., fatalities > 0) %>%  
          # Remove events that are battles, include only events with fatalities
          filter(., !grepl("Battle", event_type, ignore.case=TRUE)) 
     
     # Filter to just the most recent year     
     events.recent <- acled_cntry_recent_by_actor(events.country.fatal)
     
     # If on-going episodes defined, take those actors out.  Otherwise,
     # events.wo is same as events.recent, ungrouped.
     
     if(rem_actor_num[1] > 0){
          events.wo <- acled_cntry_remove_ongoing(events.recent, rem_actor_name, 
                                             rem_actor_num)
     } else {
          events.wo <- ungroup(events.recent)
     }
     
     # group by actor1 and summarise - get monthly per actor, then total
     events.wo <- group_by(events.wo, actor1, year, month)
     
     last_year_monthly.actor1<- summarise(events.wo, deaths = sum(fatalities, na.rm=TRUE))
     last_year_tot.actor1 <- last_year_monthly.actor1 %>%
          group_by(., actor1) %>%
          summarise(., total_deaths = sum(deaths, na.rm = FALSE))
     
     # do for actor 2
     events.wo <- events.wo %>%
          ungroup(.) %>%
          group_by(., actor2, year, month)
     
     last_year_monthly.actor2 <- summarise(events.wo, deaths = sum(fatalities, na.rm=TRUE))
     last_year_tot.actor2 <- last_year_monthly.actor2 %>%
          group_by(., actor2) %>%
          summarise(., total_deaths = sum(deaths, na.rm = FALSE))
     
     # build filename, use country and today's date
     filename <- paste(c, today() ,sep="")
     
     save("events.country","events.country.fatal","events.recent",
          "events.wo","last_year_monthly.actor1", 
          "last_year_monthly.actor2","last_year_tot.actor1",
          "last_year_tot.actor2",file=filename)
}
