# Script to answer:  How many civilian fatalities in DRC between 1 May 2016
#    and 31 Dec 2016 in GJOpen Early Warning Project challenge
# First - run acled_importer.R and acled_country_mk.R for DRC
# and load the resulting file

events.civilian.fatal <- events.country %>%
     # Only events with fatalities
     filter(., fatalities > 0) %>%  
     # Only since May 1 2016
     filter(., year == 2016) %>%
     filter(., month >= 5) %>%
     
     # Keep only events that are Violence against Civilians or Remote Violence
     filter(., event_type == 'Violence against civilians' | event_type == 'Remote violence)')%>%
     
     #Remove events that are Remote violence where actor 2 is not civilians
     filter (., !(event_type == 'Remote violence' & actor2 != 'Civilians(Democratic Republic of the Congo)'))

# Sum the fatalities
civ_fatalities <- summarise(events.civilian.fatal, sum(fatalities))