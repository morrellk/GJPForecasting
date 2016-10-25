# Script to process Ethiopian data for ethnic group
# 
# Want to tally all deaths in each category which have Oromo or Amhara ethnic
# group as one of the actors or ally actors.  
#
# Most important - try to sort out which violence against civilians events 
# might have targeted Oromo ethnic group.
#
# Then, check the Riots/Protests number
#
# Start from events.wo, ungroup, then filter by "Oromo Ethnic group" in any actor
# field.  
#
# Or, start from events.wo, ungroup, then create a new field (Oromo) that has
# a 1 if Oromo Ethnic Group appears in any of the actor fields.  Do the same for
# Amhara.  Then, do a group_by event_type, ethnic -> sum fatalities and save results
#  Same for those + year, month

oromo_str <- "Oromo ethnic group"
amhara_str <- "Amhara ethnic group"

deaths.ethnic <- events.wo %>%
     ungroup(.) %>%
     mutate(., oromo = (grepl(oromo_str, actor1, ignore.case=TRUE) | 
                             grepl(oromo_str, ally_actor_1, ignore.case=TRUE) | 
                             grepl(oromo_str, actor2, ignore.case=TRUE) | 
                             grepl(oromo_str, ally_actor_2, ignore.case=TRUE))) %>%
     mutate(., amhara = (grepl(amhara_str, actor1, ignore.case=TRUE) | 
                             grepl(amhara_str, ally_actor_1, ignore.case=TRUE) | 
                             grepl(amhara_str, actor2, ignore.case=TRUE) | 
                             grepl(amhara_str, ally_actor_2, ignore.case=TRUE))) 
     
oromo_deaths <- summarise(group_by(deaths.ethnic, event_type,oromo), 
                          oromo_deaths = sum(fatalities))
amhara_deaths <- summarise(group_by(deaths.ethnic, event_type,amhara), 
                           amhara_deaths = sum(fatalities))
o_and_a_deaths <- summarise(group_by(deaths.ethnic, event_type,oromo, amhara), 
                            deaths = sum(fatalities))

deaths.ethnic.oromo <- deaths.ethnic %>%
     filter(., oromo == TRUE) %>%
     # set up a column that indicates if actor2 is Oromo
     mutate(., actor2_T = ((grepl(oromo_str, actor1, ignore.case=TRUE) | 
                              grepl(oromo_str, ally_actor_1, ignore.case=TRUE)))) %>%
     mutate(., other = if_else(actor2_T, actor2, actor1))
            

save("deaths.ethnic","deaths.ethnic.oromo", "oromo_deaths", "amhara_deaths", "o_and_a_deaths", 
     file="latest_ethnic_fatalities_Ethiopia")
write_csv(deaths.ethnic.oromo, "Oromo_deaths.csv")
