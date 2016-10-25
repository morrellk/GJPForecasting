# GJPForecasting
Stuff to use with new Good Judgment Project site, 2015-16 season.

Intended to contain code snippets and tools that I've written for this year.  Not polished, I'm doing this in part for the practice with R.  Some things other than R code too.

Notify me here if there is something you want written/changed - will do if I can & have time.

### Files
* acled_country_mk.R
     + input: ACLED data in dataframe output by acled.importer.R 
     + filters for a specific country, within the last year
     + attempts to remove events associated with on-going episode of mass killing
     + output: tables of events created at different stages in process, including
tally of fatalities by actor1 (perpetrator) and actor2 (victim)

* plot_forecast.R
     + plots forecast and effect on Brier scores over forecast period
     + input: list of forecast dates (as strings), array of forecast values
     + output: PNG file with two graphs
     + see file header for more details.
     
* getGJPquestions.R (Note: doesn't work with updated site)
     + reads an html copy of the "questions" page at www.goodjudgmentproject.com
     + extracts question number (from id), closing date, question text, and current probability for binary questions (not category questions yet).
     + puts info in a data frame, returns it, and also writes to a CSV file
     + see file header for comments on usage, arguments to pass.
     
* refugees_2015.xlsx (11/30/15: Not up to date)
     + Excel workbook for estimation of refugees arriving in Europe by sea, 2015
     + update red-outlined cells with most recent data from http://data.unhcr.org/mediterranean/regional.php

* "Bracket refugees.pdf"," Bracket refugees.csv" (11/30/15: Not up to date)
     + PDF and CSV files for "Bracket" sheet from refugees_2015.xlsx.
     + Do not have all features in the Excel worksheet, but wanted different format options.
     
* Greece.pdf (11/30/15: Not up to date)
     + PDF file for "Greece" sheet from refugees_2015.xlsx.  
     + Just has text for current version of the Excel sheet.
     
* scoreOrderedCat.R
     + ordered categorical scoring
     + provides Brier score estimates for all potential outcomes
     + input is vector of probability estimates, length = # result categories
     + use for seeing how different probabilities affect ordered categorical score
     + doesn't account for consensus forecast
     + doesn't track over time
     
* acled_Burundi_local.R
     + Script to read in file containing Burundi local data sources and process
     + input: .csv file of Burundi data (hardcoded - will need to update)
     + Basically does what the acled_importer does, but for a specific .csv file.
     
* acled_local_country_mk.R
     + Script to process local data.  Functions the same as acled_country_mk.R
     
* acled_asia_import.R
     + Importer for the Asia data file
     
* asia_country_mk.R
     + Process ACLED data for Asia, slight differences in the data storage from 
     + Africa data.
     
* acled_DRC_civ_fatal.R
     + Script to filter the ACLED data for DRC in specific ways for an Early
     + Warning Project question at GJ Open.  Question specifies exactly which
     + fields to use
     
* acled_Ethiopia_ethnic.R
     + Script to mark which Ethiopian events in ACLED database affect Oromo 
     + Ethnic group or Amhara ethnic group.  Creates a csv file for those events.
     
     


