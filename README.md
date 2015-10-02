# GJPForecasting
Stuff to use with new Good Judgment Project site, 2015-16 season.

Intended to contain code snippets and tools that I've written for this year.  Not polished, I'm doing this in part for the practice with R.  May put in some things other than R code too.

Notify me here if there is something you want written/changed - will do if I can & have time.

### Files
* getGJPquestions.R
     + reads an html copy of the "questions" page at www.goodjudgmentproject.com
     + extracts question number (from id), closing date, question text, and current probability for binary questions (not category questions yet).
     + puts info in a data frame, returns it, and also writes to a CSV file
     + see file header for comments on usage, arguments to pass.
     
* refugees_2015.xlsx
     + Excel workbok for estimation of refugees arriving in Europe by sea, 2015
     + update red-outlined cells with most recent data from http://data.unhcr.org/mediterranean/regional.php

* "Bracket refugees.pdf"," Bracket refugees.csv"
     + PDF and CSV files for "Bracket" sheet from refugees_2015.xlsx.
     + Do not have all features in the Excel worksheet, but wanted different format options.
     
* Greece.pdf
     + PDF file for "Greece" sheet from refugees_2015.xlsx.  
     + Just has text for current version of the Excel sheet.
     
* scoreOrderedCat.R
     + ordered categorical scoring
     + provides Brier score estimates for all potential outcomes
     + input is vector of probability estimates, length = # result categories
     + use for seeing how different probabilities affect ordered categorical score
     + doesn't account for consensus forecast
     + doesn't track over time
     


