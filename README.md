# GJPForecasting
R code to use with new Good Judgment Project site, 2015-16 season.

Contains code snippets and tools that I've written for this year.  Not polished, 
I'm doing this in part for the practice with R.  

### Files
* getGJPquestions.R
     + reads an html copy of the "questions" page at www.goodjudgmentproject.com
     + extracts question text, question id, closing date, and current probability
     for binary questions (not category questions yet).
     + puts info in a data frame, returns it, and also writes to a CSV file
     + see file header for comments on usage, arguments to pass.

