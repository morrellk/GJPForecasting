##   getGJPquestions.R
##
##   Function to read a question page at https://www.goodjudgmentproject.com/
##   and pull out the questions on that page, then write them to a csv file.
##
##   Why?  Because I want to get them into a spreadsheet a little easier.
##
##   This is currently set up to read a locally saved copy of the questions page.
##
##   Rather save the page locally and run this on the local file, esp since may
##   run into https access issues.
##
##   Arguments:
##        qloc:     location to read, web page or file, defaults to locally stored
##                  file named "questions.htm"
##        outFile:  name of the output CSV file, defaults to "GJPquestions.csv"
##
##   Output:
##        datafrome containing:
##             question number (number extracted from question id)
##             closing date    (currently as string, UTC)
##             question title  (short text of question)
##             current probability est for binary questions, nothing for multi
## 
##   Packages:
##        loads and uses XML library
##
##   Usage:
##        qlist <- getGJPquestions("fileToRead", "outputfilename")
##
##   Possible updates to make:
##        create a version to pull all questions, not just one page
##        pull the predictions correctly for multi-category questions
##        version to use the queries?
##        add in automatic download/copy of page source using download.file?
##
##
##   21-Sep-2015   K. Morrell
##
getGJPquestions <- function(qloc="questions.htm", outFile="GJPquestions.csv") {
     
     library(XML)
     
     ##   Read the file in
     html <- htmlTreeParse(qloc, useInternalNodes=T)
     
     ##  The following creates a list of the question items on a page
     info <- xpathSApply(html,"//div[@class='question']",fun=xmlToList)
     ##
     ##   after doing the above, then
     ##   info[1][[1]]$div$a$text returns the text of the question
     ##   and
     ##   info[1][[1]]$.attrs["id"]  gives the question id
     ##   and so does:
     ##   info[[1]]$.attrs["id"]
     
     ##  Get a list of the questions on the page and id's
     qlist <- sapply(info, function(x) as.character(x$div$a$text))
     idlist <- sapply(info, function(x) as.character(x$.attrs["id"]))
     
     ##  Pull out just the question number 
     ##  (id's currently in format "question_#")
     qnum <- as.numeric(sapply(strsplit(idlist,"_"), function(x) x[2]))
          
     ##  This at least pulls the predictions for binary questions, but the category
     ## ones are a mess.  Pull them anyway, so that lists align. Currently
     ## these are the only two types being used, but may need to change later.
     
     vals <- xpathSApply(html,
        "//div[@class='binary-probability-value'] | //div[@class='consensus-donut-graph']",
        xmlValue)
     p_end <- regexpr("%", vals)
     p_start <- p_end-2
     p_val <- substr(vals, p_start, p_end)

     ##  Dates now using data-localizable-timestamp
     cl_date <- xpathSApply(html, "//span[@data-localizable-timestamp]",xmlValue)
     
     ## Create a dataframe
     qinfo <- data.frame(qnum, cl_date, qlist, p_val, stringsAsFactors=FALSE)
     
     ##  Output as CSV
     write.csv( qinfo, outFile)
     
     return(qinfo)
}
     
     
     