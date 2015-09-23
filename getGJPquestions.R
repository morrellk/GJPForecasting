##   getGJPquestions.R
##
##   Function to read the question page at https://www.goodjudgmentproject.com/
##   and pull out the questions, then write them to a csv file.
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
##             question title
##             question ID
##             current probabiliy est for binary questions
##             closing date
##
##   Usage:
##        qlist <- getGJPquestions("fileToRead", "outputfilename")
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
     ##
     ##   also, can reassign the value as:
     ##   id1 <- as.character(info[[1]]$.attrs["id"])
     
     ##  Get a list of the questions on the page and id's
     qlist <- sapply(info, function(x) as.character(x$div$a$text))
     idlist <- sapply(info, function(x) as.character(x$.attrs["id"]))
     
     ## if can figure out how to get the closing date and the current prediction...
     
     ##  This at least gives the predictions for binary ones, but the category
     ## ones are a mess:
     vals <- xpathSApply(html,
        "//div[@class='binary-probability-value'] | //div[@class='consensus-donut-graph']",
        xmlValue)
     p_loc <- regexpr("%", vals)
     p_val <- 0
     for(i in 1:length(vals)){
          p_val[i] <- substr(vals[i], p_loc[i]-2, p_loc[i])
     }

     ##  Well, can pull out the "Closing" with regexpr, then loop
     cl_loc <- regexpr("Closing:", info)
     cl_date = ""
     for(i in 1:length(info)){
          cl_date[i] <- substr(info[i],cl_loc[i]+22,cl_loc[i]+45)
     }
     
     ## Create a dataframe
     qinfo <- data.frame(qlist,idlist,p_val,cl_date, stringsAsFactors=FALSE)
     
     ##  Output as CSV
     write.csv( qinfo, outFile)
     
     return(qinfo)
}
     
     
     