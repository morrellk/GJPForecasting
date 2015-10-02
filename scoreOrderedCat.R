##   scoreOrderedCat.R
##
##   Code to estimate Brier scores for possible outcomes
##   in a question using ordered categorical scoring.
##
##   This would correspond to a snapshot in time - scores for a single day/single
##   weighting scheme.  Doesn't consider what consensus forecast is.  Primary
##   use is to see how different probabilities affect Brier score under the
##   ordered categorical scoring rule.
##
##   Arguments:
##        plist:  list of probability assigned to each category, 
##                  length = number of possible outcomes.  Must indicate 
##                  a value for all outcomes, ie. enter 0 for outcomes with
##                  0 probability assigned.  Will work for values given as 0-100
##                  or as 0-1, but not if mixed. No default.
##
##   Output:
##        scoreOrderedCat returns a numeric vector of same length as plist of 
##        the Brier score that would result for each potential question outcome
##
##   Example Usage:
##        source('~/scoreOrderedCat.R')
##        ordBrier <- scoreOrderedCat(plist=c(25,25,25,25))
##
##   2-Oct-2015  K. Morrell
##
scoreOrderedCat <- function(plist){
     
     ##   Set up scoring pairs
     ncat <- length(plist)
     npair <- ncat - 1
     
     ## Create a npair x 2 matrix of scoring pairs
     
     val <- sapply(c(1:npair), function(x) c(sum(plist[1:x]),
                                             sum(plist[(x+1):ncat])))
     val <- t(val)
     
     ##  Should check that val <1, otherwise divide by 100
     if (any(val>1)) val <- val/100
     
     ##  Calculate score for each possible TRUE   
     ##  "scoring" package not available at CRAN for current verison of R
     ##  so, might be worth naming a function that calculates brier score for 
     ##   pair.
     ##  Bascially: for a given result 
     ##             brier score for each score pair
     ##             then average all the score pairs
     
     ##   Need result vectors
     ##   if result is i or less, 1,
     ##   Trouble getting mapply to work correctly, anyway, easier to read
     ##   this way.
     pair_val <- array(dim=c(2, npair, ncat))
     
     for (r in 1:ncat){
          paira <- sapply(1:npair, function(x) r<=x)
          pairb <- sapply(1:npair, function(x) r>x)
          pair_val[,,r] <- rbind(as.numeric(paira),as.numeric(pairb))
     }
     
     ##   With the result matrix, calculate Brier score.
     
     ordBrier<-sapply(1:ncat, function (y) 
                      mean(sapply(1:npair, function(x) 
                           brierScore(pair_val[,x,y], val[x,]))))

     return(ordBrier)
} 

##   brierScore
##   
##   Return the Brier score for a score pair
##
brierScore <- function(result = c(1,0), forecast) {
     
     b <- sum((forecast-result)^2)
     return(b)
}
     
     
