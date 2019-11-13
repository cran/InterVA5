#' Data cleaning for InterVA-5 algorithm
#' 
#' This function implements the data cleaning steps in the InterVA5 software.
#' 
#' @param Input original data vector for one observation coded by 0 (absence), 1 (presence), and NA (missing).
#' @param id id for this observation
#' @param probbaseV5 matrix of probbaseV5
#' @param InSilico_check logical indicator for if the check uses InSilicoVA rule. InSilicoVA rule sets all symptoms that should not be asked to missing. In contrast, the default InterVA5 rule sets these symptoms to missing only when they take the substantive value. 
#' @param write logical indicator of writing to file
#' 
#' @return  \item{Output}{ new data vector} \item{firstPass
#' }{ message for the first pass check} \item{secondPass}{ message for the second pass check}
#' @author Jason Thomas, Zehang Li, Tyler McCormick, Sam Clark
#' @seealso \code{\link{InterVA5.plot}}
#' @references http://www.interva.net/
#' @keywords InterVA
#' @export DataCheck5
#' @examples
#' 
#' data(RandomVA5)
#' data(probbaseV5)
#' probbaseV5 <- as.matrix(probbaseV5)
#' RandomVA5 <- as.matrix(RandomVA5)
#' input <- as.character(RandomVA5[1, ])
#' input[which(toupper(input) == "N")] <- "0" 
#' input[which(toupper(input) == "Y")] <- "1" 
#' input[which(input != "1" & input != "0")] <- NA
#' input <- as.numeric(input)
#' output <- DataCheck5(Input=input, id="d1", probbaseV5=probbaseV5, write=TRUE)
#'
#' 

DataCheck5 <- function (Input, id, probbaseV5, InSilico_check=FALSE, write) {
    
    input.current <- Input
    S <- length(input.current)
    index.current <- id
    firstPass <- NULL
    secondPass <- NULL

    for (k in 1:2) {
        for (j in 2:S) {
                
            subst.val <- NA
            subst.val[probbaseV5[j,6]=="N"] <- 0
            subst.val[probbaseV5[j,6]=="Y"] <- 1

            cols.dont.asks <- (8:15)[substr(probbaseV5[j, 8:15],1,5)!=""]
            if (length(cols.dont.asks)>0) {
                for (q in cols.dont.asks) {
                    
                    Dont.ask  <- substr(probbaseV5[j,q], 1, 5)
                    Dont.ask.who <- probbaseV5[match(toupper(Dont.ask), toupper(probbaseV5[,1])),4]
                        
                    Dont.ask.row <- match(toupper(Dont.ask), toupper(probbaseV5[,1]))
                    input.Dont.ask <- input.current[Dont.ask.row]
                        
                    Dont.ask.val.tmp <- substr(probbaseV5[j,q], 6, 6)
                    Dont.ask.val     <- NA
                    Dont.ask.val[Dont.ask.val.tmp=="N"] <- 0
                    Dont.ask.val[Dont.ask.val.tmp=="Y"] <- 1
                        
                    if (!is.na(input.current[j]) & !is.na(input.Dont.ask)) {
                        if ( (input.current[j]==subst.val || InSilico_check)  & input.Dont.ask==Dont.ask.val) {
                            input.current[j] <- NA
                            if (write) {
                                if ( k==1 ) {
                                    firstPass <- rbind(firstPass,
                                                       paste(index.current, "   ", probbaseV5[j, 4], " (",
                                                             probbaseV5[j, 3], ") value inconsistent with ",
                                                             Dont.ask.who, " (", probbaseV5[Dont.ask.row, 3],
                                                             ") - cleared in working information", sep="")
                                                       )
                                }
                                if (k == 2) {
                                    secondPass <- rbind(secondPass,
                                                        paste(index.current, "   ", probbaseV5[j, 4], " (",
                                                              probbaseV5[j, 3], ") value inconsistent with ",
                                                              Dont.ask.who, " (", probbaseV5[Dont.ask.row, 3],
                                                              ") - cleared in working information", sep="")
                                                        )
                                }
                                
                            }
                        }
                    }
                }
            }
                
            if (substr(probbaseV5[j, 16], 1, 5)!="" & !is.na(input.current[j])) {
                Ask.if    <- substr(probbaseV5[j,16], 1, 5)
                Ask.if.who <- probbaseV5[match(toupper(Ask.if), toupper(probbaseV5[,1])),4]

                Ask.if.row <- match(toupper(Ask.if), toupper(probbaseV5[,1]))
                input.Ask.if <- input.current[Ask.if.row]
                    
                Ask.if.val.tmp <- substr(probbaseV5[j, 16], 6, 6)
                Ask.if.val <- NA
                Ask.if.val[Ask.if.val.tmp=="N"] <- 0
                Ask.if.val[Ask.if.val.tmp=="Y"] <- 1
                Ask.if.subst.tmp <- probbaseV5[Ask.if.row, 6]
                Ask.if.subst <- ifelse(Ask.if.subst.tmp == "Y", 1, 0)
                    
                if (input.current[j]==subst.val) {

                    changeAskIf <- input.Ask.if!=Ask.if.val & subst.val != input.Ask.if
                    if(is.na(changeAskIf)) changeAskIf <- TRUE
                    if (changeAskIf) {
                            
                        input.current[Ask.if.row] <- Ask.if.val
                            
                        if (write) {
                            if (k == 1) {
                                firstPass <- rbind(firstPass,
                                                   paste(index.current, "   ", probbaseV5[j, 4], " (", probbaseV5[j, 3],
                                                         ")  not flagged in category ", probbaseV5[Ask.if.row, 4], " (",
                                                         probbaseV5[Ask.if.row, 3], ") - updated in working information", sep="")
                                                   )
                            }
                            if (k == 2) {
                                secondPass <- rbind(secondPass,
                                                    paste(index.current, "   ", probbaseV5[j, 4], " (", probbaseV5[j, 3],
                                                          ")  not flagged in category ", probbaseV5[Ask.if.row, 4], " (",
                                                          probbaseV5[Ask.if.row, 3], ") - updated in working information", sep="")
                                                    )
                            }
                        }
                    }
                }
            }

            if (substr(probbaseV5[j, 17], 1, 5)!="" & !is.na(input.current[j])) {
                    
                NN.only <- substr(probbaseV5[j, 17], 1, 5)

                NN.only.row   <- match(toupper(NN.only), toupper(probbaseV5[,1]))
                input.NN.only <- input.current[NN.only.row]
                input.NN.only <- ifelse(is.na(input.NN.only), 0, input.NN.only)

                if (input.current[j]==subst.val & input.NN.only!=1) {
                    input.current[j] <- NA
                    if (write) {
                        if (k == 1) {
                            firstPass <- rbind(firstPass,
                                               paste(index.current, "   ", probbaseV5[j, 4], " (", probbaseV5[j, 3],
                                                     ") only required for neonates - cleared in working information", sep="")
                                               )
                        }
                        if (k == 2) {
                            secondPass <- rbind(secondPass,
                                                paste(index.current, "   ", probbaseV5[j, 4], " (", probbaseV5[j, 3],
                                                      ") only required for neonates - cleared in working information", sep="")
                                                )
                        }
                    }
                }
            }
        }
    }
    return(list(Output=input.current, firstPass=firstPass, secondPass=secondPass))
 }
