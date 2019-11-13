#' Get the symptoms with the largest conditional probability (symptom | cause) for causes assigned by InterVA-5.
#'
#' The function takes an interVA5 object and the data used to assign the causes, and returns the
#' the symptoms that contribute to the cause assignment (ranked in order of the conditional
#' probabilities of observing a symptom, given the death is due to that particular cause).
#'
#' @param object An interVA5 object (i.e., the results returned from the InterVA5() function).
#' @param data The input data that InterVA5 used to assign the causes of death.
#' @param IDs A vector that contains the IDs for each death (note that all of IDs are contained
#' in data$ID and object$ID).
#' @param pretty A logical indicating if you want the results in an easy-to-read format (default is `TRUE`)
#' @param includeAll A logical indicating if you want all of the symptoms included in the output
#' (even those which are absent or have a value of missing/no) (default is `FALSE` which only includes
#' symptoms that are present).
#' 
#' @return \item{dist.cod}{A list of results for each death (organized by ID).  For each death, a list
#' is returned that includes the death's ID, the cause, and a vector of strings listing a symptom,
#' it if contributes to the cause assignment (if includeAll = TRUE), and the conditional probability of
#' observing the symptom given that the death is due to this cause.}
#' 
#' @author Jason Thomas, Zehang LI, Tyler McCormick, Sam Clark
#' @keywords interVA
#' @seealso \code{\link{InterVA5}, \link{getTopSymptoms}}
#' @export whyNotCOD
#' @examples
#'
#' \dontrun{
#' data(RandomVA5)
#' sample.output <- InterVA5(RandomVA5, HIV = "h", Malaria = "v", write=FALSE)
#' topSymptoms <- getTopSymptoms(object = sample.output,
#'                               data = RandomVA5,
#'                               IDs = sample.output$ID[1],
#'                               pretty = TRUE,
#'                               includeAll = FALSE)
#' }
#'
getTopSymptoms <- function(object, data, IDs = NULL, pretty = TRUE, includeAll = FALSE){

    if (is.null(IDs)) IDs <- object$ID

    data("causetextV5", envir = environment())
    causetextV5 <- get("causetextV5", envir = environment())

    data(probbaseV5, envir = environment())
    probbaseV5 <- get("probbaseV5", envir = environment())
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "I"  ] <- 1
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "A+" ] <- 0.8
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "A"  ] <- 0.5
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "A-" ] <- 0.2
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "B+" ] <- 0.1
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "B"  ] <- 0.05
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "B-" ] <- 0.02
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "B -"] <- 0.02
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "C+" ] <- 0.01
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "C"  ] <- 0.005
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "C-" ] <- 0.002
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "D+" ] <- 0.001
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "D"  ] <- 5e-04
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "D-" ] <- 1e-04
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "E"  ] <- 1e-05
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "N"  ] <- 0
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == ""   ] <- 0
    probbaseV5[1, 1:17] <- rep(0, 17)
 
    nSymp <- ncol(data)
    substantive.value.vector <- rep(NA, length = nSymp)
    substantive.value.vector[probbaseV5[,6] == "N"] <- 0
    substantive.value.vector[probbaseV5[,6] == "Y"] <- 1

    dataMatrix <- matrix(NA, nrow = nrow(data), ncol = ncol(data))
    dataMatrix[data == "Y" | data == "y"] <- 1
    dataMatrix[data == "N" | data == "n"] <- 0
    dataMatrix[,1] <- 1:nrow(dataMatrix)

    topSymptoms <- vector(mode = "list", length = length(IDs))
    names(topSymptoms) <- paste0("ID", 1:length(IDs))

    for (i in 1:length(IDs)) {
        
        indexData <- which(data$ID == IDs[i])
        
        tmp <- DataCheck5(dataMatrix[indexData,],
                          id = dataMatrix[indexData, 1],
                          probbaseV5 = probbaseV5,
                          write = FALSE)
        input.current <- tmp$Output
        new.input <- rep(0, nSymp)
        for (y in 2:nSymp) {
            if (!is.na(input.current[y])) {
                if (input.current[y] == substantive.value.vector[y]) {
                  new.input[y] <- 1
                }
            }
        }
        input.current[input.current == 0] <- 1
        input.current[1] <- 0
        input.current[is.na(input.current)] <- 0

        included <- c(FALSE, new.input[2:length(input.current)] == 1)

        cod1 <- object$VA5[[i]]$CAUSE1
        cod2 <- object$VA5[[i]]$CAUSE2
        cod3 <- object$VA5[[i]]$CAUSE3
        codVec <- c(cod1, cod2, cod3)
        nCOD <- sum( codVec != " " )
        topSymp1 <- topSymp2 <- topSymp3 <- NULL
        if (nCOD == 0) {
            topSymptoms[[i]] <- list(ID = IDs[i],
                                     Causelabels = NULL,
                                     Cause1_Symptoms = topSymp1,
                                     Cause2_Symptoms = topSymp2,
                                     Cause3_Symptoms = topSymp3)
            next
        }

        for (j in 1:nCOD) {

            indexCOD <- which(causetextV5[, 2] == codVec[j])
            codeCOD <- causetextV5[indexCOD, 1]
            indexProbbase <- which(colnames(probbaseV5) == codeCOD)
            topSymp <- cbind(probbaseV5[, 1:2],
                             included,
                             probbaseV5[, indexProbbase])[-1, ] ## don't need prior or ID
            condProb <- as.numeric(probbaseV5[-1, indexProbbase])
            newOrder <- order(condProb, decreasing = TRUE)
            colnames(topSymp) <- c("Symptom Label", "Description",
                                   "Symptom Contributes to Propensity",
                                   "Conditional Probability")
            topSymp[topSymp[, 3] == "FALSE", 3] <- "no"
            topSymp[topSymp[, 3] == "TRUE", 3] <- "yes"
            topSymp <- topSymp[newOrder,]

            if (!includeAll) {
                topSymp <- topSymp[which(topSymp[, 3] == "yes"), c(1, 2, 4)]
            }

            if (pretty) {
                newCol1 <- paste0("(", topSymp[, 1], ")")
                newCol2 <- topSymp[, 2]
                newTopSymptoms <- cbind(newCol1, newCol2)
                if (includeAll) {
                    newCol3 <- paste0("[ contributes: ", topSymp[, 3], "]")
                    newCol4 <- paste0("[ cond. prob: ", topSymp[, 4], "]")
                    newTopSymptoms <- cbind(newTopSymptoms, newCol3, newCol4)
                }
                if (!includeAll) {
                    newCol3 <- paste0("[ cond. prob: ", topSymp[, 3], "]")
                    newTopSymptoms <- cbind(newTopSymptoms, newCol3)
                }
                prettytopSympoms <- apply(newTopSymptoms, 1, paste, collapse = " ")
                topSymp <- prettytopSympoms
            }
            assign(paste0("topSymp", j), topSymp)
        }
        topSymptoms[[i]] <- list(ID = as.character(IDs[i]),
                                 Causelabels = codVec[codVec != " "],
                                 Cause1_Symptoms = topSymp1,
                                 Cause2_Symptoms = topSymp2,
                                 Cause3_Symptoms = topSymp3)
    }
    return(topSymptoms)
}

#' Get the symptoms with the largest conditional probability (symptom | cause) using VA data.
#'
#' The function takes takes verbal autopsy data (which can be passed to InterVA5() to assign
#' causes of death), and returns the the symptoms that contribute to the assignment of a 
#' particular cause of death.  This function differs from getTopSymptom() in that the user
#' specified the cause for which they would like the results.  This is an interactive function
#' in the sense that if a cause is not provided as an argument, then the function will print out
#' a numbered list of possible causes and the user can enter in the number to identify the
#' cause of interest.
#'
#' @param data The input data that InterVA5 used to assign the causes of death.
#' @param IDs A vector that contains the IDs for each death (note that all of IDs are contained
#' in data$ID and object$ID).
#' @param cause A string giving the name of the cause for which the conditional probabilities will
#' be returned.
#' @param pretty A logical indicating if you want the results in an easy-to-read format (default is `TRUE`).
#' @param includeAll A logical indicating if you want all of the symptoms included in the output
#' (even those which are absent or have a value of missing/no) (default is `FALSE` which only includes
#' symptoms that are present).
#' 
#' @return \item{dist.cod}{A list of results for each death (organized by ID).  For each death, a list
#' is returned that includes the death's ID, the cause, and a vector of strings listing a symptom,
#' it if contributes to the cause assignment (if includeAll = TRUE), and the conditional probability of
#' observing the symptom given that the death is due to this cause.}
#' 
#' @author Jason Thomas, Zehang LI, Tyler McCormick, Sam Clark
#' @keywords interVA
#' @seealso \code{\link{InterVA5}, \link{whyNotCOD}}
#' @export getTopSymptoms
#' @examples
#'
#' \dontrun{
#' data(RandomVA5)
#' whyNotCOD(data = RandomVA5,
#'           IDs = RandomVA5$ID[1],
#'           pretty = TRUE,
#'           includeAll = FALSE)
#' 
#' data(causetextV5)
#' causetextV5[22, 2]
#' whyNotCOD(data = RandomVA5,
#'           IDs = RandomVA5$ID[1],
#'           cause = causetextV5[22, 2], 
#'           pretty = TRUE,
#'           includeAll = FALSE)
#' }
#'
whyNotCOD <- function(data, IDs = NULL, cause = NULL, pretty = TRUE, includeAll = FALSE){

    ## if (!interactive()) {
    ##     stop("This function is designed for use in interactive sessions")
    ## }
    
    flagNonNumeric <- function(x) {
        return(tryCatch(as.numeric(x), error = function(c) -9999, 
            warning = function(c) -9999))
    }

    if (is.null(IDs)) IDs <- data$ID

    if (anyNA(IDs)) {
        stop("Variable data$ID has NAs (please assign valid IDs).")
    }

    data("causetextV5", envir = environment())
    causetextV5 <- get("causetextV5", envir = environment())

    data(probbaseV5, envir = environment())
    probbaseV5 <- get("probbaseV5", envir = environment())
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "I"  ] <- 1
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "A+" ] <- 0.8
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "A"  ] <- 0.5
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "A-" ] <- 0.2
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "B+" ] <- 0.1
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "B"  ] <- 0.05
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "B-" ] <- 0.02
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "B -"] <- 0.02
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "C+" ] <- 0.01
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "C"  ] <- 0.005
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "C-" ] <- 0.002
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "D+" ] <- 0.001
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "D"  ] <- 5e-04
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "D-" ] <- 1e-04
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "E"  ] <- 1e-05
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == "N"  ] <- 0
    probbaseV5[,18:ncol(probbaseV5)][probbaseV5[,18:ncol(probbaseV5)] == ""   ] <- 0
    probbaseV5[1, 1:17] <- rep(0, 17)
 
    nSymp <- ncol(data)
    substantive.value.vector <- rep(NA, length = nSymp)
    substantive.value.vector[probbaseV5[,6] == "N"] <- 0
    substantive.value.vector[probbaseV5[,6] == "Y"] <- 1

    dataMatrix <- matrix(NA, nrow = nrow(data), ncol = ncol(data))
    dataMatrix[data == "Y" | data == "y"] <- 1
    dataMatrix[data == "N" | data == "n"] <- 0
    dataMatrix[,1] <- 1:nrow(dataMatrix)

    topSymptoms <- vector(mode = "list", length = length(IDs))
    names(topSymptoms) <- paste0("ID", 1:length(IDs))

    if (is.null(cause)) {

        optionNumbers <- cbind(1:61, ": ", causetextV5[4:64, 2])
        
        cat(paste("Please enter the number for the cause you want to analyze: \n"),
            paste("\n"),
            paste(1:61, ": ", causetextV5[4:64, 2], "\n", sep = ''))

        ANSWER <- readline("[Enter number from the list of causes above:] ")
        causeID <- flagNonNumeric(ANSWER)
        if (!causeID %in% 1:61) {
            stop("You must enter a number between 1 and 61.")
        }

        if (round(causeID) != causeID) {
            warning("Expected integer (so rounding number)")
            causeID <- round(causeID)
        }

        cause <- causetextV5[3 + causeID, 2]

        cat(paste0("\n", "Returning results for the cause \n",
                  "\n \t", causeID, ": ", cause, "\n \n"))
        
    }

    for (i in 1:length(IDs)) { # i = 1
        
        indexData <- which(data$ID == IDs[i])
        
        tmp <- DataCheck5(dataMatrix[indexData,],
                          id = dataMatrix[indexData, 1],
                          probbaseV5 = probbaseV5,
                          write = FALSE)
        input.current <- tmp$Output
        new.input <- rep(0, nSymp)
        for (y in 2:nSymp) {
            if (!is.na(input.current[y])) {
                if (input.current[y] == substantive.value.vector[y]) {
                  new.input[y] <- 1
                }
            }
        }
        input.current[input.current == 0] <- 1
        input.current[1] <- 0
        input.current[is.na(input.current)] <- 0

        included <- c(FALSE, new.input[2:length(input.current)] == 1)

        indexCOD <- which(causetextV5[, 2] == cause)
        codeCOD <- causetextV5[indexCOD, 1]
        indexProbbase <- which(colnames(probbaseV5) == codeCOD)
        topSymp <- cbind(probbaseV5[, 1:2],
                         included,
                         probbaseV5[, indexProbbase])[-1, ] ## don't need prior or ID
        condProb <- as.numeric(probbaseV5[-1, indexProbbase])
        newOrder <- order(condProb, decreasing = TRUE)
        colnames(topSymp) <- c("Symptom Label", "Description",
                               "Symptom Contributes to Propensity",
                               "Conditional Probability")
        topSymp[topSymp[, 3] == "FALSE", 3] <- "no"
        topSymp[topSymp[, 3] == "TRUE", 3] <- "yes"
        topSymp <- topSymp[newOrder,]

        if (!includeAll) {
            topSymp <- topSymp[which(topSymp[, 3] == "yes"), c(1, 2, 4)]
        }

        if (pretty) {
            newCol1 <- paste0("(", topSymp[, 1], ")")
            newCol2 <- topSymp[, 2]
            newTopSymptoms <- cbind(newCol1, newCol2)
            if (includeAll) {
                newCol3 <- paste0("[ contributes: ", topSymp[, 3], "]")
                newCol4 <- paste0("[ cond. prob: ", topSymp[, 4], "]")
                newTopSymptoms <- cbind(newTopSymptoms, newCol3, newCol4)
            }
            if (!includeAll) {
                newCol3 <- paste0("[ cond. prob: ", topSymp[, 3], "]")
                newTopSymptoms <- cbind(newTopSymptoms, newCol3)
            }
            prettytopSympoms <- apply(newTopSymptoms, 1, paste, collapse = " ")
            topSymp <- prettytopSympoms
        }
        
        topSymptoms[[i]] <- list(ID = as.character(IDs[i]),
                                 topSymptoms = topSymp)
    }
    return(topSymptoms)
}
