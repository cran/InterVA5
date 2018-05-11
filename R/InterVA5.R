#' Provide InterVA5 analysis on the data input.
#'
#' This function implements the algorithm in the InterVA5 software.  It
#' produces individual cause of death (COD) and population cause-specific mortality
#' fractions.  The output is saved in a .csv file specified by user.
#' The calculation is based on the conditional and prior distribution
#' of 61 CODs. The function can also  save the full probability distibution
#' of each individual to file. All information about each individual is
#' saved to a va class object.
#'
#' Be careful if the input file does not match InterVA5 input format strictly.
#' The function will run normally as long as the number of symptoms are
#' correct. Any inconsistent symptom names will be printed in console as
#' warning. If there is a wrong match of symptom from warning, please change 
#' the input to the correct order.
#'
#' @param Input A matrix input, or data read from csv files in the same format
#' as required by InterVA5. Sample input is included as data(RandomVA5).
#' @param HIV An indicator of the level of prevalence of HIV. The input should
#' be one of the following: "h"(high),"l"(low), or "v"(very low).
#' @param Malaria An indicator of the level of prevalence of Malaria. The input
#' should be one of the following: "h"(high),"l"(low), or "v"(very low).
#' @param write A logical value indicating whether or not the output (including
#' errors and warnings) will be saved to file.  If the value is set to TRUE, the
#' user must also provide a value for the parameter "directory".
#' @param directory The directory to store the output from InterVA5. It should
#' either be an existing valid directory, or a new folder to be created. If no
#' path is given and the parameter for "write" is true, then the function stops
#' and and error message is produced.
#' @param filename The filename the user wish to save the output. No extension
#' needed. The output is in .csv format by default.
#' @param output "classic": The same deliminated output format as InterVA5; or
#' "extended": deliminated output followed by full distribution of cause of
#' death proability.
#' @param append A logical value indicating whether or not the new output
#' should be appended to the existing file.
#' @param groupcode A logical value indicating whether or not the group code
#' will be included in the output causes.
#' @param ... not used
#' @return \item{ID }{ identifier from batch (input) file} \item{MALPREV
#' }{ selected malaria prevalence} \item{HIVPREV }{ selected HIV prevalence}
#' \item{PREGSTAT }{most likely pregnancy status} \item{PREGLIK }{ likelihood of
#' PREGSTAT} \item{PRMAT }{ likelihood of maternal death} \item{INDET
#' }{ indeterminate outcome} \item{CAUSE1 }{ most likely cause} \item{LIK1 }{
#' likelihood of 1st cause} \item{CAUSE2 }{ second likely cause} \item{LIK2 }{
#' likelihood of 2nd cause} \item{CAUSE3 }{ third likely cause} \item{LIK3 }{
#' likelihood of 3rd cause} \item{wholeprob}{ full distribution of causes of death}
#' \item{COMCAT }{ most likely circumstance of mortality}
#' \item{COMNUM }{ likelihood of COMCAT}
#' \item{wholeprob }{ full distribution of causes of death}
#' @author Jason Thomas Zehang Li, Tyler McCormick, Sam Clark
#' @seealso \code{\link{InterVA5.plot}}
#' @references http://www.interva.net/
#' @keywords InterVA
#' @export InterVA5
#' @examples
#'
#' data(RandomVA5)
#' # only fit first 5 observations for a quick illustration
#' RandomVA5 <- RandomVA5[1:5, ]
#' 
#' ## to get easy-to-read version of causes of death make sure the column
#' ## orders match interVA5 standard input this can be monitored by checking
#' ## the warnings of column names
#'
#' sample.output1 <- InterVA5(RandomVA5, HIV = "h", Malaria = "l", write=FALSE, 
#'     directory = tempdir(), filename = "VA5_result", output = "extended", append = FALSE)
#'
#' \dontrun{
#' ## to get causes of death with group code for further usage
#' sample.output2 <- InterVA5(RandomVA5, HIV = "h", Malaria = "l", 
#'     write = FALSE, directory = tempdir(), filename = "VA5_result_wt_code", output = "classic", 
#'     append = FALSE, groupcode = TRUE)
#'}
#' 
#'
InterVA5 <- function (Input, HIV, Malaria, write = TRUE, directory = NULL, filename = "VA5_result", 
                      output = "classic", append = FALSE, groupcode = FALSE,
                      ...) 
{
    va5 <- function(ID, MALPREV, HIVPREV, PREGSTAT, PREGLIK, CAUSE1, LIK1, CAUSE2, LIK2, CAUSE3, LIK3, INDET,
                    COMCAT, COMNUM, wholeprob, ...) {
        ID <- ID
        MALPREV <- as.character(MALPREV)
        HIVPREV <- as.character(HIVPREV)
        PREGSTAT <- PREGSTAT
        PREGLIK <- PREGLIK
        COMCAT <- as.character(COMCAT)
        COMNUM <- COMNUM
        wholeprob <- wholeprob
        va5.out <- list(ID = ID, MALPREV = MALPREV, HIVPREV = HIVPREV, PREGSTAT = PREGSTAT, PREGLIK = PREGLIK, 
                        CAUSE1 = CAUSE1, LIK1 = LIK1, CAUSE2 = CAUSE2, LIK2 = LIK2, CAUSE3 = CAUSE3, LIK3 = LIK3, INDET = INDET,
                        COMCAT=COMCAT, COMNUM=COMNUM, wholeprob = wholeprob)
        va5.out
    }
    save.va5 <- function(x, filename, write) {
        if (!write) {
            return()
        }
        x <- x[-15]
        x <- as.matrix(x)
        filename <- paste(filename, ".csv", sep = "")
        write.table(t(x), file = filename, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
    }
    save.va5.prob <- function(x, filename, write) {
        if (!write) {
            return()
        }
        prob <- unlist(x[15])
        x <- x[-15]
        x <- unlist(c(as.matrix(x), as.matrix(prob)))
        filename <- paste(filename, ".csv", sep = "")
        write.table(t(x), file = filename, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
    }
    if (is.null(directory) & write)
        stop("error: please provide a directory (required when write=TRUE)")
    if (is.null(directory)) 
        directory = getwd()
    dir.create(directory, showWarnings = FALSE)
    globle.dir <- getwd()
    setwd(directory)
    data("probbaseV5", envir = environment())
    probbaseV5 <- get("probbaseV5", envir = environment())
    probbaseV5 <- as.matrix(probbaseV5)
    data("causetextV5", envir = environment())
    causetextV5 <- get("causetextV5", envir = environment())
    if (groupcode) {
        causetextV5 <- causetextV5[, -2]
    }
    else {
        causetextV5 <- causetextV5[, -3]
    }
    if (write) {
        cat(paste("Error & warning log built for InterVA5", Sys.time(), "\n"),
            file = "errorlogV5.txt", append = FALSE)
    }

    Input <- as.matrix(Input)
    if (dim(Input)[1] < 1) {
        stop("error: no data input")
    }
    N <- dim(Input)[1]
    S <- dim(Input)[2]
    if (S != dim(probbaseV5)[1]) {
        stop("error: invalid data input format. Number of values incorrect")
    }
    if (tolower(colnames(Input)[S]) != "i459o") {
        stop("error: the last variable should be 'i459o'")
    }
    data("RandomVA5", envir = environment())
    RandomVA5 <- get("RandomVA5", envir = environment())
    valabels = colnames(RandomVA5)
    count.changelabel = 0
    for (i in 1:S) {
        if (tolower(colnames(Input)[i]) != tolower(valabels)[i]) {
            warning(paste("Input column '", colnames(Input)[i], "' does not match InterVA5 standard: '", valabels[i], "'", sep = ""),
                    call. = FALSE, immediate. = TRUE)
            count.changelabel = count.changelabel + 1
        }
    }
    if (count.changelabel > 0) {
        warning(paste(count.changelabel, "column names changed in input. \n If the change in undesirable, please change in the input to match standard InterVA5 input format."), 
            call. = FALSE, immediate. = TRUE)
        colnames(Input) <- valabels
    }
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
    Sys_Prior <- as.numeric(probbaseV5[1, ])
    D <- length(Sys_Prior)
    HIV <- tolower(HIV)
    Malaria <- tolower(Malaria)
    if (!(HIV %in% c("h", "l", "v")) || !(Malaria %in% c("h","l", "v"))) {
        stop("error: the HIV and Malaria indicator should be one of the three: 'h', 'l', and 'v'")
    }
    if (HIV == "h") 
        Sys_Prior[23] <- 0.05
    if (HIV == "l") 
        Sys_Prior[23] <- 0.005
    if (HIV == "v") 
        Sys_Prior[23] <- 1e-05
    if (Malaria == "h") {
        Sys_Prior[25] <- 0.05 
        Sys_Prior[45] <- 0.05 
    }
    if (Malaria == "l") {
        Sys_Prior[25] <- 0.005
        Sys_Prior[45] <- 1e-05
    }
    if (Malaria == "v") {
        Sys_Prior[25] <- 1e-05
        Sys_Prior[45] <- 1e-05
    }
    ID.list <- rep(NA, N)
    VAresult <- vector("list", N)
    if (write && append == FALSE) {
        header = c("ID", "MALPREV", "HIVPREV", "PREGSTAT", "PREGLIK", 
                   "CAUSE1", "LIK1", "CAUSE2", "LIK2", "CAUSE3", "LIK3", "INDET", "COMCAT", "COMNUM")
        if (output == "extended") 
            header = c(header, as.character(causetextV5[, 2]))
        write.table(t(header), file = paste(filename, ".csv", sep = ""), row.names = FALSE, col.names = FALSE, sep = ",")
    }
    nd <- max(1, round(N/100))
    np <- max(1, round(N/10))
    
    if (write) {
        cat(paste("\n\n", "the following records are incomplete and excluded from further processing:", "\n\n",
                  sep=""), file = "errorlogV5.txt", append = TRUE)
    }

    firstPass  <- NULL
    secondPass <- NULL
    errors <- NULL
    for (i in 1:N) {
        if (i%%nd == 0) {
            cat(".")
        }
        if (i%%np == 0) {
            cat(paste(round(i/N * 100), "% completed\n", sep = ""))
        }

        index.current <- as.character(Input[i, 1])   
        Input[i, which(toupper(Input[i, ]) == "N")] <- "0"
        Input[i, which(toupper(Input[i, ]) == "Y")] <- "1"
        Input[i, which(Input[i, ] != "1" & Input[i, ] != "0")] <- NA
        input.current <- as.numeric(Input[i, ])

        input.current[1] <- 0                      
        if (sum(input.current[6:12], na.rm=TRUE) < 1) {
            if (write) {
                errors <- rbind(errors, paste(index.current, " Error in age indicator: Not Specified "))
            }
            next
        }
        if (sum(input.current[4:5], na.rm=TRUE) < 1) {
            if (write) {
                errors <- rbind(errors, paste(index.current, " Error in sex indicator: Not Specified "))
            }
            next
        }
        if (sum(input.current[21:328], na.rm=TRUE) < 1) {
            if (write) {
                errors <- rbind(errors, paste(index.current, " Error in indicators: No symptoms specified "))
            }
            next
        }

        tmp <- DataCheck5(input.current, id=index.current, probbaseV5=probbaseV5, write=write)
        input.current <- tmp$Output
        firstPass <- rbind(firstPass, tmp$firstPass)
        secondPass <- rbind(secondPass, tmp$secondPass)

        subst.vector <- rep(NA, length=S)
        subst.vector[probbaseV5[,6]=="N"] <- 0
        subst.vector[probbaseV5[,6]=="Y"] <- 1

        new.input <- rep(0, S)
        for(y in 2:S){
            if(!is.na(input.current[y])){
                if(input.current[y]==subst.vector[y]){
                    new.input[y] <- 1
                }
            }
        }

        input.current[input.current==0] <- 1
        input.current[1] <- 0
        input.current[is.na(input.current)] <- 0

        reproductiveAge <- 0
        preg_state      <- " "
        lik.preg        <- " "
        if (input.current[5] == 1 && (input.current[17] == 1 || input.current[18] == 1 || input.current[19] == 1)) {
            reproductiveAge <- 1
        }
        prob <- Sys_Prior[18:D]
        temp <- which(new.input[2:length(input.current)] == 1)
        for (jj in 1:length(temp)) {
            temp_sub <- temp[jj]
            for (j in 18:D) {
                    prob[j - 17] <- prob[j - 17] * as.numeric(probbaseV5[temp_sub + 1, j])
            }
            if (sum(prob[1:3]) > 0) 
                prob[1:3] <- prob[1:3]/sum(prob[1:3])
            if (sum(prob[4:64]) > 0)
                prob[4:64] <- prob[4:64]/sum(prob[4:64])
            if (sum(prob[65:70]) > 0)
                prob[65:70] <- prob[65:70]/sum(prob[65:70])
        }
        names(prob) <- causetextV5[, 2]
        prob_A <- prob[ 1: 3]
        prob_B <- prob[ 4:64]
        prob_C <- prob[65:70]

        ## Determine Preg_State and Likelihood
        if (sum(prob_A) == 0 || reproductiveAge == 0) {
            preg_state <- "n/a"
            lik.preg <- " "
        }
        if (max(prob_A) < 0.1 & reproductiveAge == 1) {
            preg_state <- "indeterminate"
            lik.preg <- " "
        }
        if (which.max(prob_A) == 1 && prob_A[1] >= 0.1 && reproductiveAge == 1) {
            preg_state <- "Not pregnant or recently delivered"
            lik.preg <- round(prob_A[1]/sum(prob_A) * 100)
        }
        if (which.max(prob_A) == 2 && prob_A[2] >= 0.1 && reproductiveAge == 1) {
            preg_state <- "Pregnancy ended within 6 weeks of death"
            lik.preg <- round(prob_A[2]/sum(prob_A) * 100)
        }
        if (which.max(prob_A) == 3 && prob_A[3] >= 0.1 && reproductiveAge == 1) {
            preg_state <- "Pregnant at death"
            lik.preg <- round(prob_A[3]/sum(prob_A) * 100)
        }

        ## Determine the output of InterVA
        prob.temp <- prob_B
        if (max(prob.temp) < 0.4) {
            cause1 <- lik1 <- cause2 <- lik2 <- cause3 <- lik3 <- " "
            indet <- 100
        }
        if (max(prob.temp) >= 0.4) {
            lik1 <- round(max(prob.temp) * 100)
            cause1 <- names(prob.temp)[which.max(prob.temp)]
            prob.temp <- prob.temp[-which.max(prob.temp)]
            lik2 <- round(max(prob.temp) * 100)
            cause2 <- names(prob.temp)[which.max(prob.temp)]
            if (max(prob.temp) < 0.5 * max(prob_B))
                lik2 <- cause2 <- " "
            prob.temp <- prob.temp[-which.max(prob.temp)]
            lik3 <- round(max(prob.temp) * 100)
            cause3 <- names(prob.temp)[which.max(prob.temp)]
            if (max(prob.temp) < 0.5 * max(prob_B)) 
                lik3 <- cause3 <- " "
            top3 <- as.numeric(c(lik1, lik2, lik3))
            indet <- round(100 - sum(top3, na.rm=TRUE))
        }

        ## Determine the Circumstance Of Mortality CATegory (COMCAT) and probability
        if(sum(prob_C) > 0) prob_C <- prob_C/sum(prob_C)
        if(max(prob_C)<.5){
            comcat <- "Multiple"
            comnum <- " "
        }
        if(max(prob_C)>=.5){
            comcat <- names(prob_C)[which.max(prob_C)]
            comnum <- round(max(prob_C)*100)
        }

        ID.list[i] <- index.current
        VAresult[[i]] <- va5(ID = index.current, MALPREV = Malaria, HIVPREV = HIV,
                             PREGSTAT = preg_state, PREGLIK = lik.preg, 
                             CAUSE1 = cause1, LIK1 = lik1, CAUSE2 = cause2, LIK2 = lik2, CAUSE3 = cause3, LIK3 = lik3,
                             INDET = indet, COMCAT=comcat, COMNUM=comnum, wholeprob = c(prob_A, prob_B, prob_C))
        if (output == "classic") 
            save.va5(VAresult[[i]], filename = filename, write)
        if (output == "extended") 
            save.va5.prob(VAresult[[i]], filename = filename, write)
    }
    if (write) {
        cat(errors, paste("\n", "the following data discrepancies were identified and handled:", "\n"), 
            firstPass, paste("\n", "Second pass", "\n"), secondPass, sep="\n", file="errorlogV5.txt", append=TRUE)
    }

    setwd(globle.dir)
    out <- list(ID = ID.list[which(!is.na(ID.list))], VA5 = VAresult[which(!is.na(ID.list))], 
                Malaria = Malaria, HIV = HIV)
    class(out) <- "interVA5"
    return(out)
}
