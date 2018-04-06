

#' Translation list of COD codes
#'
#' This is the translation of COD abbreviation codes into their corresponding
#' full names.
#'
#'
#' @name causetextV5
#' @docType data
#' @format A data frame with the translation of codes to their names for 3
#' pregnancy statuses, 61 CODs (both the version of COD only and COD with group code),
#' and 6 circumstances of mortality (COMCAT).
#' @keywords datasets
#' @examples
#'
#' data(causetextV5)
#'
NULL





#' Perform InterVA5 algorithm and provide graphical summarization of COD
#' distribution.
#'
#' Computes individual cause of death and population cause-specific mortality
#' fractions using the InterVA5 algorithm. Provides a simple graphical
#' representation of the result.
#'
#' To get the most up-to-date version of the package, as well as the past
#' versions, please check the github repository at:
#' \url{https://github.com/jarathomas/InterVA5-R-Replicate/}
#'
#' \tabular{ll}{ Package: \tab InterVA5\cr Type: \tab Package\cr Version: \tab
#' 1.0\cr Date: \tab 2018-02-01\cr License: \tab GPL-3\cr }
#'
#' @name InterVA5-package
#' @docType package
#' @author Jason Thomas, Zehang Li, Tyler McCormick, Sam Clark
#'
#' Maintainer: Jason Thomas <jarathomas@@gmail.com>
#' @references http://www.interva.net/
#' @keywords InterVA
#' @examples
#'
#' data(SampleInputV5)
#' sample.output <- InterVA5(SampleInputV5, HIV = "h", Malaria = "v", 
#'     write = TRUE, directory = tempdir(), filename = "VA5_result", 
#'     output = "extended", append = FALSE)
#'
NULL





#' Conditional probability of InterVA5
#'
#' This is the table of conditional probabilities of symptoms given CODs, along with
#' prior probabilities in the first row. The
#' values are from InterVA-5
#'
#'
#' @name probbaseV5
#' @docType data
#' @format A data frame with 354 observations on 87 variables. The first row contains
#' observations corresponding to prior probabilities; while the subsequent observations
#' (rows 2 - 354) are the conditional probabilities.
#' @keywords datasets
#' @examples
#'
#' data(probbaseV5)
#'
NULL

#' 3 records of Sample Input
#'
#' This is a dataset consisting of 3 arbitrary sample input deaths in the
#' acceptable format of InterVA5. Any dataset that needs to be analyzed by this
#' package should be in the same format. The order of the input fields must
#' not be changed.
#'
#'
#' @name SampleInputV5
#' @docType data
#' @format 3 arbitrary input records.
#' @keywords datasets
#' @examples
#'
#' data(SampleInputV5)
#'
NULL



