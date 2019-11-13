#' Update the Symptom-Cause-Information source (aka probbaseV5).
#'
#' The function downloads the most recent version of InterVA5, extracts
#' the probbase.xls file (i.e., the Symptom-Cause-Information [SCI] source ), 
#' and returns SCI as a matrix.
#'
#' @return \item{newProbbase}{The Symptom-Cause-Information ("Probbase") used to assign causes of death.}
#' @author Jason Thomas, Zehang LI, Tyler McCormick, Sam Clark
#' @keywords interVA
#' @seealso \code{\link{probbaseV5}}
#' @export download.SCI
#' @examples
#'
#' \dontrun{
#' data(RandomVA5)
#' RandomVA5 <- RandomVA5[1:2, ]
#' newProbbase <- download.SCI()
#' out <- InterVA5(RandomVA5, sci = newProbbase, HIV = "h", Malaria = "l", write=FALSE, 
#'     directory = tempdir(), filename = "VA5_result", output = "extended", append = FALSE)
#' }
#'
download.SCI <- function(){

    zipFile = tempfile(fileext = ".zip")
    curl::curl_download("http://www.byass.uk/interva/InterVA_5_v5.0_release.zip", zipFile)
    utils::unzip(zipfile = zipFile, files = "probbase.xls", exdir = tempdir())
    xlsName <- paste(tempdir(), "probbase.xls", sep = "/")
    newProbbase <- readxl::read_xls(xlsName)
    newProbbase <- newProbbase[-1, ]
    newProbbase[is.na(newProbbase)] <- ""
    newProbbase <- as.matrix(newProbbase)
    message("Downloaded Probbase version:  ", newProbbase[1,3])

    return(newProbbase)
}

#' Print the version of the Symptom-Cause-Information source (aka probbaseV5).
#'
#' The function takes 
#'
#' @param sci a symptom-cause-information matrix 
#' @return \item{}{Message stating the Probbase version.}
#' @author Jason Thomas, Zehang LI, Tyler McCormick, Sam Clark
#' @keywords interVA
#' @seealso \code{\link{download.SCI}}
#' @export version.SCI
#' @examples
#'
#' \dontrun{
#' data(probbaseV5)
#' version.SCI(sci = probbaseV5)
#' }
#'
version.SCI <- function(sci){

    message("Probbase version:  ", sci[1,3])

}
