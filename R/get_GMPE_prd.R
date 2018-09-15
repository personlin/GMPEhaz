#' To obtain usable period range of GMPE
#'
#' \code{get.GMPE.prd} returns the the minimum and maximum period of any GMPE.
#'
#' @param GMPE Name of GMPE.
#'
#' @return A data.frame of period.min and period.max will be return.
#'
#' @examples
#' get.GMPE.prd("ASK14")
#' get.GMPE.prd("CY14")
#'
#' @export
get.GMPE.prd <- function(GMPE){
  if (!GMPE %in% GMPEhaz::GMPEhaz.model$name) {
    stop("GMPE not in the list! \n\n")
  }
  out <- subset(GMPEhaz::GMPEhaz.model, name == GMPE)[,c("period.min", "period.max")]
  return(out)
}
