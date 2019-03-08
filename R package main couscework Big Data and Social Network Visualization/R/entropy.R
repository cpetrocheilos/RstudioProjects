 #' Returns a value for the entropy of a proportion vector
#'
#' This function returns the entropy value for a supplied proportion vector
#'
#' @param p A number vector containing a sequence of p values
#'
#' @return A value containing the entropy value of a numerical vector
#' \describe{
#' \item{e}{The calculated entropy value.}
#' }
#'
#' @author Julian Blau \email{julian.blau@@postgrad.plymouth.ac.uk}, Dan Buckwell \email{dan.buckwell@@postgrad.plymouth.ac.uk}, Clint Cridland \email{clint.cridland@@postgrad.plymouth.ac.uk}, Charalampos Petrocheilos \email{charalampos.petrocheilos@postgrad.plymouth.ac.uk}
#' @keywords internal
#' @noRd
#' @examples
#' x <- entropy(p)
#'
entropy <- function(p){
  #
  # Calculate the entropy and assign it to the variable e
  #
  e <- -sum(p * log(p, base = 2))
  #
  # Return the entropy
  #
  return(e)
}
