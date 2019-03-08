#' Returns the ukbabynames data set, along with the proportion of children with each name
#'
#' This function uses the ukbabyname data set to calculate the proportion of
#' children with each name for each year and sex. A data frame is then
#' returned containing this data
#'
#' @param data_set A data frame contailing the data frame to calculate the proportion of
#'
#' @return A data frame the ukbabynames data:
#' \describe{
#' \item{year}{The year the data is from.}
#' \item{sex}{The sex of the child.}
#' \item{name}{The name of the child.}
#' \item{n}{The number of children with the given name.}
#' \item{rank}{The rank of the n.}
#' \item{p}{The proportion of children of the given sex with the same name for that year.}
#' }
#'
#' @author Julian Blau \email{julian.blau@@postgrad.plymouth.ac.uk}, Dan Buckwell \email{dan.buckwell@@postgrad.plymouth.ac.uk}, Clint Cridland \email{clint.cridland@@postgrad.plymouth.ac.uk}, Charalampos Petrocheilos \email{charalampos.petrocheilos@postgrad.plymouth.ac.uk}
#' @keywords internal
#' @noRd
#' @examples
#' x <- get_proportion(ukbabynames::ukbabynames)
#'
get_proportion <- function(data_set){
  #
  # Group the ukbabynames data set by year and sex, then mutate the data frame to include
  # the proportion of children for the given sex in the given year with that name
  #
  names_with_proportion <- data_set %>%
    group_by(year, sex) %>%
    mutate(p = n / sum(n))
  #
  # Returns the new data frame includeing the proportions
  #
  return(names_with_proportion)
}
