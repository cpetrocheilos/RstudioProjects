#' Compares the proportion of children with a given name between 1996 and 2015
#'
#' This function provides a plot of the proportion of children with a given name
#' between the years 1996 and 2015, grouped by sex and name
#'
#' @param given_names A string vector containing the names to plot
#'
#' @return A plot comparing the proportion of babies having the specified name(s) between 1996 and 2015
#' \describe{
#' \item{Plot}{A ggplot2 plot.}
#' }
#'
#' @author Julian Blau \email{julian.blau@@postgrad.plymouth.ac.uk}, Dan Buckwell \email{dan.buckwell@@postgrad.plymouth.ac.uk}, Clint Cridland \email{clint.cridland@@postgrad.plymouth.ac.uk}, Charalampos Petrocheilos \email{charalampos.petrocheilos@postgrad.plymouth.ac.uk}
#' @export
#' @examples
#' # Plot a singular name directly
#' compare_names("John")
#'
#' # Plot multiple names directly
#' compare_names(c("John", "Jane"))
#'
#' # Plot multiple names using a vector variable
#' names <- c("John", "Jane", "Jack", "Sam")
#' compare_names(names)
#'
compare_names <- function(given_names){
  #
  # Use the get_proportion() method to create a data frame containing the ukbabyname data set
  # with the inclusion of the proportion of of each name for the given year and sex
  #
  names_to_plot <- get_proportion(ukbabynames::ukbabynames)
  #
  # Perform a subset on the names_to_plot data frame to select only the names passed to the function
  #
  names_to_plot <- subset(names_to_plot, names_to_plot$name %in% given_names)
  #
  # Use ggplot2 to create a set of facet_wrapped graphs plotting proportion per sex against year
  # for each name given
  #
  ggplot(names_to_plot, aes(
      x = year,
      y = p,
      colour = factor(sex))) +
    geom_point() +
    geom_line() +
    geom_smooth(fill = NA, method = "loess") +
    facet_wrap(~name,
      scales = "free") +
    theme(
      legend.position = "bottom") +
    labs(
      title = "Babies' names in the UK, 1996 − 2015",
         subtitle = "Data from the R package ukbabynames",
         x = "Year",
         y = "Proportion of babies having the given name",
         colour = "Sex") +
    scale_y_continuous(labels = scales::percent)
}
