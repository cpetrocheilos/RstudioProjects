#' Returns a plot of the name entropy per year
#'
#' This function returns a plot visualising the entropy of baby names for each year from
#' 1996 to 2015
#'
#' @return A plot of the ukbabyname data
#' \describe{
#' \item{Plot}{A ggplot2 plot.}
#'}
#' @author Julian Blau \email{julian.blau@@postgrad.plymouth.ac.uk}, Dan Buckwell \email{dan.buckwell@@postgrad.plymouth.ac.uk}, Clint Cridland \email{clint.cridland@@postgrad.plymouth.ac.uk}, Charalampos Petrocheilos \email{charalampos.petrocheilos@postgrad.plymouth.ac.uk}
#' @export
#' @examples
#' name_entropy()
#'
name_entropy <- function(){
  # Import ukbabyname data from the ukbabyname package, using the getProportion()
  # function to calculate the proportion for each name in each year, grouping unisex names by sex
  entropy_data <- get_proportion(ukbabynames::ukbabynames)
  # Use the entropy() function to summarise the data to calculate the entropy for each sex, each year
  entropy_data <- entropy_data %>%
    group_by(year, sex) %>%
    summarise(entropy = entropy(p))
  # Plot the entropy data as a measure of diversity using ggplot2
  ggplot(entropy_data, aes(
    x = year,
    y = entropy,
    color = sex)) +
    geom_point() +
    geom_line(aes(linetype = sex, color = sex)) +
    labs(x = "Year",
         y = "Entropy as a measure of diversity",
         title = "Entropy of babies' names in the UK, 1996 - 2015",
         subtitle = "Data from the R package ukbabynames") +
    theme(legend.position = "bottom")
}
