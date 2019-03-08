#' Returns a plot of comparing the number of unique names per year
#'
#' This function returns a plot visualising the number of unique baby names for each year from
#' 1996 to 2015
#'
#' @return A plot of the ukbabyname data:
#'\describe{
#'\item{Plot}{A ggplot2 plot.}
#'}
#'
#' @author Julian Blau \email{julian.blau@@postgrad.plymouth.ac.uk}, Dan Buckwell \email{dan.buckwell@@postgrad.plymouth.ac.uk}, Clint Cridland \email{clint.cridland@@postgrad.plymouth.ac.uk}, Charalampos Petrocheilos \email{charalampos.petrocheilos@postgrad.plymouth.ac.uk}
#' @export
#' @examples
#' name_variation()
#'
name_variation <- function(){
  #
  # Create a data frame containing the data for each year and sex, along with a column counting
  # the number of unique values for each
  #
  number_names_per_year <- ukbabynames::ukbabynames %>%
    group_by(year, sex) %>%
    select(year, sex) %>%
    count(sex)
  #
  # Use ggplot2 to plot the number of unique names for each year, plotting individual lines for each sex
  #
  ggplot(number_names_per_year,
         aes(x = year,
             y = n,
             group = sex,
             colour = sex)) +
    geom_point() +
    geom_line(aes(linetype = sex, color = sex)) +
    scale_x_continuous(breaks = seq(1995, 2015, by = 5), minor_breaks = seq(1995, 2015, 1),  limits= c(1995.5, 2015.5)) +
    scale_y_continuous(breaks = seq(3000, 8000, by = 1000), limits = c(3000.5, 8000.5)) +
    #geom_smooth(fill = NA, linetype = 0) +
    labs(x = "Year",
         y = "Number of Different Names",
         title = "Number of different babies' names in the UK, 1996 - 2015",
         subtitle = "Data from the R package ukbabynames") +
    theme(legend.position = "bottom")
}
