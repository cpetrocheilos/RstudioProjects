#' Uses the EU referendum data to plot the leave voters against the diversity score
#'
#' This function provides a plot of the number of leave voters in each voting district
#' against it's diversity score
#'
#'
#' @return A plot visualising the percentage of leave votes against the diversity of the area
#'\describe{
#'\item{Plot}{A ggplot2 plot.}
#'}
#'
#' @author Julian Blau \email{julian.blau@@postgrad.plymouth.ac.uk}, Dan Buckwell \email{dan.buckwell@@postgrad.plymouth.ac.uk}, Clint Cridland \email{clint.cridland@@postgrad.plymouth.ac.uk}, Charalampos Petrocheilos \email{charalampos.petrocheilos@postgrad.plymouth.ac.uk}
#' @export
#' @examples
#' referendum_diversity()
#'
referendum_diversity <- function(){

  referendum_data <- referendum_data %>%
    mutate(total_voters = Votes_Remain + Votes_Leave,
           remain_p = Votes_Remain / total_voters,
           leave_p = Votes_Leave / total_voters)
  referendum_data = referendum_data[, c(1:4, 9, 5, 10, 8, 6, 7)]

  referendum_data$Country <- as.factor(referendum_data$Country)

  ggplot(referendum_data, aes(
    x = Diversity_Score,
    y = leave_p,
    color = Country)) +
    geom_point(aes(
      size = total_voters),
      alpha = 0.35) +
    stat_smooth(
      method = "lm",
      se = FALSE,
      color = "YELLOW") +
    scale_size(
      name = waiver(),
      breaks = c(100000, 200000, 300000, 400000),
      labels = c("100,000", "200,000", "300,000", "400,000"),
      limits = NULL,
      range = c(1, 6),
      trans = "identity",
      guide = "legend") +
    facet_wrap(~ Country) +
    geom_hline(yintercept = 0.50) +
    scale_x_log10(
      labels = scales::percent,
      breaks = c(.01,.02,.05,.10,.25)) +
    scale_y_continuous(
      labels = scales::percent,
      breaks = seq(0.00, 0.80, by = 0.10),
      limits = c(0.00, 0.80)) +
    scale_color_manual(values = c('red','blue','green')) +
    guides(color = guide_legend(override.aes = list(size = 6))) +
    labs(
      x = "Percentage with Unique Surnames (using a logarithmic scale)",
      y = "Percentage Voting Leave",
      size = "Votes Cast",
      title = "The representation of leave votes to district diversity")
}

