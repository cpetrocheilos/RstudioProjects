#' The referendum data
#'
#' The referendum data is data set containing statistics from the 2016 UK EU Referendum.
#' It reports the number and respose to votes cast per voting district, along with
#' the turnout and diversity score of the area
#'
#' @format A data frame with 372 rows and 7 columns:
#' \describe{
#'  \item{Voting_District}{The voting district.}
#'  \item{Region}{The bigger region to which the voting district belongs.}
#'  \item{Country}{The country to which the region belongs.}
#'  \item{Votes_Remain}{The number of people who voted Remain.}
#'  \item{Votes_Leave}{The number of people who voted Leave.}
#'  \item{Percentage_Voter_Turnout}{The percentage of people eligible to vote who actually voted.}
#'  \item{Diversity_Score}{The proportion of people in the population with unique surnames.}
#' }
#'
#' @examples
#' with(referendum_data, mean(Votes_Remain))
#' with(referendum_data, mean(Votes_Leave))
#'
#' @source Data provided by Julian Stander and Luciana Dalla Valle, but disclosure protected
"referendum_data"
