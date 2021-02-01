#' Manual update of rDQL field
#'
#' This function is to provide an ability to manually update rDQL field after manual review in shiny app for the volmon
#' program. Input is intended to be the df4.results dataframe must contain the following fields: rDQL, row.results,
#' Result.Comment
#'
#' @param df Dataframe to modify. Should be df4.results. No need to specify if part or pipe structure
#' @param rows Rows to modify. Get these from reviewing the shinyapp
#' @param DQL Updated final DQL value
#' @param comment Comment to be used in Result.Comment field
#' @export
#' @return Dataframe with updated DQL and comment
#'
#'

update_DQL <- function(df, rows, DQL, comment){


  df2 <- df %>%
    dplyr::mutate(rDQL = dplyr::case_when(row.results %in% rows ~ DQL,
                                          TRUE ~ rDQL),
                  Result.Comment = dplyr::case_when(row.results %in% rows ~ comment,
                                                    TRUE ~ Result.Comment)

    )

  return(df2)

}
