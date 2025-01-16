#' Update Result.Status based on rDQL field
#'
#' This function updates Result.Status field based on rDQL. DQLs of A, B, E, or F are set as Final. DQLs of C or D are
#' set as Rejected.
#'
#' @param df Dataframe to modify.
#' @export
#' @return Dataframe with updated Result.Status
#'
#'

status_update <- function(df){

  df2 <- dplyr::mutate(df, Result.Status.ID = dplyr::case_when(rDQL %in%  c("A", "B", "E", "F") ~ "Final",
                                                               rDQL %in%  c("C", "D") ~ "Rejected",
                                                               TRUE ~ Result.Status.ID)
  )

  return(df2)

}
