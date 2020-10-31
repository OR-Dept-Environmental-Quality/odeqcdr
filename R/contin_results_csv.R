#' Import continuous result information from a template csv file.
#'
#' Retrieve monitoring results from Oregon DEQ's continuous data submission template v2.03 that has been saved as a csv
#' This function will read all csv files from a selected directory, combine them all using [rbind] and return them as a single dataframe.
#' The column names must be the same in every csv file.
#'
#' Column names are made into syntactically valid names acceptable by R.
#' @param col_rename named character vector, with new names as values, and old names as names.c("new_name=old_name")
#' @export
#' @return data frame

contin_results_csv <- function(col_rename=NULL) {

  files <- list.files(path=choose.dir(),pattern="*.csv", all.files=FALSE, recursive = FALSE, full.names = TRUE)

  df.all <- data.frame()

  for(file in files) {

    df <- read.csv(file=file, header = TRUE)

    df.all <- rbind(df.all, df)

  }

  if(is.null(col_rename)) {


    # Replace by position and hope for the best

    # get result column names
    cols <- odeqcdr::cols_results()

    # Make names R friendly
    cols <- make.names(cols)

    # Replace names
    names(df.all) <- cols[1:length(names(df.all))]


  } else {

    # change col names by name, not position.
    df.all <- dplyr::rename(df.all, col_rename)

  }

  return(df.all)

}


