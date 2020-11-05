#' Import continuous result information from a template csv file.
#'
#' Retrieve monitoring results from Oregon DEQ's continuous data submission template v2.03 that has been saved as a csv
#' This function will read all csv files from a selected directory, combine them all using [rbind] and return them as a single dataframe.
#' The column names must be the same in every csv file.
#'
#' Column names are made into syntactically valid names acceptable by R.
#' @param path The path to the directory where the csv files are saved. Default is NULL which implements the [choose.dir] function on windows OS
#' or [rstudioapi::selectDirectory] on Mac/Linux OS when using Rstudio. If Rstudio is not avaibile a path must be provided.
#' @param col_rename named character vector, with new names as values, and old names as names.c("new_name=old_name")
#' @export
#' @return data frame

contin_results_csv <- function(path=NULL, col_rename=NULL) {

  if(is.null(path) & exists('utils::choose.dir')){
    # Windows
    path<-choose.dir()
  }

  if(is.null(path) & Sys.getenv("RSTUDIO") == "1"){
    # Mac/Linux and running Rstudio
    path<-rstudioapi::selectDirectory()
  } else {
    stop("path must be provided.")
  }

  files <- list.files(path=path, pattern="*.csv$", all.files=FALSE, recursive = FALSE, full.names = TRUE)

  df.all <- data.frame()

  for(file in files) {

    df <- read.csv(file=file, header = TRUE, stringsAsFactors = FALSE)

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

  # convert date and time columns to POSIXct, combine them
  df.all$Activity.Start.Time <- as.POSIXct(paste(df.all$Activity.Start.Date, df.all$Activity.Start.Time), format="%Y/%m/%d %H:%M:%S", tz="UTC")
  df.all$Activity.Start.Date <- df.all$Activity.Start.Time

  return(df.all)

}


