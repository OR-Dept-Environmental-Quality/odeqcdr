#' Import continuous result information from a template csv file.
#'
#' Retrieve monitoring results from Oregon DEQ's continuous data submission template v2.03 that has been saved as a csv
#' This function will read all csv files from a selected directory, combine them all using [rbind] and return them as a single dataframe.
#' The column names must be the same in every csv file.
#'
#' Column names are made into syntactically valid names acceptable by R.
#' @param path The path to the directory where the csv files are saved. Default is NULL which implements the [choose.dir] function on windows OS
#' or [rstudioapi::selectDirectory] on Mac/Linux OS when using Rstudio. If Rstudio is not available a path must be provided.
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

  # Make df col names R friendly
  names(df.all) <- make.names(names(df.all))

  # fix Equipment ID name
  names(df.all) <- gsub(pattern="Equipment.ID..", replacement = "Equipment.ID", x=names(df.all))

  # get standard result column names and make R friendly
  cols <- make.names(odeqcdr::cols_results())

  missing_cols <- cols[!cols %in% names(df.all)]

  warning(paste0("The following columns are missing and have been added: ", paste(missing_cols, collapse = ", ")))

  # Add cols that are not there. Make them NA
  df.all[,missing_cols] <- as.character(NA)

  # convert date and time columns to POSIXct, combine them
  df.all$Activity.Start.Time <- as.POSIXct(paste(df.all$Activity.Start.Date, df.all$Activity.Start.Time), format="%Y/%m/%d %H:%M:%S", tz="UTC")
  df.all$Activity.Start.Date <- df.all$Activity.Start.Time

  return(df.all)

}
