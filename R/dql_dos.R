#' Assigns DO concentration DQLs to DO saturation
#'
#'  This function takes the accDQL, precDQL, and rDQL from DO concentration results and applies them to DO saturation
#'  results, as the measurements are taken from the same sensor. This is consistent with how the lab treats DOsaturation
#'  DQLs. This script should be run after DO concentration results have been assigned final grades.
#'
#'@param df Dataframe of results after manual grade corrections have been applied to DO concentration values. Default is results.
#'@export
#'@return Dataframe of all results with final grade.


dql_dos <- function(df = results){


if(!"Dissolved oxygen saturation" %in% df$Characteristic.Name){

  print("No DOs results. Returning dataframe.")
  return(df)
} else {

DOs_DQLs <- df %>%
  dplyr::filter(Characteristic.Name == "Dissolved oxygen (DO)") %>%
  dplyr::select(Monitoring.Location.ID, Activity.Start.Date,Activity.Start.Time, Equipment.ID, Result.Status.ID,
                datetime, accDQL, precDQL, rDQL) %>%
  dplyr::rename(new_accDQL = accDQL,
                new_precDQL = precDQL,
                new_rDQL = rDQL) %>%
  dplyr::mutate(Characteristic.Name = "Dissolved oxygen saturation")

df_joined <- df %>%
  dplyr::left_join(DOs_DQLs, by = c("Monitoring.Location.ID", "Activity.Start.Date", "Activity.Start.Time",
                                      "Equipment.ID","Characteristic.Name", "Result.Status.ID", "datetime")) %>%
  dplyr::mutate(accDQL =  ifelse(Characteristic.Name == "Dissolved oxygen saturation" & !is.na(new_accDQL), new_accDQL, accDQL),
                precDQL = ifelse(Characteristic.Name == "Dissolved oxygen saturation" & !is.na(new_precDQL), new_precDQL, precDQL),
                rDQL =    ifelse(Characteristic.Name == "Dissolved oxygen saturation" & !is.na(new_rDQL), new_rDQL, rDQL)
         ) %>%
  dplyr::select(-new_accDQL, -new_precDQL, -new_rDQL )

return(df_joined)

}
}
