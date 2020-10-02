#' Oregon DEQ NHD
#'
#'Table of National Hydrography Dataset (NHD) in Oregon. The table corresponds
#'to NHDH_OR_931v220, which is the current version used for DEQ business data.
#'
#' \itemize{
#'   \item Permanent_Identifier:
#'   \item FDate: Date of last feature modification.
#'   \item Resolution: Source resolution.
#'   \item GNIS_ID: Unique identifier assigned by GNIS, length 10.
#'   \item GNIS_Name: Proper name, specific term, or expression by which a particular geographic entity is known, length 65
#'   \item LengthKM: 	Length of linear feature based on Albers Equal Area, length 8.
#'   \item ReachCode: Unique identifier composed of two parts.  The first eight digits is the subbasin code as defined by FIPS 103.  The next six digits are randomly assigned, sequential numbers that are unique within a subbasin, length 14.
#'   \item FlowDir: Direction of flow relative to coordinate order, length 4.
#'   \item WBArea_Permanent_Identifier:
#'   \item FType: Three-digit integer value; unique identifier of a feature type.
#'   \item FCode: Five-digit integer value; composed of the feature type and combinations of characteristics and values.
#'   \item MainPath:
#'   \item InNetwork:
#'   \item StreamOrder: Strahler stream order number for the reach.
#' }
#'
#' @docType data
#' @usage data(ornhd)
#' @keywords datasets
#' @examples
#' anomaly_stats <- odeqcdr::ornhd
#'

"ornhd"
