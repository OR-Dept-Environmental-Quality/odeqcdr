#' Replace Invalid Characters.
#'
#' This function will replace invalid characters.
#'
#' '@' symbol is replaced with 'at'
#' Any of the following: ` ~ ! # $ % ^ & * \ | ; : ' " < > / ? are replaced with an underscore '_'.
#' Parentheses and brackets are replaced with an underscore.
#' Spaces are replaced with and underscore.
#'
#' @param x string or vector
#' @export
#' @return vector without invalid characters.

inchars <- function(x) {

  # Replace @ with 'at'
  x1 <- gsub(pattern="\\@+", replacement="at", x=x)

  invalid_chars <- "\\~+|\\`+|\\!+|\\#+|\\$+|\\%+|\\^+|\\&+|\\*+|\\(+|\\)+|\\[+|\\{+|\\]+|\\}+|\\;+|\\:+|\"+|\'+|\\,+|\\|+|\\\\+|[<>]|\\/+|\\?+|\\s+"

  # Replace ` ~ ! # $ % ^ & * ( ) [ { ] } \ | ; : \' \" < > / ? [space] with an underscore '_'
  x2 <- gsub(pattern=invalid_chars, replacement="_", x=x1)

  # Replace multiple underscores with a single underscore
  x3 <- gsub(pattern="_+", replacement="_", x=x2)

  return(x3)

  }
