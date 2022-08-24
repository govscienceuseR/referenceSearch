#' Normalize a string
#'
#' Normalizes a string by setting to lowercase, filtering out punct and digits
#' @param x string to normalize
#' @return normalized string
#' @export
normalize_string = function(x) {
    if (is.null(x)) {
	return (NULL)
    }
    tolower(gsub("[[:punct:][:blank:][:digit:]]+", " ", x))
}

#' Check to see if a string is potentially a valid DOI
#'
#' @param x string input to see if doi
#' @return bool
#' @export
check_doi = function(x) {
    grepl('/^10.\\d{4,9}/[-._;()/:A-Z0-9]+$/i', x)
}

#' Check to see if user has the number of matching columns specified
#' 
#' 
#' @param cols char colnames to check
#' @param expected char names to check against
#' @param return bool
#' @export
validate_columns = function (cols, expected) {
    ms = setdiff(expected, tolower(cols))
    if (length(ms) > 0) {
	return(FALSE)
    }
    return (TRUE)
}

#' Check if years column contains only numbers between 1800 and 2025
#' 
#' 
#' @param x years column
#' @return bool
#' @export
validate_years = function(x) {
    x = as.numeric(x)
    x = x[!is.na(x)]
    if (min(x) < 1800 | max(x) > 2025) {
	return(FALSE)
    }
    return (TRUE)
}

#' Check if the contents of a string should be considered empty
#' 
#' Useful because of the ambiguity of our input data, use with purrr::discard
#'
#' @param x string to search, single value
#' @return bool
empty_string = function(x) {
    if (x== "null" || x == "na" || x == "NULL" || x == "NA" ||
	is.na(x) || x == "") {
	return (TRUE)
    }
    FALSE
}

#' change author format from anystyle output to simple set
#' Anystyle makes json-style outputs that split out first/middle/surname for each author
#' @param x author string set that follows anystyle format
#' @return string
list_authors = function(x) {
  name_extracts <- stringr::str_extract_all(x,'Person[^\\)]+')[[1]]
  split_sets <- stringr::str_extract_all(name_extracts,"'.*?'")
  name_set <- sapply(split_sets,function(x) stringr::str_remove_all(paste0(x[3],", ",x[1]," ",x[2]),"'"))
  paste(name_set,collapse = '; ')
}
