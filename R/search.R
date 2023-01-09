# Exported functions 
# search_collection 

#' Given search query, get top matches
#'
#' @param collection_name string to search against
#' @param query_string created with construct_query function
#' @param topn optional number or results to return
#' @param conn optional SolrClient object
#' @return dataframe of top 5 results
#' @importFrom solrium SolrClient solr_search
#' @importFrom stringr str_trim str_split
#' @export
search_collection = function(query_string, collection_name, topn=1,conn=NULL) {
    if (is.null(conn)) {
	conn = solrium::SolrClient$new()
    }

    results = solrium::solr_search(
				   conn,
				   name= collection_name,
				   params = list(
						 q = query_string,
						 fl = c("title", "authors", 
							"publisher", "year", 
							"doi", "source",
							"journal_title",
							"score"),
						 rows = topn)
    )
}

#' Convert dataframe of citations into search queries
#'
#' @param citations dataframe of citations
#' @return search queries
#' @export
create_queries = function(citations) {

    # check input data
    valid = validate_columns(colnames(citations),
		     expected=c("title", "authors", "year", "publisher", 
				"doi", "journal_title"))
    if(!valid) { stop("columns don't match expected") }	

    # for each string field:
    # normalize string
    sfn = c("title", "authors", "publisher", "doi", "journal_title")
    citations[sfn] = lapply(citations[sfn], normalize_string)

    # for year field
    # confirm either NA or 4 digit year
    # throw error
    valid = validate_years(citations$year)
    if(!valid) { stop("years not within 1800 2025") }
    citations$year = as.character(citations$year)

    # for title, add phrase
    citations$title = sapply(citations$title, add_phrase, "title", 
			     USE.NAMES = FALSE)
    citations$title[citations$title == ""] = NA

    # for rest add field identifiers
    fn = c("authors", "year", "publisher", "doi", "journal_title")
    citations$authors = add_field(citations$authors, "authors")
    citations$year = add_field(citations$year, "year")
    citations$publisher = add_field(citations$publisher, "publisher")
    citations$doi = add_field(citations$doi, "doi")
    citations$journal_title = add_field(citations$journal_title, "journal_title")

    # paste together
    citations[is.na(citations)] = ""
    combined = apply(citations[], 1, paste, collapse=" ") 
    combined = str_trim(combined)
}

add_field = function(col, name) {
    res = sapply(col, add_field_to_string, name, USE.NAMES=FALSE)
    res[res == ""] = NA
    res
}

add_phrase = function(str, name) {
    if(empty_string(str)) {
	return ("")
    }
    paste0('"', name, ":", str, '"')
}

add_field_to_string = function(string, fieldname) {
    elem = str_split(string, " ")[[1]]
    elem = purrr::discard(elem, empty_string)

    res = ""
    if (length(elem) > 0) {
        res = paste0(fieldname, ":", elem)
	res = paste0(res, collapse=" ")
    }
    res
}
