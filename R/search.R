# Exported functions 
# search_collection 

#' Given search query, get top matches
#'
#' @param collection_name string to search against
#' @param query_string created with construct_query function
#' @param topn optional number or results to return
#' @param conn optional SolrClient object
#' @return dataframe of top 5 results
#' 

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
#' @param boost_fields optional fields to boost weights
#' @param boost_values optional boost factors. Value is weight relative to other fields (e.g., ^2 is twice the weight) (see https://solr.apache.org/guide/7_3/the-standard-query-parser.html#boosting-a-term-with)
#' @param treat_as_phrase character vector of variables to treat as phrase (i.e., in "quotes"), currently limited to title and journal title options
#' @return search queries
#' @export
create_queries = function(citations,boost_values = NULL,boost_fields = NULL,treat_as_phrase = c('title','journal_title')) {

    # check input data
    valid = validate_columns(colnames(citations),
		     expected=c("title", "authors", "year", "publisher", 
				"doi", "journal_title"))
    if(!valid) { stop("columns don't match expected") }	
    if(!any(treat_as_phrase) %in% c('journal_title','title')){stop(paste0("treating ", paste(treat_as_phrase[!treat_as_phrase %in% c('journal_title','title')],collapse = ',')," as phrase not currently supported"))}
    if(length(boost_fields)!=length(boost_values)|(length(boost_fields)>1&length(boost_values)==1)){stop("must specify a single boost value or 1 boost value for every field to boost")}
    if(!all(boost_fields %in% c("title", "authors", "year", "publisher", 
                                        "doi", "journal_title"))){stop("boost column doesn't match expected")}
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
    if('title' %in% treat_as_phrase){
      # for title, add phrase
      citations$title = sapply(citations$title, add_phrase, "title", 
                                       USE.NAMES = FALSE)
      citations$title[citations$title == ""] = NA
    }else{
      citations$title = add_field(citations$title, "title")
    }
    if('journal_title' %in% treat_as_phrase){
    # for journal_title, add phrase
    citations$journal_title = sapply(citations$journal_title, add_phrase, "journal_title", 
                             USE.NAMES = FALSE)
    citations$journal_title[citations$journal_title == ""] = NA
    }else{
      citations$journal_title = add_field(citations$journal_title, "journal_title")
    }
    # for rest add field identifiers
    fn = c("authors", "year", "publisher", "doi")
    citations$authors = add_field(citations$authors, "authors")
    citations$year = add_field(citations$year, "year")
    citations$publisher = add_field(citations$publisher, "publisher")
    citations$doi = add_field(citations$doi, "doi")
    #citations$journal_title = add_field(citations$journal_title, "journal_title")

    
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
  paste0(name, ':"', str, '"')
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
