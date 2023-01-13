#' Create collection for index
#'
#'
#' @param records a dataframe to be indexed
#' @param overwrite a boolean for whether the index should be overwritten if something with same name already exists
#' @param collection_name name of the collection, defaults to same base name of the data.frame being indexed (records)
#' @description The collection passed in must have the following specified columns:
#' Title, Authors, Year, Publisher, Source, Miscid, Journal Title, DOI
#' @export 
#' @importFrom solrium SolrClient collection_exists collection_create

index_records = function (records, overwrite = F,
			  collection_name=paste0(substitute(records))){

    valid = validate_columns(colnames(records), c("title", "authors", "year",
						     "publisher", "doi", "journal_title",
						     "source", "miscid"))
    if (!valid) {
	stop("colnames problem")
    }
    conn = solrium::SolrClient$new()

    if (solrium::collection_exists(conn, collection_name)&!overwrite){
	    warning("\"", collection_name, "\"", " collection already exists, not overwriting\n")
		  return (-1)
    }
    if (solrium::collection_exists(conn, collection_name)&overwrite){
      message("collection ", "\"", collection_name, "\"", " already exists, deleting")
      solrium::collection_delete(conn = conn, name = collection_name)
    }
    message('creating new collection ', "\"", collection_name, "\"")
    collection_create(conn, name = collection_name , numShards = 1)
    conn$add(records, collection_name)
}
