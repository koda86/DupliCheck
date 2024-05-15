#' dupliHunter: Find Similar Duplicates
#'
#' This function identifies similar duplicates in a dataset based on titles and author names.
#' 
#' @param data A data frame containing the dataset.
#' @param title_col The name of the column containing titles.
#' @param author_col The name of the column containing author names.
#' @param doi_col The name of the column containing DOIs.
#' @param threshold The similarity threshold for titles (default is 0.03).
#' 
#' @return A data frame with an additional column 'Duplicate' indicating duplicates.
#' @importFrom parallel detectCores makeCluster clusterEvalQ clusterExport parLapply stopCluster
#' @import stringdist
#' @export
dupliHunter <- function(data, title_col, author_col, doi_col, threshold = 0.03) {
  # Preprocess author names to remove diacritical marks and filter
  preprocess_authors <- function(authors) {
    words <- unlist(strsplit(authors, ' '))
    words <- ifelse(length(words) <= 1, words, words[nchar(words) > 1])
    has_diacritics <- function(x) any(grepl("[^a-zA-Z]", iconv(x, to = "ASCII//TRANSLIT")))
    remove_diacritics <- function(x) iconv(x, to = "ASCII//TRANSLIT")
    if (has_diacritics(words)) words <- remove_diacritics(words)
    filtered_words <- words[!grepl("^[A-Z]+$", words)]
    if (length(filtered_words) == 0) {
      return("xxxx")
    } else {
      return(ifelse(length(filtered_words) > 2, filtered_words[1:2], filtered_words))
    }
  }
  
  # Preprocess titles to escape special characters
  preprocess_titles <- function(title) {
    gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", title) # Filter special characters
  }
  
  preprocessed_authors <- lapply(data[[author_col]], preprocess_authors)
  preprocessed_titles <- sapply(data[[title_col]], preprocess_titles)
  
  # Function to check if two rows are duplicates
  is_duplicate <- function(i, j, data, title_col, author_col, doi_col, threshold, preprocessed_titles, preprocessed_authors) {
    title_i <- preprocessed_titles[i]
    title_j <- preprocessed_titles[j]
    title_identical <- grepl(title_i, title_j)
    
    doi_exist <- data[[doi_col]][i] != "" && data[[doi_col]][j] != ""
    doi_identical <- if (doi_exist) grepl(data[[doi_col]][i], data[[doi_col]][j]) | grepl(data[[doi_col]][j], data[[doi_col]][i]) else FALSE
    
    similarity_score <- stringdist::stringdistmatrix(data[[title_col]][i], data[[title_col]][j], method = "jaccard")
    title_similar <- (similarity_score <= threshold) & !title_identical
    
    authors_i <- preprocessed_authors[[i]]
    authors_j <- preprocessed_authors[[j]]
    common_authors <- any(grepl(authors_i, authors_j) | grepl(authors_j, authors_i))
    
    if (doi_exist) {
      return((title_identical & !doi_identical) | doi_identical | (common_authors & title_similar & !doi_identical))
    } else {
      return(title_identical | (common_authors & title_similar))
    }
  }
  
  # Use parallel processing to check for duplicates
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  
  # Ensure the packages are loaded in the cluster
  clusterEvalQ(cl, {
    library(stringdist)
  })
  
  # Export necessary variables and functions
  clusterExport(cl, varlist = c("data", "title_col", "author_col", "doi_col", "threshold", 
                                "preprocessed_titles", "preprocessed_authors", "is_duplicate", 
                                "preprocess_authors", "grepl", "gsub"), envir = environment())
  
  # Parallel computation using parLapply to ensure we get a list of lists
  duplicate_list <- parLapply(cl, 1:(nrow(data) - 1), function(i) {
    sapply((i + 1):nrow(data), function(j) {
      is_duplicate(i, j, data, title_col, author_col, doi_col, threshold, preprocessed_titles, preprocessed_authors)
    })
  })
  
  stopCluster(cl)
  
  # Flatten the duplicate list and sum duplicates for each row
  duplicate <- rep(0, nrow(data))  # Initialize with zeros
  for (i in 1:(nrow(data) - 1)) {
    duplicate[(i + 1):nrow(data)] <- duplicate[(i + 1):nrow(data)] + unlist(duplicate_list[[i]])
  }
  
  # Insert duplicate column directly after the existing columns
  data <- cbind(data, Duplicate = duplicate)
  
  return(data)
}
