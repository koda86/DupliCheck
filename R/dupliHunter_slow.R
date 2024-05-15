#' dupliHunter: Find Similar Duplicates
#'
#' This function identifies similar duplicates in a dataset based on titles and author names.
#' @param data A data frame containing the dataset.
#' @param title_col The name of the column containing titles.
#' @param author_col The name of the column containing author names.
#' @param doi_col The name of the column containing DOIs.
#' @param threshold The similarity threshold for titles (default is 0.03).
#' @return A data frame with an additional column 'Duplicate' indicating duplicates.
#' @examples
#' data <- data.frame(
#'   Title = c("Meta Analysis of Studies", "Meta-Analysis of Studies", "A Comprehensive Review", "Comprehensive Review"),
#'   Authors = c("John Doe", "J. Doe", "Jane Smith", "J. Smith"),
#'   DOI = c("10.1000/xyz123", "10.1000/xyz124", "10.1000/xyz125", "10.1000/xyz126"),
#'   stringsAsFactors = FALSE
#' )
#' duplicates <- dupliHunter(data, "Title", "Authors", "DOI")
#' print(duplicates)
dupliHunter <- function(data, title_col, author_col, doi_col, threshold = 0.03) {
  
  duplicate <- integer(nrow(data))
  
  for (i in 1:(nrow(data) - 1)) {
    if (duplicate[i] == 1) next # Skip rows already marked as duplicates
    
    title_identical <- FALSE
    authors_identical <- FALSE
    doi_identical <- FALSE
    
    # Filter author names
    words <- unlist(strsplit(data[[author_col]][i], ' '))
    words <- ifelse(length(words) <= 1, words, words[nchar(words) > 1])
    # Remove diacritical marks if present
    has_diacritics <- function(x) {
      any(grepl("[^a-zA-Z]", iconv(x, to = "ASCII//TRANSLIT")))
    }
    remove_diacritics <- function(x) {iconv(x, to = "ASCII//TRANSLIT")}
    if (has_diacritics(words)) {words <- remove_diacritics(words)}
    filtered_words <- words[!grepl("^[A-Z]+$", words)]
    if (length(filtered_words) == 0) {
      authors_i <- "xxxx"
    } else {
      authors_i <- ifelse(length(filtered_words) > 2, filtered_words[1:2], filtered_words)
    }
    
    for (j in (i + 1):nrow(data)) {
      tryCatch({
        if (duplicate[j] == 1) next # Skip rows already marked as duplicates
        
        # Check for identical pairs
        title_i <- data[[title_col]][i]
        title_j <- data[[title_col]][j]
        title_i_escaped <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", title_i) # Filter special characters
        title_j_escaped <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", title_j)
        title_identical <- grepl(title_i_escaped, title_j_escaped)
        if (data[[doi_col]][i] != "" && data[[doi_col]][j] != "") {
          doi_exist <- TRUE
          doi_identical <- grepl(data[[doi_col]][i], data[[doi_col]][j]) | grepl(data[[doi_col]][j], data[[doi_col]][i])
        } else {
          doi_identical <- FALSE
        }
        
        # Check for "sufficient" similarity between title pairs
        similarity_score <- stringdist::stringdistmatrix(data[[title_col]][i], data[[title_col]][j], method = "jaccard")
        title_similar <- (similarity_score <= threshold) & !title_identical
        
        # Filter author names
        words <- unlist(strsplit(data[[author_col]][j], ' '))
        words <- ifelse(length(words) <= 1, words, words[nchar(words) > 1])
        # Remove diacritical marks if present
        if (has_diacritics(words)) {words <- remove_diacritics(words)}
        filtered_words <- words[!grepl("^[A-Z]+$", words)]
        if (length(filtered_words) == 0 | is.na(filtered_words)) {
          authors_j <- "yyyy"
        } else {
          authors_j <- ifelse(length(filtered_words) > 2, filtered_words[1:2], filtered_words)
        }
        
        common_authors <- any(grepl(authors_i, authors_j) | grepl(authors_j, authors_i))
        
        # Logical check for equality/similarity of different factors (title, author, DOI)
        if (doi_exist) {
          if ((title_identical & !doi_identical) |
              doi_identical |
              (common_authors & title_similar & !doi_identical)) {
            duplicate[j] <- 1
          }
        } else {
          if (title_identical |
              (common_authors & title_similar)) {
            duplicate[j] <- 1
          }
        }
        
      }, error = function(err) { # Belongs to tryCatch
        cat("Error at i =", i, "j =", j, ":", conditionMessage(err), "\n")
      })
    }
  }
  
  print(sum(duplicate))
  
  # Insert duplicate column directly after the existing columns
  data <- cbind(data, Duplicate = duplicate)
  
  return(data)
}

# Example usage
data <- data.frame(
  Title = c("Meta Analysis of Studies", "Meta-Analysis of Studies", "A Comprehensive Review", "Comprehensive Review"),
  Authors = c("John Doe", "J. Doe", "Jane Smith", "J. Smith"),
  DOI = c("10.1000/xyz123", "10.1000/xyz124", "10.1000/xyz125", "10.1000/xyz126"),
  stringsAsFactors = FALSE
)

system.time(
  duplicates <- dupliHunter(data, "Title", "Authors", "DOI")
)
print(duplicates)
