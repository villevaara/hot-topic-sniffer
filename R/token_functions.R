

tokenize_list <- function (phraselist) {
  phraselist <- as.character(phraselist)
  splitted <- strsplit(phraselist, " +")
  tokens <- unlist(splitted)
  return(tokens)
}


clean_tokens <- function(tokens, stopwords) {
  f1tokens <- subset(tokens, nchar(tokens) > 3)
  f2tokens <- gsub("\\W", "", f1tokens)
  f3tokens <- gsub("\\d", "", f2tokens)
  f4tokens <- subset(f3tokens, nchar(f3tokens) > 3)
  f5tokens <- tolower(f4tokens)
  f6tokens <- f5tokens[!(f5tokens %in% stopwords)]
  return (f6tokens)
}


count_tokens <- function(tokens) {
  counted_tokens <- count(tokens)
  colnames(counted_tokens) <- c("token", "count")
  counted_tokens$token <- as.character(counted_tokens$token)
  return (counted_tokens)
}


get_token_freqs <- function(token_counts) {
  token_counts_freqs <- token_counts
  total_count <- sum(token_counts_freqs$count)
  token_counts_freqs$frequency <- token_counts$count / total_count
  token_counts_freqs <- token_counts_freqs[order(token_counts_freqs$frequency,
                                                 decreasing = TRUE), ]
  return(token_counts_freqs)
}


tokenize_dataset_titles <- function(dataset, stopwords) {
  all_titles <- dataset$title
  tokens <- tokenize_list(all_titles)
  cleaned_tokens <- clean_tokens(tokens, stopwords)
  token_counts <- count_tokens(cleaned_tokens)
  token_freqs <- get_token_freqs(token_counts)
  return(token_freqs)
}

