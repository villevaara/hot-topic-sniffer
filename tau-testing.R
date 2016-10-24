
source("R/token_functions.R")
library(tau)

data_initial <- readRDS("data/enriched_estc.Rds")
stopwords <- read.csv("data/stopwords.csv", header = FALSE)
stopwords <- as.character(stopwords$V1)

data_years1700_1710 <- subset(data_initial,
                              publication_year >= 1700 &
                                publication_year <= 1710)

dataset <- data_years1700_1710

all_titles <- as.character(dataset$title)

# tokens <- tokenize_list(all_titles)

tokenized_titles <- tokenize_phrase_list(all_titles)

cleaned_tokenized_titles <- clean_tokens(tokenized_titles, stopwords)

grams2 <- textcnt(all_titles, n = 2, method = "string", verbose = TRUE)
