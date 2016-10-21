
library(plyr)
# library(stringi)
source("R/token_functions.R")


data_initial <- readRDS("data/enriched_estc.Rds")
stopwords <- read.csv("data/stopwords.csv", header = FALSE)
stopwords <- as.character(stopwords$V1)
data_initial_subset <- data_initial[10000:20000, ]


data_years1710_1720 <- subset(data_initial,
                              publication_year >= 1710 &
                                publication_year <= 1720)

data_years1720_1730 <- subset(data_initial,
                              publication_year >= 1720 &
                                publication_year <= 1730)

data_years1700_1800 <- subset(data_initial,
                              publication_year >= 1700 &
                                publication_year <= 1800)

tokens1710_1720 <- tokenize_dataset_titles(data_years1710_1720, stopwords)

tokens1720_1730 <- tokenize_dataset_titles(data_years1720_1730, stopwords)

tokens1700_1800 <- tokenize_dataset_titles(data_years1700_1800, stopwords)

difference_from_normal <- tokens1720_1730
# subset and discard anything with less than 30 hits
# difference_from_normal <- subset(difference_from_normal,
#                                  count >= 30)

difference_from_normal$general_frequency <-
  tokens1700_1800$frequency[match(difference_from_normal$token,
                                  tokens1700_1800$token)]
difference_from_normal$relative_frequency <- 
  difference_from_normal$frequency / difference_from_normal$general_frequency

difference_from_normal <- difference_from_normal[order(difference_from_normal$relative_frequency,
                             decreasing = TRUE), ]


# write.csv(difference_from_normal,
#           file = "211016_1grams_estc_titles_1710-1720vs1700-1800_alpha.csv")


# data_years1710_1720_southsea <-
#   subset(data_years1710_1720,
#          grepl("south.*sea", tolower(data_years1710_1720$title)))


difference_from_normal100 <- subset(difference_from_normal,
                                 count >= 100)

write.csv(difference_from_normal100,
          file = "211016_1grams_estc_titles_1720-1730vs1700-1800_min100.csv")



# 
# for (title in tolower(data_years1710_1720$title)) {
#   if (grepl("impeachment", title)) {
#     print(title)
#   }
# }

