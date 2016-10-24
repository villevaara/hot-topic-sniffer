
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

# difference_from_normal <- tokens1710_1720
# 
# difference_from_normal$general_frequency <-
#   tokens1700_1800$frequency[match(difference_from_normal$token,
#                                   tokens1700_1800$token)]
# 
# difference_from_normal$relative_frequency <- 
#   difference_from_normal$frequency / difference_from_normal$general_frequency
# 
# difference_from_normal <- difference_from_normal[order(difference_from_normal$relative_frequency,
#                              decreasing = TRUE), ]
# 

get_difference_from_normal <- function (sample_token_set,
                                        normal_token_set,
                                        cut_off_count = NA,
                                        save_csv = FALSE) {
  
  difference_from_normal <- sample_token_set
  
  difference_from_normal$general_frequency <-
    normal_token_set$frequency[match(difference_from_normal$token,
                                    normal_token_set$token)]
  
  difference_from_normal$relative_frequency <- 
    difference_from_normal$frequency / difference_from_normal$general_frequency
  
  difference_from_normal <-
    difference_from_normal[order(difference_from_normal$relative_frequency,
                                 decreasing = TRUE), ]
  
  if (!is.na(cut_off_count)) {
    difference_from_normal <- subset(difference_from_normal,
                                     count >= 100)
  }
  
  if (save_csv) {
    sample_set_name <- deparse(substitute(sample_token_set))
    normal_set_name <- deparse(substitute(normal_token_set))
    date_and_time <- format(Sys.time(), "%Y-%m-%d %H:%M")
    directory <- "output"
    
    filename <- paste(directory, "/", sample_set_name, "_",
                      normal_set_name, "_", date_and_time, ".csv",
                      sep = "", collapse = "")
    
    write.csv(difference_from_normal,
              file = filename)
  }
  
  return (difference_from_normal)
}

difference_1710_1720 <- get_difference_from_normal(tokens1710_1720,
                                                   tokens1700_1800,
                                                   cut_off_count = 100,
                                                   save_csv = TRUE)

difference_1720_1730 <- get_difference_from_normal(tokens1720_1730,
                                                   tokens1700_1800,
                                                   cut_off_count = 100,
                                                   save_csv = TRUE)


# 
# for (title in tolower(data_years1710_1720$title)) {
#   if (grepl("impeachment", title)) {
#     print(title)
#   }
# }

