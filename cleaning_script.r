library(ggplot2)
library(dplyr)
library(lubridate)

View(movies_metadata) # View original dataset

# Select the fields we're interested in
cleaned_movies_metadata <- movies_metadata[, c("revenue", "budget", "genres", "original_language", 
                                               "release_date", "runtime") ]

# Remove rows containing zeros
cleaned_movies_metadata <- cleaned_movies_metadata[!apply(cleaned_movies_metadata[,1:6] == 0, 1, FUN = any, na.rm = TRUE),]

# Convert original language to a factor field
cleaned_movies_metadata$original_language <- as.factor(cleaned_movies_metadata$original_language)

# Split release_date field into Year and Month fields, and convert the month field to a factor
cleaned_movies_metadata <- cleaned_movies_metadata %>%
  mutate(release_date = as.Date(release_date, format = "%y-%m-%d")) %>%
  mutate(month = month(release_date)) %>%
  mutate(year = year(release_date))
cleaned_movies_metadata$month <- as.factor(cleaned_movies_metadata$month)

# Get rid of unneccessary release_date field now
cleaned_movies_metadata <- cleaned_movies_metadata[, c("revenue", "budget", "genres", "original_language", 
                                                       "year", "month", "runtime") ]

# Convert original language to an integer field
cleaned_movies_metadata$runtime <- as.integer(cleaned_movies_metadata$runtime)

# Get rid of rows where the budget is under 10,000 (most likely false measurements)
cleaned_movies_metadata <- cleaned_movies_metadata[which(cleaned_movies_metadata$budget > 10000),]

# Get rid of rows containing "[]" for genres and production_companies
cleaned_movies_metadata <- cleaned_movies_metadata[which(cleaned_movies_metadata$genres != "[]"),]

# Need this library to deal with the formatting of the genres field
library(jsonlite)

cleaned_movies_metadata$genres <- gsub("'", '"', cleaned_movies_metadata$genres)

cleaned_movies_metadata$genre <- "None"

# Iterate through each row/movie, select its first genre and make that the value of the new genre field
for ( i in 1:nrow(cleaned_movies_metadata) ){
  json <- toString(cleaned_movies_metadata$genres[i])
  mydf <- fromJSON(json)
  genre <- mydf[1,2]
  cleaned_movies_metadata$genre[i] = genre
}

# Get rid of unneccessary fields genres
cleaned_movies_metadata <- cleaned_movies_metadata[, c("revenue", "budget", "original_language", 
                                                       "year", "month", "runtime", "genre") ]


# Separate data into training and test set

## 50% - 50% split
smp_size <- floor(0.50 * nrow(cleaned_movies_metadata))

## set the seed to make the partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(cleaned_movies_metadata)), size = smp_size)

train <- cleaned_movies_metadata[train_ind, ]
test <- cleaned_movies_metadata[-train_ind, ]

# Use these to view/see and summary of the data
View(train)
View(test)
View(cleaned_movies_metadata)
str(cleaned_movies_metadata)
