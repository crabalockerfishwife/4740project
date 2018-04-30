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

cleaned_movies_metadata$genre <- as.factor(cleaned_movies_metadata$genre)

# Get rid of unneccessary fields genres
cleaned_movies_metadata <- cleaned_movies_metadata[, c("revenue", "budget", "original_language", 
                                                       "year", "month", "runtime", "genre") ]

# Dealing with factor fields --> need to create dummy fields

# Convert the silent films (original_language =="xx") to original_language of "fr"
for ( i in 1:nrow(cleaned_movies_metadata) ){
  if (cleaned_movies_metadata$original_language[i] == "xx") {
    cleaned_movies_metadata$original_language[i] = "fr"
  }
}

# Initialize new factor fields for language
# language1 will be "1" if the original_language is english, "0" otherwise
# language2 will be "1" if the original_language is chinese, japanese, french, hindi, german, italian or spanish, "0" o/w
# language1 will be "1" if the original_language is any other language, "0" if it fits into category 1 or 2
cleaned_movies_metadata$language1 <- 0
cleaned_movies_metadata$language2 <- 0
cleaned_movies_metadata$language3 <- 0

# Assign each row the correct "1"
for ( i in 1:nrow(cleaned_movies_metadata) ){
  if (cleaned_movies_metadata$original_language[i] == "en") {
    cleaned_movies_metadata$language1[i] = 1
  } else if (cleaned_movies_metadata$original_language[i] == "cn" || cleaned_movies_metadata$original_language[i] == "zh"
             || cleaned_movies_metadata$original_language[i] == "ja" || cleaned_movies_metadata$original_language[i] == "fr"
             || cleaned_movies_metadata$original_language[i] == "hi" || cleaned_movies_metadata$original_language[i] == "de"
             || cleaned_movies_metadata$original_language[i] == "it" || cleaned_movies_metadata$original_language[i] == "es") {
    cleaned_movies_metadata$language2[i] = 1
  } else {
    cleaned_movies_metadata$language3[i] = 1
  }
}

cleaned_movies_metadata$language1 <- as.factor(cleaned_movies_metadata$language1)
cleaned_movies_metadata$language2 <- as.factor(cleaned_movies_metadata$language2)
cleaned_movies_metadata$language3 <- as.factor(cleaned_movies_metadata$language3)

cleaned_movies_metadata$genre <- gsub(" ", "_", cleaned_movies_metadata$genre)

# Initialize new factor fields for genre
cleaned_movies_metadata$Action <- 0
cleaned_movies_metadata$Adventure <- 0
cleaned_movies_metadata$Animation <- 0
cleaned_movies_metadata$Comedy <- 0
cleaned_movies_metadata$Crime <- 0
cleaned_movies_metadata$Documentary <- 0
cleaned_movies_metadata$Drama <- 0
cleaned_movies_metadata$Family <- 0
cleaned_movies_metadata$Fantasy <- 0
cleaned_movies_metadata$Foreign <- 0
cleaned_movies_metadata$History <- 0
cleaned_movies_metadata$Horror <- 0
cleaned_movies_metadata$Music <- 0
cleaned_movies_metadata$Mystery <- 0
cleaned_movies_metadata$Romance <- 0
cleaned_movies_metadata$Science_Fiction <- 0
cleaned_movies_metadata$Thriller <- 0
cleaned_movies_metadata$TV_Movie <- 0
cleaned_movies_metadata$War <- 0
cleaned_movies_metadata$Western <- 0

# Assign each row the correct "1"
for ( i in 1:nrow(cleaned_movies_metadata) ){
  if (cleaned_movies_metadata$genre[i] == "Action") {
    cleaned_movies_metadata$Action[i] = 1
  } else if (cleaned_movies_metadata$genre[i] == "Adventure") {
    cleaned_movies_metadata$Adventure[i] = 1
  } else if (cleaned_movies_metadata$genre[i] == "Animation") {
    cleaned_movies_metadata$Animation[i] = 1
  } else if (cleaned_movies_metadata$genre[i] == "Comedy") {
    cleaned_movies_metadata$Comedy[i] = 1
  } else if (cleaned_movies_metadata$genre[i] == "Crime") {
    cleaned_movies_metadata$Crime[i] = 1
  } else if (cleaned_movies_metadata$genre[i] == "Documentary") {
    cleaned_movies_metadata$Documentary[i] = 1
  } else if (cleaned_movies_metadata$genre[i] == "Drama") {
    cleaned_movies_metadata$Drama[i] = 1
  } else if (cleaned_movies_metadata$genre[i] == "Family") {
    cleaned_movies_metadata$Family[i] = 1
  } else if (cleaned_movies_metadata$genre[i] == "Fantasy") {
    cleaned_movies_metadata$Fantasy[i] = 1
  } else if (cleaned_movies_metadata$genre[i] == "Foreign") {
    cleaned_movies_metadata$Foreign[i] = 1
  } else if (cleaned_movies_metadata$genre[i] == " History") {
    cleaned_movies_metadata$History[i] = 1
  } else if (cleaned_movies_metadata$genre[i] == "Horror") {
    cleaned_movies_metadata$Horror[i] = 1
  } else if (cleaned_movies_metadata$genre[i] == "Music") {
    cleaned_movies_metadata$Music[i] = 1
  } else if (cleaned_movies_metadata$genre[i] == "Mystery") {
    cleaned_movies_metadata$Mystery[i] = 1
  } else if (cleaned_movies_metadata$genre[i] == "Romance") {
    cleaned_movies_metadata$Romance[i] = 1
  } else if (cleaned_movies_metadata$genre[i] == "Science_Fiction") {
    cleaned_movies_metadata$Science_Fiction[i] = 1
  } else if (cleaned_movies_metadata$genre[i] == "Thriller") {
    cleaned_movies_metadata$Thriller[i] = 1
  } else if (cleaned_movies_metadata$genre[i] == "TV_Movie") {
    cleaned_movies_metadata$TV_Movie[i] = 1
  } else if (cleaned_movies_metadata$genre[i] == "War") {
    cleaned_movies_metadata$War[i] = 1
  } else if (cleaned_movies_metadata$genre[i] == "Western") {
    cleaned_movies_metadata$Western[i] = 1
  }
}

# Convert these fields to factors
cleaned_movies_metadata$Action <- as.factor(cleaned_movies_metadata$Action)
cleaned_movies_metadata$Adventure <- as.factor(cleaned_movies_metadata$Adventure)
cleaned_movies_metadata$Animation <- as.factor(cleaned_movies_metadata$Animation)
cleaned_movies_metadata$Comedy <- as.factor(cleaned_movies_metadata$Comedy)
cleaned_movies_metadata$Crime <- as.factor(cleaned_movies_metadata$Crime)
cleaned_movies_metadata$Documentary <- as.factor(cleaned_movies_metadata$Documentary)
cleaned_movies_metadata$Drama <- as.factor(cleaned_movies_metadata$Drama)
cleaned_movies_metadata$Family <- as.factor(cleaned_movies_metadata$Family)
cleaned_movies_metadata$Fantasy <- as.factor(cleaned_movies_metadata$Fantasy)
cleaned_movies_metadata$Foreign <- as.factor(cleaned_movies_metadata$Foreign)
cleaned_movies_metadata$History <- as.factor(cleaned_movies_metadata$History)
cleaned_movies_metadata$Horror <- as.factor(cleaned_movies_metadata$Horror)
cleaned_movies_metadata$Music <- as.factor(cleaned_movies_metadata$Music)
cleaned_movies_metadata$Mystery <- as.factor(cleaned_movies_metadata$Mystery)
cleaned_movies_metadata$Romance <- as.factor(cleaned_movies_metadata$Romance)
cleaned_movies_metadata$Science_Fiction <- as.factor(cleaned_movies_metadata$Science_Fiction)
cleaned_movies_metadata$Thriller <- as.factor(cleaned_movies_metadata$Thriller)
cleaned_movies_metadata$TV_Movie <- as.factor(cleaned_movies_metadata$TV_Movie)
cleaned_movies_metadata$War <- as.factor(cleaned_movies_metadata$War)
cleaned_movies_metadata$Western <- as.factor(cleaned_movies_metadata$Western)

# Initialize new factor fields for season
cleaned_movies_metadata$winter <- 0
cleaned_movies_metadata$spring <- 0
cleaned_movies_metadata$summer <- 0
cleaned_movies_metadata$fall <- 0

for ( i in 1:nrow(cleaned_movies_metadata) ){
  if (cleaned_movies_metadata$month[i] == 12 || cleaned_movies_metadata$month[i] == 1 
      || cleaned_movies_metadata$month[i] == 2) {
    cleaned_movies_metadata$winter[i] = 1
  } else if (cleaned_movies_metadata$month[i] == 3 || cleaned_movies_metadata$month[i] == 4 
             || cleaned_movies_metadata$month[i] == 5) {
    cleaned_movies_metadata$spring[i] = 1
  }else if (cleaned_movies_metadata$month[i] == 6 || cleaned_movies_metadata$month[i] == 7 
            || cleaned_movies_metadata$month[i] == 8) {
    cleaned_movies_metadata$summer[i] = 1
  }else if (cleaned_movies_metadata$month[i] == 9 || cleaned_movies_metadata$month[i] == 10 
            || cleaned_movies_metadata$month[i] == 11) {
    cleaned_movies_metadata$fall[i] = 1
  }
}

cleaned_movies_metadata$winter <- as.factor(cleaned_movies_metadata$winter)
cleaned_movies_metadata$spring <- as.factor(cleaned_movies_metadata$spring)
cleaned_movies_metadata$summer <- as.factor(cleaned_movies_metadata$summer)
cleaned_movies_metadata$fall <- as.factor(cleaned_movies_metadata$fall)

# Select the fields we're interested in
cleaned_movies_metadata <- cleaned_movies_metadata[, c("revenue", "budget", "year", "runtime", 
      "language1", "language2", "language3", "Action", "Adventure", "Animation", "Comedy", "Crime", "Documentary",
      "Drama", "Family", "Fantasy", "Foreign", "History", "Horror", "Music", "Mystery", "Romance", "Science_Fiction",
      "Thriller", "TV_Movie", "War", "Western", "winter", "spring", "summer", "fall") ]


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
