library(ggplot2)
library(dplyr)
library(lubridate)

View(movies_metadata) # View original dataset

# Select the fields we're interested in
cleaned_movies_metadata <- movies_metadata[, c("revenue", "budget", "genres", "original_language", "production_companies",
                                               "release_date", "runtime") ]

# Remove rows containing zeros
cleaned_movies_metadata <- cleaned_movies_metadata[!apply(cleaned_movies_metadata[,1:7] == 0, 1, FUN = any, na.rm = TRUE),]

# Convert original language to a factor field
cleaned_movies_metadata$original_language <- as.factor(cleaned_movies_metadata$original_language)

# Split release_date field into Year and Month fields, and convert the month field to a factor
cleaned_movies_metadata <- cleaned_movies_metadata %>%
  mutate(release_date = as.Date(release_date, format = "%y-%m-%d")) %>%
  mutate(month = month(release_date)) %>%
  mutate(year = year(release_date))
cleaned_movies_metadata$month <- as.factor(cleaned_movies_metadata$month)

# Get rid of unneccessary release_date field now
cleaned_movies_metadata <- cleaned_movies_metadata[, c("revenue", "budget", "genres", "original_language", "production_companies",
                                               "year", "month", "runtime") ]

# Convert original language to an integer field
cleaned_movies_metadata$runtime <- as.integer(cleaned_movies_metadata$runtime)

# Get rid of rows where the budget is under 10,000 (most likely false measurements)
cleaned_movies_metadata <- cleaned_movies_metadata[which(cleaned_movies_metadata$budget > 10000),]

# Get rid of rows containing "[]" for genres and production_companies
cleaned_movies_metadata <- cleaned_movies_metadata[which(cleaned_movies_metadata$genres != "[]"),]
cleaned_movies_metadata <- cleaned_movies_metadata[which(cleaned_movies_metadata$production_companies != "[]"),]


# Use these to view/see and summary of the data
View(cleaned_movies_metadata)
str(cleaned_movies_metadata)
