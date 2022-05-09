#SCRAPPING DATA FROM A WEBSITE
install.packages("rvest")
library(rvest)
url <- "https://www.beeradvocate.com/beer/top-rated/"
data <- rvest::html_table(rvest::read_html(url), header = TRUE)
str(data)
df <- data[[1]]
View(df)
df$You <- NULL
str(df)
library(tidyverse)
library(tidyr)
library(dplyr)
library(stringr)
df$Ratings <- stringr::str_replace(df$Ratings, ",", "") #alternative is to use gsub()
class(df$Ratings)
df$Ratings <- as.integer(df$Ratings)
names(df) <- c("rank", "name", "n_ratings", "avg_rating")

#STRING MANIPULATION
df_tmp <- df[grepl(pattern = "%", x = df$name, fixed = TRUE), ]
View(df_tmp)
nrow(df) == nrow(df_tmp) #nrow is different --> there is names which don't include the % (alcohol level)

df$alcohol <- df$name
df$alcohol <- gsub(pattern = ".*\\|([^%]+).*", replacement = "\\1", x = df$alcohol)
##there are 2 obs that don't contain level of alcohol --> the data type is not consistent in the column "alcohol"
## alternative: strsplit(), separate()
##gsub() don't return a list so we don't need to unlist
## we use strsplit()
tmp <- strsplit(x = df$name, split = "\\|")
View(tmp)
for (i in 1:length(tmp)) {
  if (length(tmp[[i]]) == 1) {
    tmp[[i]] <- c(tmp[[i]], NA)
  } else if (length(tmp[[i]]) > 2) {
    print(paste("TOO LONG:", i))
  }
}
tmp <- unlist(tmp)
n <- length(tmp)
tmp <- matrix(data = tmp, nrow = n/2, ncol = 2, byrow = TRUE)
View(tmp)

#MORE STRING MANIPULATIONS
tmp <- as.data.frame(tmp)
str(tmp)
names(tmp) <- c("Name","Percentage")

tmp$Percentage <- stringr::str_trim(tmp$Percentage)

tmp$Percentage <- gsub("%", "", tmp$Percentage, fixed = TRUE)
tmp$Percentage <- as.numeric(tmp$Percentage)
class(tmp$Percentage)
head(df, 5)

sum(is.na(tmp$Percentage))
names(df)[names(df) == "name"] <- "name_old"
names(df)
df <- cbind(df, tmp)
df <- df[, !names(df) %in% c("name_old", "alcohol")]
df <- relocate(df, Name, .after = rank)

head(df, 6)

#WRITING A FUNCTION THAT CAN BE RE-USED

perc_to_num <- function(x) {
  x <- as.character(x)
  stopifnot(is.character(x))
  
  x <- stringr::str_trim(x, side = "left")
  x <- gsub(pattern = "%", replacement = "", x, fixed = TRUE)
  x <- as.numeric(x)
  
  return(x)
}
exp <- data.frame(name = c("Class 1", "Class 2", "Class 3"),
                  level = c(" 45.5%", " 72.2%", "90%"))
exp$level <- perc_to_num(exp$level)
class(exp$level)
View(exp)

#AUTOMATE THIS PROCESS
country_codes <- c("de","se","gb")
for (i in 1:length(country_codes)) {
  url <- paste0("https://www.beeradvocate.com/beer/top-rated/", country_codes[i], "/")
  df_auto <- rvest::html_table(rvest::read_html(url), header = TRUE)
  df_auto <- df_auto[[1]]
  names(df_auto) <- c("Rank", "Name", "Ratings", "Avg", "Yours")
  if (is.character(df_auto$Ratings)) {
    df_auto$Ratings <- gsub(",", "", df_auto$Ratings, fixed = TRUE)
    df_auto$Ratings <- as.integer(df_auto$Ratings)
  }
  df_auto$Country <- country_codes[i]
  if (i == 1) {
    df_combined <- df_auto
  } else {
    df_combined <- rbind(df_combined, df_auto)
  }
}

head(df_combined, 10)

df_combined$Yours <- NULL
df_combined <- df_combined[grepl(pattern = "%", x = df_combined$Name, fixed = TRUE),]
dim(df_combined)
View(df_combined)

df_combined$Percentage <- df_combined$Name
df_combined$Percentage <- gsub(pattern = ".*\\|([^%]+).*", replacement = "\\1", df_combined$Percentage)
df_combined$Percentage <- as.numeric(df_combined$Percentage)
class(df_combined$Percentage)
df_combined$Name <- gsub("\\|.*","", df_combined$Name)




