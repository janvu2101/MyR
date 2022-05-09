#Start working with R
install.packages("tidyverse")
library(tidyverse)
setwd("~/Documents/R/Self-learn-R")

#Import data
## Use rio package: 
## https://cran.r-project.org/web/packages/rio/vignettes/rio.html
install.packages("rio")
library(rio)
practice_import <- rio::import("card_krueger_public.dta") 
practice_import
practice_import_tibble <- tibble::as_tibble(practice_import)
practice_import_tibble
library(readxl)

#Export data
library(dplyr)
practice_export <- select(practice_import, V1, V2, V3)
rio::export(practice_export, "practice_export.xlsx")

#Replace the names of the variables
practice_replace_name <- rio::import("card_krueger_public.dta")
var_name <- rio::import("variable_names.csv")
var_name$variable
names(practice_replace_name) <- var_name$variable
#names(practice_replace_name)[names(practice_replace_name) == "SHEET"] <- "SHEET_rename"
names(practice_replace_name)
colnames(practice_replace_name) #similar to the above

#Data type and data structure
## 5 basic types: integer, numeric, logical, character,complex
## 5 basic structures: vector, matrix, list, data frame, 
## Check data type
names(practice_replace_name)
class(practice_replace_name$SHEET)
typeof(practice_replace_name$SHEET)
sapply(practice_replace_name, class) #all variables horizontal format
lapply(practice_replace_name, class) #all variable vertical format
levels(x = experiment_simulation$takeup) #factor variable
nlevels(x = experiment_simulation$takeup) #number of levels of a factor variable

##Convert data type
set.seed(123)
convert_data_type <- data.frame(convert1 = sample(x = 1:3, size = 10, replace = TRUE, prob = c(0.25,0.25,0.5)),
                                convert2 = sample(x = 1:4, size = 10, replace = TRUE, prob = c(0.15,0.25,0.15,0.45)),
                                convert3 = sample(x = 1:2, size = 10, replace = TRUE, prob = c(0.3, 0.7))
                                )
names(convert_data_type)
View(convert_data_type)
str(convert_data_type)
convert_data_type$convert1 <- as.factor(convert_data_type$convert1)
convert_data_type[, c('convert2','convert3')] <- lapply(X = convert_data_type[, c('convert2','convert3')], 
                                                        function(x) as.factor(x))
str(convert_data_type) #why this works with lapply but not with sapply?
class(convert_data_type$convert2)

## Create a vector
create_vector_v1 <- c(1, 2L,"a")
create_vector_v2 <- c(4, 6L,"b")
create_vector_v3 <- c(8, 3L,"c")
create_vector_v4 <- c("d", 7)
create_vector_v5 <- c(1, 7)
create_vector_v6 <- c(3, 2)
## Create a list
create_list <- list(create_vector_v1,create_vector_v4)
create_list
## Create a data frame
create_dataframe <- data.frame(create_vector_v1,create_vector_v2)
create_dataframe
create_dataframe[2,2]
## Create a matrix (data must be the same type)
create_matrix <- matrix(1:4,nrow=2,ncol=3)
create_matrix

#Simulate data
## Create a data frame of control group and treatment group with 3 items, 175 observations
control <- data.frame(condition = 0,
                      item1 = sample(x = 1:6, size = 175, replace = TRUE, prob = c(0.15,0.16,0.19,0.22,0.17,0.11)),
                      item2 = sample(x = 1:6, size = 175, replace = TRUE, prob = c(0.17,0.12,0.18,0.24,0.15,0.14)),
                      item3 = sample(x = 1:6, size = 175, replace = TRUE, prob = c(0.08,0.15,0.22,0.27,0.18,0.13)),
                      takeup = 0)
View(control)
str(control)
compliers <- data.frame(condition = 1,
                   item1 = sample(x = 1:6, size = 150, replace = TRUE, prob = c(0.01,0.06,0.12,0.14,0.39,0.28)),
                   item2 = sample(x = 1:6, size = 150, replace = TRUE, prob = c(0.01,0.05,0.1,0.15,0.41,0.29)),
                   item3 = sample(x = 1:6, size = 150, replace = TRUE, prob = c(0.02,0.06,0.09,0.13,0.4,0.3)),
                   takeup = 1)
never_takers <- data.frame(condition = 1,
                           item1 = rnorm(n = 25, mean = 0, sd = 0),
                           item2 = rnorm(n = 25, mean = 0, sd = 0),
                           item3 = rnorm(n = 25, mean = 0, sd = 0),
                           takeup = 0)

## Convert data type of item1,2,3 to factor
#using loops
for (i in names(control)) {
  control[[i]] <- as.numeric(control[[i]])
}
control$condition <- as.factor(control$condition)
control$takeup <- as.factor(control$takeup)
class(control$condition)
typeof(control$condition)

for (i in names(compliers)) {
  compliers[[i]] <- as.numeric(compliers[[i]])
}
compliers$condition <- as.factor(compliers$condition)
compliers$takeup <- as.factor(compliers$takeup)

for (i in names(never_takers)) {
  never_takers[[i]] <- as.numeric(never_takers[[i]])
}
never_takers$condition <- as.factor(never_takers$condition)
never_takers$takeup <- as.factor(never_takers$takeup)

## Create the mean of all item and store all in the column named "mean"
control <- mutate(control, response = round((item1+item2+item3)/3, 1))
control <- control %>%
  mutate(response = round((item1+item2+item3)/3, 1)) #alternative to the above
control

compliers <- compliers %>%
  mutate(response = round((item1+item2+item3)/3, 1))
never_takers <- never_takers %>%
  mutate(response = round((item1+item2+item3)/3, 1))

##Combine control, compliers and never_takers into 1 dataframe
experiment_simulation <- rbind(control,compliers,never_takers)
experiment_simulation <- experiment_simulation %>%
  select(-item1:-item3)
names(experiment_simulation)
head(experiment_simulation, 7)
str(experiment_simulation)
experiment_simulation$condition <- as.numeric(experiment_simulation$condition)
experiment_simulation$condition[experiment_simulation$condition == 0] <- "control group"
experiment_simulation$condition[experiment_simulation$condition == 1] <- "treatment group"
experiment_simulation$condition <- as.factor(experiment_simulation$condition)

#Basic queries
## Column index
match(c('response'), names(experiment_simulation))
match(c('takeup','response'), names(experiment_simulation))

#Select, subset data
## https://dplyr.tidyverse.org/reference/select.html

set.seed(123)
practice_subset <- data.frame(subset1 = sample(x = 1:3, size = 10, replace = TRUE, prob = c(0.25,0.25,0.5)),
                              subset2 = sample(x = 1:4, size = 10, replace = TRUE, prob = c(0.15,0.25,0.15,0.45)),
                              subset3 = sample(x = 1:2, size = 10, replace = TRUE, prob = c(0.3, 0.7))
)
names(practice_subset)
subset <- practice_subset[sample(nrow(practice_subset), 3), ]
head(subset, 10)

#Gather vs Spread data
## We need tidyverse (tidyr) to perform these operations
earnings_panel <- data.frame(person = c("Elsa", "Mickey","Ariel","Gaston","Jasmine","Peter","Alice"),
                             y1999 = c(10,20,17,19,32,22,11),
                             y2000 = c(15,28,21,19,35,29,15)
                             )
names(earnings_panel)
library(dplyr)
earnings_panel_gather <- earnings_panel %>% #function %>% require dplyr
  tidyr::gather(key = "year", value = "wage", y1999:y2000)
View(earnings_panel_gather)
earnings_panel_spread <- earnings_panel_gather%>%
  tidyr::spread(key = "year", value = "wage")
View(earnings_panel_spread)

#Cleaning data: filter(), mutate(), summarize(), arrange(), group, recode variables
df_cleaning <- data.frame(person = c("Elsa", "Mickey","Ariel","Gaston","Jasmine","Peter","Alice"),
                          y1999 = c(10,20,17,19,32,22,10),
                          y2000 = c(15,28,21,19,35,29,16)
)
##filter()
df_cleaning[df_cleaning$y1999 > 20, ]  #basic R
df_cleaning[df_cleaning$y1999 > 20, ]
df_cleaning %>% filter(y1999 > 20) #dplyr
df 
##mutate()
df_cleaning$sum1 <- df_cleaning$y1999+df_cleaning$y2000 #basic R
names(df_cleaning)
View(df_cleaning)
df_cleaning$sum <- NULL

df_cleaning <- df_cleaning %>%
  mutate(df_cleaning$y1999+df_cleaning$y2000) #dplyr

##summarize()
summarize_Rbasic <- data.frame(avg_y1999 = mean(df_cleaning$y1999),
                                  median_y1999 = median(df_cleaning$y1999)) #basic R
summarize_Rbasic

summarize_dplyr <- df_cleaning %>%
  summarize(avg_y1999 = mean(y1999, na.rm = TRUE),
            median_y1999 = median(y1999, na.rm = TRUE),
            sd_y1999 = sd(y1999, na.rm = TRUE),
            cor_y1999_2000 = cor(y1999,y2000, use = "everything", method = "pearson"),
            quantile_y1999 = quantile(y1999, na.rm = TRUE),
            IQR_y1999 = IQR(y1999, na.rm = TRUE)
            )
summarize_dplyr
nrow(df_cleaning)
length <- length(df_cleaning$y1999) #alternative to above
length_unique <- length(unique(df_cleaning$y1999))
length_unique
n_distinct <- dplyr::n_distinct(df_cleaning$y1999)
n_distinct

table(df_cleaning$y1999) #get freq of unique values
table_disctinct <- as.data.frame(table(df_cleaning$y1999))
rio::export(table_disctinct, "table_distinct.xlsx")

##arrange()
View(df_cleaning)
df_cleaning[order(df_cleaning[, "y1999"], -df_cleaning[, "y2000"]), ] #basic R
df_cleaning %>% dplyr::arrange(y1999, -y2000)

##group data (aggregate vs group_by)
install.packages("stringi")
library(stringi)
set.seed(123)
df_grouping <- data.frame(id = c(1:10),
                          name = stri_rand_strings(n=10, len=5, pattern='[A-Za-z0-9]'),
                          gender = sample(c('female','male'), size = 10, replace = TRUE, prob = c(0.5,0.5)),
                          grade = round(rnorm(10, 7.0, 1.2), 1))
View(df_grouping)
str(df_grouping)
aggregate(grade ~ gender, df_grouping, mean) #wrong --> why?
library(dplyr)
df_grouping %>% dplyr::group_by(gender) %>% summarize(avg.grade = mean(grade))

##recode variable

library(stringi)
set.seed(123)
df_recode <- data.frame(id = c(1:10),
                        name = stri_rand_strings(n=10, len=5,pattern='[A-Za-z0-9]'),
                        gender = sample(c('female','male'), size = 10, replace = TRUE, prob = c(0.5,0.5)),
                        grade = round(rnorm(10, 7.0, 1.2), 1))
str(df_recode)
View(df_recode) #6 males and 4 females
df_recode$is_female <- as.integer(df_recode$gender == "female") #R basic
library(dplyr)
df_recode$is_female <- df_recode$gender %>% recode("male" = 0,
                                                   "female" = 1) #tidyverse
View(df_recode)

#Identify missing values
library(stringi)
set.seed(123)
df_missing <- data.frame(id = c(1:10),
                         name = stri_rand_strings(n = 10, len = 5, pattern = '[A-Za-z0-9]'),
                         gender = sample(c('female','male'), size = 10, replace = TRUE, prob = c(0.5, 0.5)),
                         grade = round(rnorm(10, 7.0, 1.2), 1))
View(df_missing)
df_missing <- df_missing %>%
  mutate(drop = "is.na<-"(id, id = 2) #this does not work????
  )

df_missing <- df_missing %>%
  mutate(retake = "is.na<-"(grade, grade > 6 & grade <7),
         drop = "is.na<-"(id, id < 2)
         )
names(df_missing)

## Identify missing values
is.na(df_missing)
table(is.na(df_missing))#count number of missing value
is.na(df_missing$retake)
table(is.na(df_missing$retake)) #count number of missing value

## Remove missing value
remove_missing_all <- df_missing[complete.cases(df_missing),] #remove all row with missing data
remove_missing_all
remove_missing_partial <- df_missing[!is.na(df_missing$retake),] #remove rows with missing value in column retake
remove_missing_partial

## Recode missing value
#df_missing_recode <- df_missing
#df_missing_recode[df_missing_recode$drop == 10, ] <- NA
#df_missing_recode$recode <- df_missing_recode$drop %>% na_if(99)
#df_missing_recode

# LOOPS AND CONDITION
library(stringi)
set.seed(123)
loops_df <- data.frame(id = c(1:20),
                     name = stri_rand_strings(n = 20, length = 7, pattern = "[A-Za-z0-9]"),
                     class = sample(x=c("A", "B", "C"), size = 20, replace = TRUE, prob = c(0.3,0.3,0.4)),
                     gender = sample(x = c("female","male"), size = 20, replace = TRUE, prob = c(0.5,0.5)),
                     district = sample(x = c(3:8), size = 20, replace = TRUE, prob = c(0.2,0.1,0.25,0.15,0.2,0.1)),
                     campus = sample(x = c("Yes","No"), size = 20, replace = TRUE, prob = c(0.3,0.7)),
                     transport = sample(x = c("shuttle bus", "bicycle", "car"), size = 20, replace = TRUE, prob = c(0.55,0.2,0.25)),
                     math = round(rnorm(n = 20, mean = 7.2, sd = 0.6), 1),
                     english = round(rnorm(n = 20, mean = 8.1, sd = 0.5), 1)
)
View(loops_df)

## for loops
### loops across named elements
str(loops_df)
for (i in c("name", "class")){
  loops_df[, i] <- as.character(loops_df[, i])
}
class(loops_df$name)
for (i in c("name","class")){
  loops_df[, i] <- as.factor(loops_df[, i])
}
levels(loops_df$class)
nlevels(loops_df$class)
### for loop and vector indexes
for (i in 1:length(loops_df$math)){
  loops_df$math[i] <- loops_df$math[i] - 0.1
}
loops_df$math
## while statements
loops_df[, 'classification'] <- "not specified"
View(loops_df)

i <- length(loops_df$math)
while (loops_df$math[i] > 8.0) {
  loops_df$classification[i] <- "excellent"
  #i <- i - 1
}

loops_df$classification <- NULL

## if statement
loops_df[, 'classification'] <- "not specified"
for (i in 1:length(loops_df$math)){
  if (loops_df$math[i] > 8.0) {
    loops_df$classification[i] <- "excellent"
  }
}
View(loops_df)
loops_df$classification <- NULL
loops_df[,'final_math'] <- loops_df$math
for (i in 1:length(loops_df$math)) {
  if (loops_df$math[i] < 6) {
    loops_df$final_math[i] <- loops_df$final_math[i] + 0.1
  }
}
loops_df$final_math <- NULL

### multiple condition: if, else if, else
loops_df$classification <- NA
for (i in 1:length(loops_df$math)) {
  if (loops_df$math[i] >= 8.0){
    loops_df$classification[i] <- "excellent"
  } else if (loops_df$math[i] >= 7.0){
    loops_df$classification[i] <- "good"
  } else if (loops_df$math[i] >= 6.0) {
    loops_df$classification[i] <- "fair"
  } else {
    loops_df$classification[i] <- "pass"
  }
}
View(loops_df)
loops_df$classification <- NULL

## vectorized if statements
loops_df$classification <- loops_df$math
loops_df$classification = ifelse (loops_df$classification >= 6.0, 
                                  "PASS", 
                                  "RETAKE")
View(loops_df)
loops_df$classification <- NULL

### Multiple vectorized if statements
library(dplyr)
loops_df$classification <- loops_df$math
loops_df$classification <- dplyr::case_when(
  loops_df$classification >= 8.0 ~ "excellent",
  loops_df$classification >= 7.0 & loops_df$classification < 8.0 ~ "good",
  loops_df$classification >= 6.0 & loops_df$classification < 7.0 ~"fair",
  loops_df$classification < 6.0 ~ "retake"
)
View(loops_df)
loops_df$classification <- NULL

#FUNCTION
set.seed(123)
function_df <- data.frame(id = c(1:20),
                          A = sample(1:2, size = 20, replace = TRUE, prob = c(0.3,0.7)),
                          B = sample(4:5, size = 20, replace = TRUE, prob = c(0.6, 0.4))
                          )
View(function_df)

#function_df[, c('A','B')] <- lapply(X = function_df[, c('A','B')], function(x) as.factor(x))

odd <- function(x) {
  output <- ifelse(x %% 2 == 0, "even", "odd")
  return(output)
}
function_df$odd_result_A <- odd(function_df$A)
function_df$odd_result_B <- odd(function_df$B)
View(function_df)
names(function_df)
function_df$odd_result_A <- NULL
function_df$odd_result_B <- NULL

#MERGING DATA

