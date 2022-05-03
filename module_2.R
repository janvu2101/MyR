library(tidyr)
library(dplyr)
library(stringi)
library(stringr)
install.packages("rio")
library(rio)
df_main <- rio::import("data/card_krueger_public.dta")
View(df_main)
#TAKING THE FIRST LOOK
dim(df_main) #410 obs 46 vars
str(df_main)
sapply(df_main, class) #all variable are numberic
sapply(df_main, typeof)
var_name <- rio::import("data/variable_names.csv")
names(df_main) <- var_name$variable
head(df_main)
df <- df_main %>% select(SHEET, CHAIN, STATE, EMPFT, EMPPT, NMGRS, EMPFT2, EMPPT2, NMGRS2, STATUS2)
head(df, 5)
var_name
#SUMMARIZING THE DATA
table(is.na(df$EMPFT)) #no missing data in EMPFT
table(df$CHAIN, df$STATE) #the value of STATE and CHAIN is coded in number so it is hard to read
df$chain_bk <- as.integer(df$CHAIN == 1)

