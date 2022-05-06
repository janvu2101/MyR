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
df$chain_kfc <- as.integer(df$CHAIN == 2)
df$chain_roys <- as.integer(df$CHAIN == 3)
df$chain_wendys <- as.integer(df$CHAIN == 4)
tmp11 <- df %>% 
  group_by(df$STATE) %>%
  summarize(avg_chain_bk = mean(chain_bk))
tmp11

df$STATE[df$STATE == 1] <- "New Jersey"
df$STATE[df$STATE == 0] <- "Pennsylvania"
tmp1 <- aggregate(chain_bk ~ STATE, df, mean)
tmp2 <- aggregate(chain_kfc ~ STATE, df, mean)
tmp3 <- aggregate(chain_roys ~ STATE, df, mean)
tmp4 <- aggregate(chain_wendys ~ STATE, df, mean)
tmp1

mean_chain <- data.frame(bk = tmp1$chain_bk,
                         kfc = tmp2$chain_kfc,
                         roys = tmp3$chain_roys,
                         wendys = tmp4$chain_wendys)
mean_chain
rownames(mean_chain) <- tmp1$STATE
mean_chain
mean_chain <- as.matrix(mean_chain)

mean_chain <- t(mean_chain)
mean_chain <- tibble::as_tibble(mean_chain)
mean_chain

#TIDING UP THE DATASET
head(df, 10)
df$FTE1 <- df$EMPFT + 0.5*df$EMPPT + df$NMGRS
df$FTE2 <- df$EMPFT2 + 0.5*df$EMPPT2 + df$NMGRS2

df_order <- df %>% arrange(FTE1,-FTE2)
head(df_order[, c('FTE1','FTE2')], 10)

data.tidy <- df %>% 
  tidyr::gather(key = "year", value = "employees", FTE1:FTE2)
dim(data.tidy)
table(data.tidy$year)
