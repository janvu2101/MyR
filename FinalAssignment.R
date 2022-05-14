install.packages("knitr")
install.packages("ggthemes")
install.packages("stargazer")
install.packages("tidyverse")
install.packages("rio")
install.packages(
  "ggplot2",
  dependencies = TRUE,
  repos = c("http://rstudio.org/_packages",
            "http://cran.rstudio.com")
)
library(tidyverse)
library(dplyr)
library(tidyr)
library(rio)
library(knitr)
library(ggplot2)
library(ggthemes)
R.Version()
#EXERCISE 1: REPLICATE FIGURE 4
##Load dataset
data_original <- rio::import("data/pub_pvt_scatters.dta")
uniid <- rio::import("data/univ_names.xlsx")

##Merge df with uniid name
df <- merge(data_original, uniid, by="unitid", all = FALSE) #keep only obs that belong to the common university ID between 2 data frames
nrow(data_original) == nrow(df) #some obs were removed
df <- relocate(df, name, .after = unitid)

##Create common deflator cpi_all which is just the mean of cpi across all universities in the same year
df <- df %>%
  group_by(year) %>%
  mutate(cpi_all = mean(cpi, na.rm = TRUE))

##Create the real value of the appropriation by dividing nominal_approp by cpi_all
df$real_approp <- df$nominal_approp / df$cpi_all #get warning message: Unknown or uninitialised column: `merge`.

## Use the Private variable to create a categorical variable that differentiate public and private university
df$Private <- as.factor(df$Private)
class(df$Private)

## Reorder data by unit and by year
df <- df %>% arrange(unitid, year)

## Create the log of the variable real_approp and ENROLL_FRESH_NON_RES_ALIEN_DEG
df <- df %>%
  mutate(log_real_approp = log(real_approp))
df <- df %>%
  mutate(log_foreign_freshmen = log(ENROLL_FRESH_NON_RES_ALIEN_DEG))

## Create the difference in log values by university between 2005 and 2012 for these two variables and save it in a new data.frame
tmp1 <- df %>% 
  select(name, year, log_real_approp) %>%
  filter(year == 2005 | year == 2012)
tmp2 <- df %>%
  select(name, year, log_foreign_freshmen) %>%
  filter(year == 2005 | year == 2012)
nrow(tmp1)
nrow(tmp2)
nrow(tmp1) == nrow(tmp2)

sum(is.na(tmp1$log_real_approp))
sum(is.na(tmp2$log_foreign_freshmen))

tmp1 <- tmp1 %>%
  tidyr::spread(key = "year", value = "log_real_approp")
tmp1 <- as.data.frame(tmp1)
tmp1$change_logreal <- tmp1$`2012` - tmp1$`2005`
tmp1 <- tmp1 %>%
  select(name, change_logreal)
head(tmp1, 10)
tmp2 <- tmp2 %>%
  tidyr::spread(key = "year", value = "log_foreign_freshmen")
tmp2 <- as.data.frame(tmp2)
tmp2$change_logforeign <- tmp2$`2012`-tmp2$`2005`
tmp2 <- tmp2 %>%
  select(name, change_logforeign)
head(tmp2,10)
tmp3 <- df %>%
  as.data.frame() %>%
  select(name, Private) %>%
  distinct()
head(tmp3, 100)

df_fig4 <- merge(x = tmp1, y = tmp2, by = "name")
df_fig4 <- merge(x = df_fig4, y = tmp3, by = "name")
df_fig4$Private <- as.integer(df_fig4$Private)
df_fig4$Private[df_fig4$Private == 1] <- "Public"
df_fig4$Private[df_fig4$Private == 2] <- "Private"


## Replicate Figure 4 using ggplot, including all the features such as fitted lines, labels and colors. If you want to make it prettier, feel free to be creative
fig4 <- ggplot(data = df_fig4, aes(x  = change_logreal, y = change_logforeign, color = Private, shape = Private)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("darkred","#0567B9")) +
  scale_shape_manual(values = c(0,16)) +
  geom_text(label = df_fig4$name, size = 2, vjust = 0, nudge_y = 0.1, show.legend = FALSE) +
  geom_smooth(method = "lm", formula = "y ~ x", se = FALSE, show.legend = FALSE, size=0.5) +
  ylim (-0.4,3) +
  labs(x = "Change in log(total appropriations in state)",
       y = "Change in log(foreign freshmen)")
fig4 + 
  theme_classic(base_size = 14) +
  ggtitle("AAU (2005-2012)") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +
  theme(axis.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 8), legend.title = element_blank(), legend.position = c(0.89,0.9), legend.box.background = element_rect(fill = "white", color = "black"))
install.packages("labeling")
library(labeling)
install.packages("farver")
library(farver)

#EXERCISE 2: REPLICATE TABLE 2
## Load dataset 
unidata <- rio::import("data/univ_data.dta")

## Create variable for total state appropriation
unidata$total_state_ap <- unidata$nominal_approp - unidata$state_ap*100000
unidata$l_total_state_ap <- log(unidata$total_state_ap)
## Find balanced sample

balanced <- unidata %>% 
  drop_na(ENROLL_FRESH_NON_RES_ALIEN_DEG, AMERICAN_OOS, IN_STATE_FRESHMEN) %>%
  select(unitid, year, AAU, Research, nonResearch, l_ENROLL_FRESH_NON_RES_ALIEN_DEG, l_state_ap, l_population, l_total_state_ap, weight, ENROLL_FRESH_NON_RES_ALIEN_DEG, AMERICAN_OOS, IN_STATE_FRESHMEN)

apply(is.na(balanced[, c("ENROLL_FRESH_NON_RES_ALIEN_DEG","AMERICAN_OOS","IN_STATE_FRESHMEN")]), 2, sum) #sum return the count, which returns the position
dim(balanced)

## Run the regression
install.packages("lfe")
library(lfe)
library(stargazer)

# 3 data frames for 3 groups: Research, nonResearch, and AAU
research <- balanced %>% filter(Research == 1)
nonResearch <- balanced %>% filter(nonResearch == 1)
AAU <- balanced %>% filter(AAU == 1)
nrow(research) #different from the article
nrow(nonResearch) #different from the article
nrow(AAU) #different from the article


# 1st stage regression
research_1st <- felm(l_state_ap ~ l_total_state_ap | unitid | 0| unitid, data = research)
summary(research_1st)

nonResearch_1st <- felm(l_state_ap ~ l_total_state_ap | unitid | 0| 0, data = nonResearch)
summary(nonResearch_1st)

AAU_1st <- felm(l_state_ap ~ l_total_state_ap | unitid | 0 | 0, data = AAU)
summary(AAU_1st)

# 2nd stage regression with IV 

research_ols <- felm(l_ENROLL_FRESH_NON_RES_ALIEN_DEG ~ l_state_ap + l_population | unitid | 0 | unitid, data = research)
summary(research_ols, robust = TRUE)
research_IV <- felm(l_ENROLL_FRESH_NON_RES_ALIEN_DEG ~ l_state_ap + l_population | year | l_total_state_ap, weights = research$weight, data = research)
summary(research_IV)


# END #














View(balanced)
# pacman: https://www.linode.com/docs/guides/pacman-package-manager/
rio::export(balanced, "balanced.csv")
library(AER)

View(balanced)
names(balanced)
names(unidata)
names(df)
#SELF-SEARCHED:
a <- c(3, 5, 12, 9, 4, 8)
b <- c(2, 10, 6, 1, 7, 4)
year <- c("y1","y1", "y2", "y2", "y2", "y2")
ab <- data.frame(year,a,b)
ab$year <- as.character(ab$year)
ab$year
ab %>% group_by(year) %>%
  mutate(g = a - lag(a))

g <- diff(ab$a)
print(g)
for (i in 1:length(ab$year)) {
  if(ab$year[i+1] = ab$year[i])
}
ab <- ab %>%
  group_by(year) %>%
  
for (i in 1:length(a)) {
  if (i == 1) {
    ab$f[i] <- 0
  } else {
    ab$f[i] <- ab$a[i] - ab$a[i-1]  
  }
}
print(ab)
str(ab)




merge(df,tmp, by="year") 
View(df)
str(df)
View(df)
View(uniid)
View(unidata)
