

install.packages("tidyverse")
install.packages("ggthemes")
install.packages("forcats")
library(tidyverse)
library(ggthemes)
library(forcats)
library(randomForest)
library(caret)


main_data <- epldata_final

## Formatting variable type

main_data <- main_data %>% mutate(position_cat= as.factor(position_cat),
  region=as.factor(region),
  club_id=as.numeric(as.factor(club)),
  age_cat = as.factor(age_cat),
  big_club=ifelse(club_id %in% c(1,5,10,11,12,17),1,0)
)

ageCat <- function(age){
  if (age < 22)return(1)
  else if( age < 25)return(2)
  else if( age < 28)return(3)
  else if( age < 30)return(4)
  else if( age < 32)return(5)
  else return(6)
}


# creating levels for age variable
main_data <- main_data %>%rowwise() %>% mutate(age_cat=ageCat(age))
# converting to factor
main_data$age_cat<-as.factor(main_data$age_cat)

# creating age category variable
main_data <- transform(main_data,age_category=cut(age,breaks=c(16,21,25,28,31,38),labels = c("17-21","22-25","26-28","29-31","32-38")))

# training data set
train_data <- filter(main_data,!club_id %in% c(3,8,13)) %>%  filter(new_foreign == 0)


## Introduction

## This dataset has been scraped from three sources transfermrkt.com, Wikipedia and 
## Fantasy Premier League. It contains all the players listed on the FPL site for each team, 
## who have a corresponding market value.


## Initial Analysis

### Most valuable players in the EPL

tp <- main_data %>% arrange(desc(market_value))
head(tp,n=6)[,c(1,6)]
# name market_value
# 1   Wayne Rooney           15
# 2     Paul Pogba           75
# 3      Dele Alli           45
# 4    Diego Costa           50
# 5     Mesut Ozil           50
# 6 Alexis Sanchez           65


### Most popular players

tp <- main_data %>% arrange(desc(page_views))
head(tp,n=6)[,c(1,7)]
# name page_views
# 1   Wayne Rooney       7664
# 2     Paul Pogba       7435
# 3      Dele Alli       4626
# 4    Diego Costa       4454
# 5     Mesut Ozil       4395
# 6 Alexis Sanchez       4329

### Market Value Distribution


ggplot(main_data,aes(market_value))+geom_histogram(binwidth = 2.5)


## It is not a normal distribution, which was expected. Teams usually have few elite players, 
## and a large number of low and mid value players. An analysis of a team's first 15 
## would mostly be a normal distribution, as we will be excluding low value players.


### Market Value Distribution for the Top 6 teams

ggplot(main_data,aes(market_value))+geom_histogram(binwidth = 0.5) + facet_wrap(~big_club)
## The top 6 have a more normal distribution of players, while the other teams have most 
## players worth below 10 million.

### Player Popularity Distribution 

ggplot(main_data,aes(page_views))+geom_histogram(binwidth = 50)


## The distribution is similar to market value, except for 2 outliers - Wayne Rooney and Paul Pogba. 


### Player Popularity Distribution of top 6 teams vs others


ggplot(main_data,aes(page_views))+geom_histogram(binwidth = 50)+facet_wrap(~big_club)


## Again, the top 6 clubs has a more normal distribution players popularity. 


## Detailed Analysis

## We are trying to see if there is a correlation between player's market value and 
## player popularity he is. It is difficult measure ability and performance as it depends on many factors

## We will also see if there is a relation with FPL data

### FPL Valuation

main_data %>% filter(!club_id %in% c(3,8,13)) %>%
  filter(age < 35) %>%
  filter(fpl_points!=0) %>%
  ggplot(aes(fpl_value,market_value,color=age))+geom_jitter(alpha=0.5,size=2)

## For most part there is a good relation between the two, except for a few older players, who have
## a low transfer value but a high FPL value. This is beacuse FPL value does not treat age as a factor
## We can see this below

main_data %>% 
  filter(fpl_points!=0) %>%
  mutate(val_ratio=market_value/fpl_value) %>% 
  group_by(age_category,position_cat) %>%
  summarize(val_ratio=mean(val_ratio)) %>%
  mutate(position_cat=fct_recode(position_cat,"Forward"="1","Midfield"="2","Defence"="3","Goalkeeper"="4")) %>% 
  ggplot(aes(age_category,val_ratio,fill=position_cat))+geom_bar(stat="identity") + facet_wrap(~position_cat) +
  theme_hc() +ylab("Market Value / FPL Value") + xlab("Age")

## Here we see that very young and unproven players have a low ratio. Also, at the old players have 
## very low market values, but they may still be valuable over the next season.


### Market Value with Age

main_data %>%ggplot(aes(age,market_value))+geom_jitter(alpha=0.25,size=3)

## The high value players are clustered around the age of 24-32, peaking at about 27. 
## There is no linear relationship, which is why age categories is used in the 
## regression model later.


### Cumulative player market value per position
## Taking top 6 teams into consideration

main_data %>%filter(club_id %in% c(1,5,10,11,12,17)) %>%
  group_by(club,position_cat) %>%
  summarise(value=sum(market_value)) %>%
  ungroup() %>% 
  mutate(position_cat=fct_recode(position_cat,"Forward"="1","Midfield"="2","Defence"="3","Goalkeeper"="4")) %>%
  ggplot(aes(club,value,fill=position_cat))+geom_bar(stat = "identity") +facet_wrap(~position_cat)+
  theme(axis.text.x = element_text(angle = 60,hjust=0.6,vjust=0.5))

## Here we see in which positions respective teams have invested


## Popularity a replacement for Ability

## Here, we test the hypothesis that there is a relationship between ability and popularity. 
## Ability is difficult to measure. Hence, we assume FPL valuation is a fair measure of ability.


ggplot(train_data,aes(fpl_value,page_views))+geom_jitter()


## Here we see a good linear relationship


## Regression Model

## We want to see if market value can be determined using popularity instead of 
## ability. A player's market value can intuitively be represented as -

## > market value ~ ability + position + age

## In the above relation, we are using popualrity instead of ability. Popularity is being measured by
## number of page views on the players Wikipedia page.


### Dataset Modifications

## 1. The newly-promoted clubs are removed from the dataset, because the Premier League 
## offers a much higher level of publicity, which these clubs weren't exposed to in the previous year.

## 2. New signings for the 17/18 from abroad are excluded. However, 
## players who were transferred within the Premier league are retained.

## 3. sqrt values of `market_value` are taken, because `market_value` is right-tail heavy, which 
## could lead to heteroscedasticity.

## 4. However, this affects the relationship between `sqrt(market_value)` and `page_views`

ggplot(train_data,aes(y=sqrt(market_value),x=page_views)) +geom_jitter()


## After applying a sqrt transform on `page_views`, we get the following graph

ggplot(train_data,aes(y=sqrt(market_value),x=sqrt(page_views))) +geom_jitter()

## This shows a better linear relationship.


## Creating a multiple linear regression model

# modifying training data
train_data <- main_data %>% 
  filter(!club_id %in% c(3,8,13)) %>%
  filter(new_foreign == 0) 
train_data$page_views <- sqrt(train_data$page_views)

model <- lm(sqrt(market_value) ~   page_views+age_category:position_cat+page_views:region+page_views:big_club+new_signing:page_views, data=train_data)
summary(model)
# Call:
#   lm(formula = sqrt(market_value) ~ page_views + age_category:position_cat + 
#        page_views:region + page_views:big_club + new_signing:page_views, 
#      data = train_data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.34847 -0.55618 -0.01892  0.58945  2.24128 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      0.130222   0.267112   0.488 0.626199    
# page_views                       0.054551   0.006235   8.750  < 2e-16 ***
#   age_category17-21:position_cat1  0.256266   0.318218   0.805 0.421187    
# age_category22-25:position_cat1  1.576169   0.298532   5.280 2.28e-07 ***
#   age_category26-28:position_cat1  1.542115   0.288898   5.338 1.70e-07 ***
#   age_category29-31:position_cat1  1.028739   0.309253   3.327 0.000973 ***
#   age_category32-38:position_cat1 -0.845027   0.481267  -1.756 0.079996 .  
# age_category17-21:position_cat2  0.644210   0.366759   1.756 0.079884 .  
# age_category22-25:position_cat2  1.270794   0.315609   4.026 6.95e-05 ***
#   age_category26-28:position_cat2  1.560059   0.294349   5.300 2.06e-07 ***
#   age_category29-31:position_cat2  0.886903   0.337567   2.627 0.008986 ** 
#   age_category32-38:position_cat2  0.231237   0.343605   0.673 0.501411    
# age_category17-21:position_cat3  0.282958   0.366732   0.772 0.440895    
# age_category22-25:position_cat3  1.229583   0.299980   4.099 5.17e-05 ***
#   age_category26-28:position_cat3  1.571121   0.284149   5.529 6.32e-08 ***
#   age_category29-31:position_cat3  1.071693   0.303386   3.532 0.000467 ***
#   age_category32-38:position_cat3  0.416735   0.323600   1.288 0.198668    
# age_category17-21:position_cat4 -1.551477   0.921304  -1.684 0.093078 .  
# age_category22-25:position_cat4  1.352890   0.467968   2.891 0.004081 ** 
#   age_category26-28:position_cat4  1.010454   0.508002   1.989 0.047476 *  
#   age_category29-31:position_cat4  0.842614   0.356848   2.361 0.018763 *  
#   age_category32-38:position_cat4        NA         NA      NA       NA    
# page_views:region2               0.011488   0.003962   2.899 0.003978 ** 
#   page_views:region3               0.013590   0.005710   2.380 0.017852 *  
#   page_views:region4               0.006894   0.005604   1.230 0.219403    
# page_views:big_club              0.021383   0.004151   5.152 4.33e-07 ***
#   page_views:new_signing           0.002002   0.004169   0.480 0.631360    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8852 on 348 degrees of freedom
# Multiple R-squared:  0.7218,	Adjusted R-squared:  0.7018 
# F-statistic: 36.12 on 25 and 348 DF,  p-value: < 2.2e-16


## Here we see a good R-squared of 72%. Also, we see a good relation between market value and page views


### Residual Plot

## The residual plots will show if we have a heteroscedasticity problem in the data.

res <- as.data.frame(resid(model))
train_data$error <- res$`resid(model)`
train_data <- train_data %>% mutate(model_mv=sqrt(market_value)-error)
train_data$model_mv <- (train_data$model_mv)^2
ggplot(train_data,aes(model_mv,error))+geom_point()
ggplot(train_data,aes(sample=error))+geom_qq()


## The residual plot shows that errors are randomly distributed, and the qq plot confirms that they 
## are normally distributed.  
