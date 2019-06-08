# Libraries Used

library(tidyverse)
#library(sparklyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(forcats)
library(car)
library(Hmisc)
library(lubridate)
library(DescTools)
library(EnvStats)
library(stats)
library(permute)
library(caret)
library(corrplot)

#=============Load Data======================
#Read both train and test data and combine them into a single csv file.
t_train = read.csv('train.csv', na.strings = c("", '#N/A', '[]', '0'))
t_test = read.csv('test.csv', na.strings = c("", '#N/A', '[]', '0'))
t_test$revenue = 0
t_full = bind_rows(t_train,t_test)
# View(t_full)

summary(t_full)
glimpse(t_full)

names(t_full)[names(t_full) == "ï..id"] = 'id'
#===============EDA============================

t_full$collection_name = str_extract(t_full$belongs_to_collection, pattern = "(?<=name':\\s{1}').*(?=', 'poster)")
t_full$collection_name


t_full %>%
  group_by(collection_name) %>%
  summarise(movie_count = n()) %>%
  arrange(desc(movie_count)) %>%
  filter(!is.na(collection_name)) %>%
  head(10) 

sum(is.na(t_full$collection_name[1:3000]))
# 2413 movies donot have collection in training data

# has_collection = '1',if the movie is part of a collection or '0'
t_full$has_collection[!is.na(t_full$collection_name)] = '1'
t_full$has_collection[is.na((t_full$collection_name))] = '0'

m = t_full[1:3000,]
ggplot(m, aes(x = has_collection, y = revenue)) + geom_boxplot() 
# From the above plot we can observe that the movies which belong to a collection have relatively more revenue.

#Now we will extract the first genre(i.e main genre) from the genres
genres_pattern <- "Comedy|Horror|Action|Drama|Documentary|Science Fiction|
              Crime|Fantasy|Thriller|Animation|Adventure|Mystery|War|Romance|Music|
Family|Western|History|TV Movie|Foreign"
t_full$main_genre = str_extract(t_full$genres, genres_pattern)
head(t_full$main_genre)

m = t_full[1:3000,]
ggplot(m) + geom_boxplot(aes(x = main_genre, y = revenue)) + theme(axis.text.x=element_text(angle=90)) +
  labs(title = "Revenue v/s Movie main Genre") + scale_y_continuous(breaks = c(0, 500000000, 1000000000, 1500000000),
                                                                    labels = c('$0M', '$500M', '$1000M', '$1500M'))
ggplot(t_full[1:3000,],  aes(x = fct_infreq(main_genre),  y=revenue, fill = main_genre)) + 
  stat_summary_bin(fun.y = median, geom = "bar") + 
  scale_fill_grey() + 
  scale_y_continuous(breaks = c(0, 25000000, 50000000, 70000000),
                     labels = c('$0M', '$25M', '$50M', '$70M')) +
  coord_flip() +
  theme_classic() +
  theme(legend.position = 'none') +  
  labs(title = 'Median revenue by genre', x = 'Genre', y = 'Median revenue') 

t_full$num_genres = str_count(t_full$genres, 'name')
ggplot(t_full[1:3000,], aes(x = factor(num_genres), y = revenue)) + geom_boxplot()

ggplot(t_full[1:3000,],  aes(x = fct_infreq(factor(num_genres)),  y=revenue, fill = main_genre)) + 
  stat_summary_bin(fun.y = median, geom = "bar") + 
  scale_fill_grey() + 
  coord_flip() +
  theme_classic() +
  theme(legend.position = 'none') +  
  labs(title = 'Median revenue by number of genre', x = 'Genre', y = 'Median revenue') 


barplot(table(t_full$main_genre), main = "Movir Genre Frequency Plot", las = 2)

# Number of Unique values of homepage
length(unique(t_full$homepage))
sum(is.na(t_full$homepage))

sum(is.na(t_full$homepage))


# has_homepage = '1',if the movie has a homepage or '0'
t_full$has_homepage[!is.na(t_full$homepage)] = '1'
t_full$has_homepage[is.na((t_full$homepage))] = '0'

ggplot(t_full[1:3000,], aes(x = has_homepage, y = revenue)) + geom_boxplot()
# From the plot we can understand that the movies with homepage have more mean revenue 

length(unique(t_full$imdb_id))
# All the movies have unique imdb_id and  it wont affect revenue, so, we'll remove that column.

sort(table(t_full$original_language), decreasing = T)[1:10]
#We can observe that most of the movies are in english language.

t_full$language[t_full$original_language == 'en'] = '1'
t_full$language[!(t_full$original_language== 'en')] = '0'
sum(t_full$language == '0')

#Now let us plot the language with respect to revenue
ggplot(t_full[1:3000,]) + geom_boxplot(aes(x = language, y = revenue))
ggplot(t_full[1:3000,]) + geom_boxplot(aes(x = original_language, y = revenue))

length(unique(t_full$original_title))
# Original titles are all different. So, this will not have affect on the target variable.
# So, we can remove the original title while training

t_full$overview_length = nchar(t_full$overview)
t_full$overview_length[is.na(t_full$overview_length)] = 0
ggplot(t_full[1:3000,], aes(x = overview_length, y = revenue)) + geom_point() + 
  geom_smooth(method = 'lm', color = 'red', fill = 'red')

# Let us give overview_length for 'na' values to 0.
t_full$overview_length[is.na(t_full$overview_length)] = 0.01 

cor(t_full$revenue[1:3000],t_full$overview_length[1:3000])
symbox(t_full$overview_length, data = t_full, powers=c(3,2,1,0,-0.5,-1,-2))
m = t_full$overview_length ^ -1
cor(t_full$revenue[1:3000],m[1:3000])
# SInce length of characters in overview is not related to revenue, we will not consider it for training
t_full$overview_length = NULL

ggplot(t_full[1:3000,], aes(x = popularity, y = revenue)) + geom_point() +
  geom_smooth(method = 'lm', color = 'red', fill = 'red')

cor(t_full$revenue[1:3000], t_full$popularity[1:3000])
symbox(t_full$popularity, data = t_full, powers=c(3,2,1,0,-0.5,-1,-2))
ggplot(t_full[1:3000,]) + geom_point(aes(x = popularity, y = revenue))
m = t_full$popularity ^ -1
cor(m[1:3000],t_full$revenue[1:3000])
# Although we can observe that After transformation, we are able to normalize the popularity, it's relation with target 'revenue' is decreasing.

# Let us look into the status of the movie
describe(t_full$status[1:3000])
#Most of the movies are released and only few are observations are different. So, the status donot effect revenue.

sum(is.na(t_full$poster_path))
length(unique(t_full$poster_path))
# Only 2 missiling values in poster path and rest all are unique. So, we will remove poster_path
t_full$poster_path = NULL

sum(is.na(t_full$production_companies))

t_full$production_name = str_extract(t_full$production_companies, pattern = "(?<=name':\\s{1}')\\D*(?=', 'id')")
sum(is.na(t_full$production_name))

# Let us look at the top 10 production companies with respect to count in movies.
t_full[1:3000,] %>%
  group_by(production_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(production_name)) %>%
  head(20)

t_full$production_name[t_full$production_name != 'Universal Pictures' & t_full$production_name != 'Paramount Pictures' &
                         t_full$production_name != 'Twentieth Century Fox Film Corporation' & t_full$production_name != 'Columbia Pictures' &
                         t_full$production_name != 'Warner Bros.' & t_full$production_name != 'New Line Cinema' &
                         t_full$production_name != 'Walt Disney Pictures'] = 'Other'

t_full$production_name[is.na(t_full$production_name)] = 'Other'
sum(t_full$production_name == 'Other')

ggplot(t_full[1:3000,]) + geom_boxplot(aes(x = production_name, y = revenue)) 

ggplot(t_full[1:3000,], aes(x = production_name, y = revenue, fill = production_name)) +
  stat_summary_bin(fun.y = median, geom = "bar") + 
  scale_y_continuous(breaks = c(0, 500000000,1000000000, 1500000000),
  labels = c('$0M', '$500M','$1000M', '$1500M')) + theme(legend.position = 'none') 
  
t_full$number_productions = str_count(t_full$production_companies, pattern = "(?<=name':\\s{1}')\\D*(?=', 'id')")
t_full$number_productions[is.na(t_full$number_productions)] = 1
describe(t_full$number_productions)
ggplot(t_full[1:3000,]) + geom_boxplot(aes(x = number_productions, y = revenue, group = number_productions))

cor(t_full$revenue, t_full$number_productions)

t_full$countryOf_production = str_extract(t_full$production_countries, "(?<=3166_1':\\s{1}')\\D*(?=', 'name')")

ggplot(t_full[1:3000,]) + geom_boxplot(aes(x = countryOf_production, y = revenue))
length(unique(t_full$countryOf_production))

# Now let us look into top 10 countries releasing more movies

t_full[1:3000,] %>%
  group_by(countryOf_production) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(countryOf_production)) %>%
  head(10)

t_full$countryOf_production[t_full$countryOf_production != 'US' & t_full$countryOf_production != 'GB' & 
                              t_full$countryOf_production != 'FR'] = 'Oth'

t_full$countryOf_production[is.na(t_full$countryOf_production)] = 'Oth'
ggplot(t_full[1:3000,]) + geom_boxplot(aes(x = countryOf_production, y = revenue))

ggplot(t_full[1:3000,], aes(x = countryOf_production, y=revenue, fill=countryOf_production)) +
  geom_boxplot() +
  scale_fill_brewer(palette = 'RdGy') + 
  scale_y_continuous(breaks = c(0, 500000000, 1000000000, 1500000000),
                     labels = c('$0M', '$500M', '$1,000M', '$1,500M')) +
  theme_classic() +
  theme(legend.position = 'none') +
  labs(title='Revenue by production country', x='Production country', y='Revenue')

ggplot(t_full[1:3000,], aes(x=countryOf_production, y=revenue, fill=countryOf_production)) +
  stat_summary_bin(fun.y = median, geom = "bar") + 
  #scale_fill_brewer(palette = 'RdGy') + 
  scale_y_continuous(breaks = c(0, 5000000,10000000, 15000000, 20000000),
                     labels = c('$0M', '$5M','$10M', '$15M', '$20M')) +
  theme_classic() +
  theme(legend.position = 'none') +
  labs(title = 'Median revenue by top production countries', 
       x = 'Countries with top production', y = 'Median revenue')

sum(is.na(t_full$release_date))
which(is.na(t_full$release_date))
t_full$release_date[3829] = '3/20/01'

t_full$release_date_mod = parse_date_time2(t_full$release_date, "mdy", cutoff_2000 = 20)
t_full$release_year = year(t_full$release_date_mod)
t_full$release_quarter = quarter(t_full$release_date_mod)
t_full$release_month = month(t_full$release_date_mod)
t_full$release_day = weekdays(t_full$release_date_mod)
t_full$release_week = week(t_full$release_date_mod)

ggplot(t_full[1:3000,], aes(x = release_year, y = revenue, color = release_year)) +
  geom_point() + geom_smooth(method = 'lm', color = 'red', fill = 'red')

cor(t_full$release_year[1:3000], t_full$revenue[1:3000])

ggplot(t_full[1:3000,], aes(x = factor(release_quarter), y = revenue, fill = factor(release_quarter))) +
  stat_summary_bin(fun.y = median, geom = "bar") +
  scale_y_continuous(breaks = c(0, 5000000,10000000, 15000000, 20000000),
  labels = c('$0M', '$5M','$10M', '$15M', '$20M')) + theme(legend.position = 'none')
# From the above plot we can see that the median revenue in second and fourth quarter is higher.

ggplot(t_full[1:3000,], aes(x = factor(release_month), y = revenue, fill = factor(release_month))) +
  stat_summary_bin(fun.y = median, geom = "bar") +
  scale_y_continuous(breaks = c(0, 5000000,10000000, 15000000, 20000000),
                     labels = c('$0M', '$5M','$10M', '$15M', '$20M')) + theme(legend.position = 'none')
# From the above plot we can see that the median revenue is high in the months of June, July and December.

ggplot(t_full[1:3000,], aes(x = factor(release_week), y = revenue, fill = factor(release_week))) +
  stat_summary_bin(fun.y = median, geom = "bar") +
  scale_y_continuous(breaks = c(0, 5000000,10000000, 15000000, 20000000),
                     labels = c('$0M', '$5M','$10M', '$15M', '$20M')) + theme(legend.position = 'none', axis.text.x = element_text(angle = 90))
# From the above plot we can see that the median revenue is high in few weeks.

ggplot(t_full[1:3000,], aes(x = factor(release_day), y = revenue, fill = factor(release_day))) +
  stat_summary_bin(fun.y = median, geom = "bar") +
  scale_y_continuous(breaks = c(0, 5000000,10000000, 15000000, 20000000),
                     labels = c('$0M', '$5M','$10M', '$15M', '$20M')) + theme(legend.position = 'none', axis.text.x = element_text(angle = 90))
# From the above plot we can see that the median revenue is high Wednesday, Thursday and Friday.

ggplot(t_full[1:3000,], aes(x = runtime, y = revenue)) + geom_point() +
  geom_smooth(method = 'lm', color = 'red', fill = 'red')

# From the above plot we can observe that the movie runtime has some relation with revenue.
symbox(t_full$runtime, data = t_full, powers=c(3,2,1,0,-0.5,-1,-2))

t_full$num_spoken_languages = str_count(t_full$spoken_languages, "name")

ggplot(t_full[1:3000,], aes(x = factor(num_spoken_languages), y = revenue, fill = factor(num_spoken_languages))) + geom_boxplot() +
  theme(legend.position = 'None') + labs(x = 'number of Spoken Languages', y = 'revenue') 

ggplot(t_full[1:3000,], aes(x = factor(num_spoken_languages), y = revenue, fill = factor(num_spoken_languages))) +
  stat_summary_bin(fun.y = median, geom = "bar") + theme(legend.position = 'None') +
  labs(title = 'Median value of revenue for number of spoken languages',x = 'number of Spoken Languages', y = 'revenue')
# From the above bar plot we can see that the median revenue is comparitively high for movies with 5,6,7 number of spoken languages.

# We will define a variable has_tagline, has_tagine = '1', i the movie has a tagline or '0'.
t_full$has_tagline = ifelse(!is.na(t_full$tagline), '1', '0')
sum(t_full$has_tagline[1:3000] == '0')

ggplot(t_full[1:3000,], aes(x = has_tagline, y = revenue, fill = has_tagline)) + geom_boxplot() +
  theme(legend.position = 'None') 

ggplot(t_full[1:3000,], aes(x = has_tagline, y = revenue, fill = has_tagline)) +
  stat_summary_bin(fun.y = median, geom = "bar") + theme(legend.position = 'None') +
  scale_y_continuous(breaks = c(0, 5000000,10000000, 15000000, 20000000),
                     labels = c('$0M', '$5M','$10M', '$15M', '$20M'))
# From the above plot we can observe that there is large difference between median revenue value for movies which 
# have or donot have a tagline.

t_full$num_keywords = str_count(t_full$Keywords, 'name')

ggplot(t_full[1:3000,], aes(x = as.factor(num_keywords), y = revenue, fill = as.factor(num_keywords))) + 
  geom_boxplot() + theme(legend.position = 'None')

t_full$num_cast = str_count(t_full$cast, 'name')
t_full$num_crew = str_count(t_full$crew, 'name')

ggplot(t_full[1:3000,], aes(x = num_cast, y = revenue)) + geom_point() +
  geom_smooth(method = 'lm', color = 'red', fill = 'red')

ggplot(t_full[1:3000,], aes(x = num_crew, y = revenue)) + geom_point() +
  geom_smooth(method = 'lm', color = 'red', fill = 'red')

cor(t_full$revenue[1:3000], t_full$num_cast[1:3000], use = "complete.obs")
cor(t_full$revenue[1:3000], t_full$num_crew[1:3000], use = "complete.obs")



sum(t_full$revenue[1:3000] == 0)
boxcox(t_full$revenue[1:3000], c(-3,3), optimize = TRUE) # lambda = 0.1637742
"
hist((t_full$revenue[1:3000]^0.164-1)/0.164)
skewness((t_full$revenue[1:3000]^0.164-1)/0.164)
t_full$revenue_mod = (t_full$revenue^0.164 - 1)/0.164
"
myfun<-function(x) mean(is.na(x))*100
apply(t_full[,c(colnames(t_full)[colSums(is.na(t_full)) > 0])],2,myfun) #Getting percentage of missing values in each variable


# Revenue replaced as per kaggle kernel
t_full[16, 'revenue'] <- 192864
t_full[313, 'revenue'] <- 12000000
t_full[451, 'revenue'] <- 12000000
t_full[1865, 'revenue'] <- 25000000

# Budget replaced as per kaggle kernel
t_full[90, 'budget'] <- 30000000
t_full[118, 'budget'] <- 60000000
t_full[149, 'budget'] <- 18000000
t_full[464, 'budget'] <- 20000000
t_full[470, 'budget'] <- 13000000
t_full[513, 'budget'] <- 930000
t_full[797, 'budget'] <- 8000000
t_full[819, 'budget'] <- 90000000
t_full[850, 'budget'] <- 90000000
t_full[1007, 'budget'] <- 2
t_full[1112, 'budget'] <- 7500000
t_full[1131, 'budget'] <- 4300000
t_full[1359, 'budget'] <- 10000000
t_full[1542, 'budget'] <- 1
t_full[1570, 'budget'] <- 15800000
t_full[1571, 'budget'] <- 4000000
t_full[1714, 'budget'] <- 46000000
t_full[1721, 'budget'] <- 17500000
t_full[1885, 'budget'] <- 12
t_full[2091, 'budget'] <- 10
t_full[2268, 'budget'] <- 17500000
t_full[2491, 'budget'] <- 6
t_full[2602, 'budget'] <- 31000000
t_full[2612, 'budget'] <- 15000000
t_full[2696, 'budget'] <- 10000000
t_full[2801, 'budget'] <- 10000000
t_full[335, 'budget'] <- 2
t_full[348, 'budget'] <- 12
t_full[470, 'budget'] <- 13000000
t_full[513, 'budget'] <- 1100000
t_full[640, 'budget'] <- 6
t_full[696, 'budget'] <- 1
t_full[797, 'budget'] <- 8000000
t_full[850, 'budget'] <- 1500000
t_full[1199, 'budget'] <- 5
t_full[1282, 'budget'] <- 9
t_full[1347, 'budget'] <- 1
t_full[1755, 'budget'] <- 2
t_full[1801, 'budget'] <- 5
t_full[1918, 'budget'] <- 592
t_full[2033, 'budget'] <- 4
t_full[2118, 'budget'] <- 344
t_full[2252, 'budget'] <- 130
t_full[2256, 'budget'] <- 1
t_full[2696, 'budget'] <- 10000000
t_full[3033, 'budget'] <- 250
t_full[3051, 'budget'] <- 50
t_full[3084, 'budget'] <- 337
t_full[3224, 'budget'] <- 4
t_full[3594, 'budget'] <- 25
t_full[3619, 'budget'] <- 500
t_full[3831, 'budget'] <- 3
t_full[3935, 'budget'] <- 500
t_full[4049, 'budget'] <- 995946
t_full[4424, 'budget'] <- 3
t_full[4460, 'budget'] <- 8
t_full[4555, 'budget'] <- 1200000
t_full[4624, 'budget'] <- 30
t_full[4645, 'budget'] <- 500
t_full[4709, 'budget'] <- 450
t_full[4839, 'budget'] <- 7
t_full[3125, 'budget'] <- 25
t_full[3142, 'budget'] <- 1
t_full[3201, 'budget'] <- 450
t_full[3222, 'budget'] <- 6
t_full[3545, 'budget'] <- 38
t_full[3670, 'budget'] <- 18
t_full[3792, 'budget'] <- 19
t_full[3881, 'budget'] <- 7
t_full[3969, 'budget'] <- 400
t_full[4196, 'budget'] <- 6
t_full[4221, 'budget'] <- 11
t_full[4222, 'budget'] <- 500
t_full[4285, 'budget'] <- 11
t_full[4319, 'budget'] <- 1
t_full[4639, 'budget'] <- 10
t_full[4719, 'budget'] <- 45
t_full[4822, 'budget'] <- 22
t_full[4829, 'budget'] <- 20
t_full[4969, 'budget'] <- 20
t_full[5021, 'budget'] <- 40
t_full[5035, 'budget'] <- 1
t_full[5063, 'budget'] <- 14
t_full[5119, 'budget'] <- 2
t_full[5214, 'budget'] <- 30
t_full[5221, 'budget'] <- 50
t_full[4903, 'budget'] <- 15
t_full[4983, 'budget'] <- 3
t_full[5102, 'budget'] <- 28
t_full[5217, 'budget'] <- 75
t_full[5224, 'budget'] <- 3
t_full[5469, 'budget'] <- 20
t_full[5840, 'budget'] <- 1
t_full[5960, 'budget'] <- 30
t_full[6506, 'budget'] <- 11
t_full[6553, 'budget'] <- 280
t_full[6561, 'budget'] <- 7
t_full[6582, 'budget'] <- 218
t_full[6638, 'budget'] <- 5
t_full[6749, 'budget'] <- 8
t_full[6759, 'budget'] <- 50
t_full[6856, 'budget'] <- 10
t_full[6858, 'budget'] <- 100
t_full[6876, 'budget'] <- 250
t_full[6972, 'budget'] <- 1
t_full[7079, 'budget'] <- 8000000
t_full[7150, 'budget'] <- 118
t_full[6506, 'budget'] <- 118
t_full[7225, 'budget'] <- 6
t_full[7231, 'budget'] <- 85
t_full[5222, 'budget'] <- 5
t_full[5322, 'budget'] <- 90
t_full[5350, 'budget'] <- 70
t_full[5378, 'budget'] <- 10
t_full[5545, 'budget'] <- 80
t_full[5810, 'budget'] <- 8
t_full[5926, 'budget'] <- 300
t_full[5927, 'budget'] <- 4
t_full[5986, 'budget'] <- 1
t_full[6053, 'budget'] <- 20
t_full[6104, 'budget'] <- 1
t_full[6130, 'budget'] <- 30
t_full[6301, 'budget'] <- 150
t_full[6276, 'budget'] <- 100
t_full[6473, 'budget'] <- 100
t_full[6842, 'budget'] <- 30

Skew(t_full$revenue[1:3000])
hist(t_full$revenue[1:3000], breaks = 20)
# As the target variable revenue is highly skewed we will normalise it
symbox(t_full$revenue[1:3000], data = t_full, powers=c(3,2,1,0,-0.5,-1,-2))
t_full$revenue_mod = log(t_full$revenue)
Skew(t_full$revenue_mod[1:3000])
hist(t_full$revenue_mod[1:3000], breaks = 20)

myfun<-function(x) mean(is.na(x))*100
apply(t_full[,c(colnames(t_full)[colSums(is.na(t_full)) > 0])],2,myfun) #Getting percentage of missing values in each variable

#Function to get mode 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


cor(t_full$fc6[1:3000], t_full$revenue_mod[1:3000], use = "complete.obs")

apply(full_data[,c(colnames(full_data)[colSums(is.na(full_data)) > 0])],2,myfun) #Getting percentage of missing values in each variable

mean_budget = aggregate(t_full$budget, by=list(t_full$release_year), FUN=mean, na.rm = TRUE)
t_full$budget[is.na(t_full$budget)] = mean_budget$x[mean_budget$Group.1 == t_full$release_year[is.na(t_full$budget)]]

#let us give the missing values of budget in each year as the mean budget in that year.
for(i in 1:nrow(t_full)) {
  row <- t_full[i,]
  if(is.na(row$budget)){
    row$budget = mean_budget$x[mean_budget$Group.1==row$release_year]
  }
  t_full[i,] = row
}

t_full$num_genres[is.na(t_full$num_genres)] = getmode(t_full$num_genres)
t_full$main_genre[is.na(t_full$main_genre)] = 'Drama'
t_full$num_spoken_languages = getmode(t_full$num_spoken_languages)
t_full$num_keywords[is.na(t_full$num_keywords)] = 0
t_full$num_cast = getmode(t_full$num_cast)
t_full$num_crew = getmode(t_full$num_crew)
t_full$runtime[is.na(t_full$runtime)] = mean(t_full$runtime, na.rm = T)


# Feature Construction 

t_full$fc1 = t_full$budget*t_full$popularity
t_full$fc2 = t_full$budget/t_full$release_year
t_full$fc3 = t_full$budget/t_full$runtime
t_full$fc4 = t_full$popularity * t_full$runtime
t_full$fc5 = t_full$popularity/t_full$release_year

full_data = subset(t_full, select = -c(belongs_to_collection, genres, homepage, imdb_id, original_title,
                                       original_language, overview, production_companies, production_countries, 
                                       release_date, release_date_mod, spoken_languages, status, tagline,
                                       title, Keywords, cast, crew, revenue, collection_name))

apply(full_data[,c(colnames(full_data)[colSums(is.na(full_data)) > 0])],2,myfun) #Getting percentage of missing values in each variable

normalize <- function(x){(x-min(x))/(max(x)-min(x))}

full_data$budget = normalize(full_data$budget)
full_data$popularity = normalize(full_data$popularity)
full_data$runtime = normalize(full_data$runtime)
full_data$fc1 = normalize(full_data$fc1)
full_data$fc2 = normalize(full_data$fc2)
full_data$fc3 = normalize(full_data$fc3)
full_data$fc4 = normalize(full_data$fc4)
full_data$fc5 = normalize(full_data$fc5)

full_data$release_quarter = as.factor(full_data$release_quarter)
full_data$release_month = as.factor(full_data$release_month)
full_data$release_quarter = as.factor(full_data$release_quarter)
full_data$release_week = as.factor(full_data$release_week)

c_corr <- full_data[sapply(full_data[1:3000,], function(x) is.numeric(x))]
corr_c <- cor(c_corr, use = "complete.obs")
View(corr_c)
corrplot(corr_c, method = "square", tl.cex = 0.6, tl.offset = 0.4,tl.srt = 90, cl.ratio = 0.3)

cor(full_data$fc5[1:3000], full_data$fc1[1:3000])

# From the abogve correlation plot we can observe that fc2 and fc3 are highly correlated and fc2 is more correlated to revenue_mod,
# we can remove fc3. Similarly, we remove fc4
full_data$fc3 = NULL
full_data$fc4 = NULL

#Also, fc1 is highly correlated to fc5, so we remove fc1.
full_data$fc1 = NULL

#=========================Final Data Splitting==============================

full_data$id = NULL
train_data = full_data[1:3000,]
test_data = full_data[3001:nrow(full_data),]

set.seed(99)
#train_data = shuffle(train_data)

test_data$revenue_mod = NULL

nearZeroVar(t_full)

#=======================Linear Regression=================================

lm_fit = lm(revenue_mod~., train_data)
summary(lm_fit)
AIC(lm_fit) #13827.65
BIC(lm_fit) #14494.35
RSS <- c(crossprod(fit2$residuals)) 
MSE <- RSS / length(fit2$residuals) #Mean squared error:
RMSE <- sqrt(MSE) # 39.39074 #Root Mean Squared Error
RMSE

par(mfrow = c(2,2))
plot(lm_fit)
#The first plot clearly shows that residuals of our model are spread equally along the horizontal line, indicating that our model do not have non-linear relationship. There are few variables which are far from the horizontal line.
#The QQ Plot indicates residuals are clearly normally distributed as they have all the values held on the dashed line. 
#The Standardized and fitted values plot shows few of the variables are not transformed but many of them are falls I straight line.
#Standardized residuals vs leverage indicates that 402 observation is outside cooks distance.  617 and 754 have high leverage.
dev.off()
plot(cooks.distance(lm_fit), rstudent(lm_fit))


influencePlot(lm_fit)

ncvTest(lm_fit)

plot(lm_fit$fitted.values, lm_fit$residuals)

#=======================Gradient Boosting================================================
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

caretGrid <- expand.grid(interaction.depth=c(1, 3, 5), n.trees = (0:50)*50,
                         shrinkage=c(0.01, 0.001),
                         n.minobsinnode=10)
metric <- "RMSE"

set.seed(99)
gbm_fit <- train(revenue_mod ~ ., data=train_data, distribution="gaussian", method="gbm",
                  trControl=train_control, verbose=FALSE, 
                  tuneGrid=caretGrid, metric=metric, bag.fraction=0.75)

save(gbm_fit,file =  "gbm_fit.rda")

