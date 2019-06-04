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
#=============Load Data======================
#Read both train and test data and combine them into a single csv file.
t_train = read.csv('train.csv', na.strings = c("", '#N/A', '[]', '0'))
t_test = read.csv('test.csv', na.strings = c("", '#N/A', '[]', '0'))
t_test$revenue = 0
t_full = bind_rows(t_train,t_test)
# View(t_full)

summary(t_full)
glimpse(t_full)

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

"
Use the lines below to attach the number of genres
f = str_extract_all(t_full[2,]$genres, genres_pattern,simplify = T)
f
"

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
ggplot(t_full[1:3000,]) + geom_point(aes(x = overview_length, y = revenue))

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

Skew(t_full$revenue[1:3000])
hist(t_full$revenue[1:3000], breaks = 20)
# As the target variable revenue is highly skewed we will normalise it
symbox(t_full$revenue[1:3000], data = t_full, powers=c(3,2,1,0,-0.5,-1,-2))
t_full$revenue_mod = log(t_full$revenue)
Skew(t_full$revenue_mod[1:3000])
hist(t_full$revenue_mod[1:3000], breaks = 20)

sum(t_full$revenue[1:3000] == 0)
boxcox(t_full$revenue[1:3000], c(-3,3), optimize = TRUE) # lambda = 0.1637742

hist((t_full$revenue[1:3000]^0.164-1)/0.164)
skewness((t_full$revenue[1:3000]^0.164-1)/0.164)
t_full$revenue_mod = (t_full$revenue^0.164 - 1)/0.164

myfun<-function(x) mean(is.na(x))*100
apply(t_full[,c(colnames(t_full)[colSums(is.na(t_full)) > 0])],2,myfun) #Getting percentage of missing values in each variable
