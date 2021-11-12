# Ian Skinner
# Created: 2021-08-28
# Description: this script scrapes web data from bringatrailer.com in order to examine the market
# and some other stuff i'm not sure of yet
# i guess i could use this to find good deals on cars or 'investment cars' to keep for a bit, enjoy, sell for profit

#clean environment
rm(list = ls())

#packages
library(rvest)
library(dplyr)
library(lubridate)
library(stringr)
library(tictoc)
library(na.tools)
library(RCurl)
library(httr)
library(chron)
library(ggplot2)
library(scales)

#start timer
tic()

#create a dummy dataset to write stuff to later
cars = data.frame(url = character(),
                  title = character(),
                  lot = character(),
                  state = character(),
                  miles = character(),
                  price = character(),
                  sold = character(),
                  reserve = character(),
                  clean_carfax = character(),
                  date = character(),
                  end_stamp = character(),
                  bids = character(),
                  comments = character(),
                  views = character(),
                  watchers = character())

#since the links are stored in json (i think???), i created a workaround that generates the links. it's not great but works overall,
#may not be expandable/plug and play for any car type...

#general process: generate possible links, loop through them and see if they return when pinged, then only keep the live ones
model_years = c(2006:2008)
model_years_count = length(model_years)
#car_id = "-bmw-z4-m-roadster-"  #-bmw-z4-m-roadster- -bmw-m3-
car_id = "-bmw-z4-m-roadster-"  #-bmw-z4-m-roadster- -bmw-m3-
max_page = 50

#generate urls by sticking things together
base_url = "https://bringatrailer.com/listing/"
base_url1 = str_c(base_url, min(model_years):max(model_years))
base_url2 = str_c(base_url1, car_id)
base_url3 = data.frame(rep(base_url2, max_page)) %>% #repeat n times, where n = max_page
  rename(url = 1) %>% 
  arrange(url) #put into data frame so i can sort it properly real quick
base_url4 = base_url3$url

#create list of numbers to attach to each url
numbers_list = rep(1:max_page, model_years_count)

#put urls and numbers together
urls = paste0(base_url4, numbers_list)

#add in extra urls because the first car listed doesn't have a number
extra_urls = str_sub(base_url2, end = -2)

#put extra urls back into url list
urls = unique(c(urls, extra_urls))

#start an empty set
return_list = c()

#for every url, try it, then return result to list
for (url in urls){
  new = GET(url)[1]
  print(new)
  return_list = c(return_list, new)
}

#create two data frames to put together; add a fwdslash since that's what gets returned by the GET function for legit urls
url_a = data.frame(urls) %>% 
  mutate(urls = paste0(urls, "/"))

#make data frame from our list of returned urls
url_b = data.frame(return_list) %>% 
  data.table::transpose() %>% 
  rename(urls = 1) %>% 
  mutate(exists = 1)

#inner join the two lists; what remains had a match for url and actually do exist (legit url)
url_df = url_a %>% 
  inner_join(url_b)

#overwrite urls vector with our urls that actually exist (stored in df)
urls = url_df$urls

# urls = "https://bringatrailer.com/listing/2013-bmw-m3-20/"

counter = 0

#then feed urls into this loop that does the web scraping for each url in the list
for (url in urls) {

#read url
car = read_html(url)

#get title of the listing
title = car %>% 
  html_element("h1") %>% 
  html_text()

#get results of the listing
result = car %>% 
  html_element("span.hide-mobile-inline") %>% 
  html_children() %>% 
  html_text()

#store results of the auction into variables
sold = if_else(result[1] == "Bid To", 0, 1)
price = gsub(result[2], pattern = "[A-z,$]", replacement = "")
date = as.Date(result[4], "%m/%d/%y")

#get the info from the text box on the right
all = car %>% 
  html_element("ul.listing-essentials-items") %>% 
  html_children() %>% 
  html_text()

#store these elements into values
lot = gsub(all[1], pattern = "[A-z #]", replacement = "")
state = na.rm(str_extract(all[3], pattern = c(state.name, state.abb, "Canada")))
# miles = gsub(all[5], pattern = "[A-z, ]", replacement = "")
miles = all[5] #storing it plainly for now since format is inconsistent

#check if car has a reserve, this is always in the title it seems
reserve = as.numeric(!str_detect(title, pattern = "No Reserve"))

#commenters
comments = gsub(car %>% 
  html_elements("h2.comments-title") %>% 
  html_text(), pattern = "[A-z ]", replacement = "")

#ending stats table that appears after photos on an auction that's ended
ending_stats = car %>% 
  html_elements("td.listing-stats-value") %>% 
  html_children() %>% 
  html_text()

#store into value. the position of the end stamp depends upon sale
end_stamp = if_else(sold == 1,
  gsub(ending_stats[3], pattern = "[\t\n]", replacement = ""),
  gsub(ending_stats[2], pattern = "[\t\n]", replacement = ""))

#engagement: viewers and watchers
engagement = car %>% 
  html_elements("td.listing-stats-views") %>% 
  html_children() %>% 
  html_text()

#bids
bids = car %>% 
  html_elements("td.listing-stats-value.number-bids-value") %>% 
  html_text()

#carfax
body_text = car %>% 
  html_nodes("p")

#put words of first paragraph into value, all lowercase
first_paragraph = tolower(body_text[1])

#carfax terms to look for in the first paragraph
carfax_terms = c("accident-free carfax", "clean carfax", "clear carfax")

#figure out from first paragraph if the carfax is clean based on the phrases above
clean_carfax = if_else(sum(as.numeric(str_detect(first_paragraph, pattern = carfax_terms))) > 0, 1, 0)

#put engagement stats into values
views = gsub(engagement[1], pattern = "[A-z, ]", replacement = "")
watchers = gsub(engagement[2], pattern = "[A-z, ]", replacement = "")

#store current car demographics into data frame
new_car = data.frame(url,
                     title,
                     lot,
                     state,
                     miles,
                     price,
                     sold,
                     reserve,
                     clean_carfax,
                     date,
                     end_stamp,
                     bids,
                     comments,
                     views,
                     watchers)

#append current car demographics to main data frame
cars = rbind(cars, new_car)

#need to find a way to bind even with empty data. or check for empty and throw out or don't try to bind it

counter = counter + 1
print(paste("Url added:", url))
print(counter)
}

#take final dataset and start some cleaning, adding new variables
cars1 = cars %>% 
  distinct() %>% 
  mutate(miles = gsub(miles, pattern = "[A-z, ]", replacement = ""),
         across(c(miles, price, sold, reserve, clean_carfax, bids, comments,
                  views, watchers), ~ as.numeric(.x)),
         date = as.Date(date),
         miles = if_else(nchar(miles) == 2,
                         miles * 1000, miles),
         miles = if_else(nchar(miles) == 3,
                         miles * 1000, miles),
         bid_engagement = bids / views * 1000,
         comment_engagement = comments / views * 1000,
         is_weekend = as.numeric(is.weekend(date)),
         am_pm = str_sub(end_stamp, start = -5, end = -4),
         sold_char = if_else(sold == 1, "Sold", "Not Sold"),
         num_date = julian(date),
         post_age = as.numeric(Sys.Date() - date),
         views_normalized = views / post_age) %>% 
  arrange(date) %>% 
  mutate(order = row_number()) %>% 
  arrange(-order) %>% 
  filter(!is.na(price),
         !is.na(date),
         !is.na(miles))

sold_cars = cars1 %>% 
  filter(sold == 1)

states = sold_cars %>% 
  count(state)

#check out simple demographics of data
ggplot(data = states,
       aes(x = state,
           y = n)) +
  geom_col() +
  theme_classic()

#simple line chart of auction results over time
ggplot(data = sold_cars,
       aes(x = date,
           y = price,
           color = miles)) +
  geom_point(size = 3) +
  theme_classic() +
  scale_y_continuous(labels = comma, limits = c(0, max(cars1$price))) +
  theme(legend.title = element_blank()) +
  xlab("Date") +
  ylab("Price") +
  geom_smooth()

#run some simple correlations
cor(sold_cars$price, sold_cars$miles)

#
ggplot(data = sold_cars %>% filter(year(date) >= 2000),
       aes(x = miles,
           y = price,
           color = num_date)) +
  geom_point(size = 3) +
  theme_classic() +
  geom_smooth() +
  scale_y_continuous(labels = comma, limits = c(0, max(cars1$price))) +
  scale_x_continuous(labels = comma) +
  xlab("Miles") +
  ylab("Price") 
  # geom_hline(aes(yintercept = 33000)) +
  # geom_vline(aes(xintercept = 36000))

#quick summary stats
median(sold_cars$price)
median(sold_cars$miles)
median(sold_cars$bids)
median(sold_cars$watchers)
median(sold_cars$views)
median(sold_cars$comments)

sold_cars = sold_cars %>% 
  filter(lot != '54570')

#create linear model for predicting price
model = lm(data = sold_cars, price ~ miles + num_date)
summary(model)

#predicting new car

#since we can't know the views and watchers in advance, we can just assume the median
#can put modifier on that for a more rosy or negative outlook for views and watchers
views_modifier = 1.0
watchers_modifier = 1.0

#input varibables for new car
p_miles = 36000
p_num_date = julian(Sys.Date())
p_views = median(sold_cars$views) * views_modifier
p_watchers = median(sold_cars$watchers) * watchers_modifier
# p_views = 9771
# p_watchers = 619

#put values into df
newdata = data.frame(miles = p_miles,
                     num_date = p_num_date)
                     # views = p_views,
                     # watchers = p_watchers)

#make prediction from model using the new data
prediction = predict.lm(model, newdata = newdata)

prediction

#stop timer
toc()

#analysis ideas
#seller comments
#seller comments ratio to all comments
#some engagement metric
#comment sentiment