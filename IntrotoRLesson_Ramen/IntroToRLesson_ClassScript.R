#Installing and Loading Packages####

#Installing packages with install.packages command

install.packages("tidyverse")
install.packages("tidylog")
install.packages("skimr")
install.packages("visdat")

# Now load the libraries for this R script - you only need to install 
# them once, but every time you write a script, you need to load 
# the libraries into your workspace

library(tidyverse) 
library(tidylog)
library(skimr)
library(visdat)
#note I don't need quotation marks here, but you can add them if you want

#Reading in data using readr####

#Reading in the data using readr, which is part of tidyverse
RawRamen <- readr::read_csv("RawData/RamenRatings.csv") 

#you could also write RawRamen <- read_csv("RawData\RamenRatings.csv").  
#the readr:: portion of the code just references the package I'm pulling the function from
#and is largely unnecessary if you have loaded the package

# Not Working? Did you change your file name? is your folder 
# named RawData? R is case sensitive

###################################################
#Practice 1####
#Try the above commands on RawRamen and CleanRamen

















###################################################
#Looking at Variable Types: Factors#### 

# mtcars is just a built in dataset that comes with R.  You'll see it (along
# with iris and diamonds) referenced a lot in tutorials

str(mtcars) 
#note cylinders is numeric, but we can probably treat it as categorical

mtcars$cyl2 <- as.factor(mtcars$cyl)
str(mtcars) 

#Want to make it numeric again?
mtcars$cyl3 <- as.numeric(mtcars$cyl)
str(mtcars) 

#WHAT HAPPENED?!
#factors have both a numerical code, and a category label. 
#On the backend, r sees levels 1, 2, and 3 with the following character labels:
levels(mtcars$cyl2)

#So to convert back, you have to get a little tricky - first convert to string, then numeric
mtcars$cyl4 <- as.numeric(as.character(mtcars$cyl))
str(mtcars$cyl) #original
str(mtcars$cyl2) # as factor
str(mtcars$cyl3) #from factor straight to numeric
str(mtcars$cyl4) #from factor to string to numeric

#Select and Filter####
RawRamen %>% #this means take our data frame and then do the next line
  select(-review_number) %>% 
  filter(style == "Cup" | style == "Bowl") #| means 'or';

#R is flexible, and there are multiple ways to do the same things
RawRamen %>% 
  select(brand, variety, style, country, stars) %>% 
  filter(style %in% c("Cup", "Bowl")) #c() is just how we make lists of items. Don't forget quotes!

#proof that the above two are the same?

MethodA <- RawRamen %>% #this means take our data frame and then do the next line
  select(-review_number) %>% 
  filter(style == "Cup" | style == "Bowl") #| means 'or';

MethodB <- RawRamen %>% 
  select(brand, variety, style, country, stars) %>% 
  filter(style %in% c("Cup", "Bowl"))

identical(MethodA, MethodB)

#Mutate####
RawRamen %>% 
  mutate(stars = as.numeric(stars), #need to convert stars first
         rating_perc = round( stars/5 * 100,2),
         style = as.factor(style),
         brand = as.factor(brand))

#note tidylog says we introduced 14 NA's when converting stars to numeric.  
#Let's check it out
RawRamen %>% 
  mutate(stars2 = as.numeric(stars)) %>% 
  select(stars, stars2) %>% 
  filter(is.na(stars2)==T) %>% #this is how we search for NA's
  View()

#Summarize, arrange, and group_by####
RawRamen %>% 
  mutate(stars = as.numeric(stars)) %>% #still need to make stars numeric first
  group_by(brand) %>% 
  summarize(AvgStars = mean(stars, na.rm = T)) %>% #na.rm is an optional argument, in this case we do want to remove the NA's
  arrange(desc(AvgStars))

###################################################
#Practice 2 ####
#How many reviews have a rating of <= 1?




#can see at this point that there's 100 rows.  Want to output the exact number?




#How many reviews are there for the US




#Create a column for "Good ramen", which flags ramen >= 4 as "good", and all others as "mediocre". How many reviews are good vs mediocre?




#What country has the most reviews and what's the average review per country?




#Bonus: How many different styles of ramen are there?




###################################################

#ggplot#### 
#This is the same dataset we produced for one of our questions in
#practice 2; we're just saving it to an object

CleanRamen <- RawRamen %>% 
  mutate(stars = as.numeric(stars),
         country = factor(country),) %>% 
  group_by(country) %>% 
  summarize(nReviews = n(),
            avgReview = round(mean(stars, na.rm = T),2)) %>% 
  arrange(desc(nReviews))


CleanRamen %>% #take this dataset
  ggplot(aes(x = country, y = avgReview)) + #create a ggplot setting x to country and y to avgReview
  geom_point()


#Let's make it prettier
CleanRamen %>% #take this dataset
  ggplot(aes(x = reorder(country, avgReview), y = avgReview)) +
  geom_point() +
  labs(title = "Average Ramen Ratings by Country",
       subtitle = paste0("Total Reviews: ", nrow(RawRamen)),
       x = "Countries",
       y = "Average Rating (0-5)", 
       caption = "Source: www.theramenrater.com/resources-2/the-list/")+
  coord_flip()

#let's make it fancy
Asia <- c("Cambodia", "Malaysia", "Indonesia", "Singapore", "Sarawak", "Myanmar", "Japan",
          "South Korea", "Hong Kong", "Taiwan", "Bangladesh", "Philippines", "China", 
          "Thailand", "India", "Pakistan")
CleanRamen %>% #take this dataset
  mutate(isAsian = ifelse(country %in% Asia ,"Asia", "Not Asia")) %>% 
  ggplot(aes(x = reorder(country, avgReview), y = avgReview, size = nReviews, color = isAsian)) +
  geom_point() +
  labs(title = "Average Ramen Ratings by Country",
       subtitle = paste0("Total Reviews: ", nrow(RawRamen)),
       x = "Countries",
       y = "Average Rating (0-5)", 
       caption = "Source: www.theramenrater.com/resources-2/the-list/")+
  coord_flip() 



#Getting nuts 
#You may not be able to run the first command here, which means you cant run
#this entire section. You can thank CBH firewalls for that

devtools::install_github("dill/emoGG") 
library(emoGG)

RamenClean<- RawRamen %>% 
  mutate(stars = as.numeric(stars)) %>% 
  filter(is.na(stars) == F, is.na(style) == F) %>% 
  mutate(style = factor(style),
         country = factor(country))

#which countries produce the highest rated ramen
RamenClean %>% 
  group_by(country) %>% 
  summarize(AvgRating = mean(stars),
            NReviews = n()) %>% 
  ggplot(aes(y = reorder(country, AvgRating),x = AvgRating)) +
  geom_segment(aes(x = 0, xend = AvgRating, yend = country), color = "#e0dabc", size = 1.5) +
  geom_emoji(emoji = "1f365", size = .03) +
  geom_vline(xintercept = mean(RamenClean$stars), color = "red")+
  geom_text(aes(label = NReviews), size = 3, hjust = -.5) +
  labs(title = "Which Countries Produce the Best Ramen?",
       x = "Average Rating Across All Products (0-5)",
       caption = "Numbers represent total N of reviews per country\nData: TheRamenRater.com|Viz: @phillynerd") +
  scale_x_continuous(limits = c(0,5)) +
  add_emoji(emoji = "1f35c") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "#9b9999"),
        axis.title.y = element_blank() ) +
  annotate(geom = "text", 
           x = mean(RamenClean$stars), y = 0, 
           label = paste0("Overall Avg: ", round(mean(RamenClean$stars),1)),
           angle = 90,
           hjust = -.2, vjust = -.5, size = 4)


###################################################
#Practice 3####

#Base code from slide 33
Asia <- c("Cambodia", "Malaysia", "Indonesia", "Singapore", "Sarawak", "Myanmar", "Japan",
          "South Korea", "Hong Kong", "Taiwan", "Bangladesh", "Philippines", "China", 
          "Thailand", "India", "Pakistan")

CleanRamen %>% #take this dataset
  mutate(isAsian = ifelse(country %in% Asia ,"Asia", "Not Asia")) %>% 
  ggplot(aes(x = reorder(country, avgReview), y = avgReview, size = nReviews, color = isAsian)) +
  geom_point() +
  labs(title = "Average Ramen Ratings by Country",
       subtitle = paste0("Total Reviews: ", nrow(RawRamen)),
       x = "Countries",
       y = "Average Rating (0-5)", 
       caption = "Source: www.theramenrater.com/resources-2/the-list/")+
  coord_flip()

# Change "color" to "fill" in aes then replace geom_point with geom_col. What happens?


# what happens if you use geom_bar()? try geom_bar(stat = "identity")


# create a visual that explores how ramen ratings vary by style (you'll need to
# manipulate RawRamen first). Try different ways to visualize the different
# styles (color, fill, facet_wrap, different geoms, etc)


###################################################



