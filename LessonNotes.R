#Sample code for demos

#to install packages if necessary
install.packages("ggmap")
install.packages("tidyverse")
install.packages("skimr")

#the library function loads the packages into your workspace
library(tidyverse) #this is our go-to package for data cleaning, manipulation, exploration, and visualization
library(ggmap) #our mapping tool for basic maps (can use leaflet for interactive/slidy maps)
library(skimr) #for snapshot summary of whats in your dataset

#pull in base philly map
philly <- get_map (location = c(lon = -75.1652, lat = 40.01), 
                  zoom = 11, source = "stamen", maptype = "toner-lite") #try roadmap and watercolor
#checking to see if map uploaded correctly
ggmap(philly)

#pulling in locations of  SA and Bup docs
bup <- read_csv("SAMHSA_BUP.csv")
skim(bup) #to make sure the file imported ok

bup <- bup %>% 
  select(name1, city, state, type_facility, latitude, longitude) #just taking columns I need

sa <- read_csv("SAMHSA_SA_Providers.csv")
skim(sa) #checking to make sure file imported ok
colnames(sa) #too many cols for skim

#merging the two datasets into a single data frame
PhlTx <- sa %>% 
  select(name1, city, state, type_facility, latitude, longitude) %>% 
  bind_rows(bup) %>% #just adds in rows from bup below rows of sa
  #filter(city == "Philadelphia") #limiting to philadelphia
  mutate(IsPhilly = ifelse(city == "Philadelphia", "Yes", "No"))

#and creating our map
ggmap(philly, 
      base_layer = ggplot(aes(x = longitude, y = latitude), data = PhlTx)) +
  geom_point(aes(color = type_facility, shape = IsPhilly), alpha = .6)

###basic example of tidyverse
sa %>% 
  group_by(city) %>% 
  summarize(Nproviders = n()) %>% 
  arrange(desc(Nproviders)) %>% 
  mutate(ProvRank = rank(-Nproviders)) %>% 
  filter(ProvRank <= 10) %>% 
  ggplot(aes(x = reorder(city, -Nproviders), y = Nproviders)) +
  geom_bar(stat = "identity", fill = "orange")
