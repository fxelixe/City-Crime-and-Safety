# CITY CRIME AND SAFETY
# This code is used to preprocess the raw data 
# and cluster the Primary Descriptions into three category:
# High crime level, medium crime level, and low crime level
# according to their frequency and distribution across the Beat

# load the library
library(dplyr)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)
library(lubridate)
library(fpc)
library(reshape)

# set the working directory
getwd()
setwd("C:/Users/user/Downloads/PEMODELAN 1/crime codes")

# load the data 
crime <- read.csv("Crime_Data.csv", sep = ";")
crime <- crime[,2:8]
crime <- crime[,!names(crime) %in% c("SECONDARY.DESCRIPTION")]
str(crime)

# pre-processing data
crime$DATE..OF.OCCURRENCE <- as.Date(crime$DATE..OF.OCCURRENCE, "%d/%m/%Y")
crime$BEAT <- as.character(crime$BEAT)

# take a look at the data
#summary(crime)
#primary description --> 28 levels with 1 ""
#secondary description --> 214 levels with 1 ""

# removing data with null value
crime <- na.omit(crime)

# drop unused levels in data frame
crime <- droplevels(crime)

# grouping the data by crime.primary and beat
#crime.primary <- crime %>% 
#                  group_by(BEAT, ARREST) %>% 
#                  count(PRIMARY.DESCRIPTION) %>% 
#                  as.data.frame()

# creating a model matrix
#crime.primary.matrix <- cbind(crime.primary$BEAT, model.matrix(BEAT~., crime.primary)[,-1]) %>% 
#                          as.data.frame()

# rename the V1 into beat
#names(crime.primary.matrix)[1]<-"BEAT"

# restructure data frame
#crime.primary.matrix$BEAT <- as.character(crime.primary.matrix$BEAT)
#crime.primary.matrix$n <- as.numeric(crime.primary.matrix$n)

# create second data set
crime.second <- crime %>% 
                  group_by(PRIMARY.DESCRIPTION) %>% 
                    count(BEAT, ARREST)

crime.second.matrix <- cbind(as.character(crime.second$PRIMARY.DESCRIPTION), model.matrix(PRIMARY.DESCRIPTION~., crime.second)[,-1]) %>% 
                          as.data.frame()

crime.second.matrix[,2:276] <- lapply(crime.second.matrix[,2:276], as.character)
crime.second.matrix[,2:276] <- lapply(crime.second.matrix[,2:276], as.numeric)
  
crime.second.matrix.a <- crime.second.matrix %>% 
                          group_by(V1) %>% 
                          summarise_all(funs(sum))

names(crime.second.matrix.a)[1]<-"PRIMARY.DESCRIPTION"
crime.second.matrix.a$ARREST_N <- crime.second.matrix.a$n - crime.second.matrix.a$ARRESTY

crime.primary.matrix <- crime.second.matrix.a[-275:-276]
# normalize the scale
crime.z <- crime.primary.matrix %>%
            mutate_if(is.numeric, scale)

# making the model
set.seed(111)

# k-means with 3 clusters
crime.km <- kmeans(crime.z[,-1], 3)

# check at the cluster means
print(crime.km)

# Each centroid from the clusters show if the cluster is high, medium, or low crime level

# inputting the cluster into master data
crime.second.matrix.a$cluster <- as.factor(crime.km$cluster)

# rename level
levels(crime.second.matrix.a$cluster)[levels(crime.second.matrix.a$cluster) == "1"] <- "High"
levels(crime.second.matrix.a$cluster)[levels(crime.second.matrix.a$cluster) == "2"] <- "Medium"
levels(crime.second.matrix.a$cluster)[levels(crime.second.matrix.a$cluster) == "3"] <- "Low"

# To download cluster result as csv
save_csv <- cbind(crime.second.matrix.a[1],crime.second.matrix.a[278])
save_csv <- data.frame(save_csv)
write.csv(save_csv,'cluster.csv')


# Visualization of clustering
gg <- fviz_cluster(crime.km, crime.primary.matrix[,-1])
gg + geom_text(label=crime.primary.matrix$PRIMARY.DESCRIPTION) +
  labs(title = "Clustering's Result Visualization")
