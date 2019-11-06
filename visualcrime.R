# VISUAL CRIME
# This code is used to create plots for visualizing the raw data from Crime_Data.csv

# load the library
library(dplyr)
library(tidyr)
library(ggmosaic)
library(ggplot2)
library(vcd)
library(lubridate)

# load working directory
getwd()
setwd("directory")

# load the data 
crime <- read.csv("Crime_Data.csv")
crime <- crime[,2:8]
crime <- crime[,!names(crime) %in% c("SECONDARY.DESCRIPTION")]
str(crime)

# pre-processing data
crime$DATE..OF.OCCURRENCE <- as.Date(crime$DATE..OF.OCCURRENCE, "%d/%m/%Y")
crime$BEAT <- as.factor(crime$BEAT)

# take a look at the data
summary(crime)

#primary description --> 28 levels with 1 ""
#secondary description --> 214 levels with 1 ""

# removing data with null value
crime <- na.omit(crime)

# drop unused levels in data frame
crime <- droplevels(crime)

# visualization by frequency table
value_freq <-  
  crime %>% 
  select_if(is.factor) %>% 
  select_if(function(x) !is.ordered(x)) %>% 
  gather("var", "value") %>% 
  group_by(var) %>% 
  count(var, value) %>%
  mutate(prop = prop.table(n)) %>% 
  filter(prop > .02)

plot_freq <-
  ggplot(data = value_freq,
         aes(x = reorder(stringr::str_wrap(value, 20), prop),
             y = prop)) + ggtitle("Tabel Frekuensi")+
  geom_bar(stat = "identity", fill = "tomato3") +
  coord_flip() +
  facet_wrap(~var, ncol = 3, scales = "free") +
  ggthemes::theme_fivethirtyeight()

plot_freq

# Mosaic Plot: Violent Crime & Arrests

mosaic_data <- 
  crime %>% 
  dplyr::select(c(PRIMARY.DESCRIPTION, ARREST)) %>% 
  mutate_if(is.factor, as.character) %>% 
  table()

vcd::mosaic(mosaic_data, shade = F, 
            cexRow = 0.1,
            cexCol = 0.1,
            margins =c(0,5),
            main="Mosaic Plot Primary Description terhadap Arrest",
            xlab="PRIMARY.DESCRIPTION", rot_labels=c(0,90,0,0))

mosaicplot(~ PRIMARY.DESCRIPTION + ARREST, data = crime, color = TRUE)

plot2<- ggplot(data = crime) +
  geom_mosaic(aes(x =product(PRIMARY.DESCRIPTION), fill=ARREST), na.rm=TRUE) +
  labs(x="Primary Description ", y="Arrest", title='Plot Mosaic Primary Description terhadap Arrest') +
  coord_flip()

plot2

# Facet plot
facet_plot <- ggplot(data = crime, aes(x=PRIMARY.DESCRIPTION, y=ARREST)) + 
  geom_col(aes(fill=DOMESTIC), position = "dodge") +
  facet_wrap(~DOMESTIC) +
  coord_flip() +
  theme_minimal()

facet_plot 

# Plot location and date of occurence
ggplot(crime, aes(x=LOCATION.DESCRIPTION , y=DATE..OF.OCCURRENCE))+
  geom_col()+
  coord_flip()
  )

# Heatmap plot
heatmap_data <- 
  crime %>% 
  dplyr::select(PRIMARY.DESCRIPTION,BEAT) %>% 
  table()

heatmap_data <- data.frame(heatmap_data)
heatmap_plot <- ggplot(heatmap_data, aes(BEAT, PRIMARY.DESCRIPTION)) + geom_tile(aes(fill = Freq)) +
  scale_fill_gradient(low = "red", high = "yellow")

print(heatmap_plot + labs(title= "Heatmap Primary Description terhadap Beat",
                      y="Primary Description", x = "Beat (tanpa digit terakhir)"))
