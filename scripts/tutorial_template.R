# Tutorial: My First Day in R
# Made by: Zuzi Koscikova
# Author: name
# Contact: 
# Date: 


# 1. Install and load packages ---- 

install.packages("tidyverse")
library(tidyverse)


# 2. Starting with the code ----

## Loading the data ---- 
# Check your working directory
getwd()

# specify your filepath
data <- read.csv("data/LPIdata.csv")


## Checking the data ----
summary(data)
glimpse(data)


## Long format ----
# use pivot.longer(data, cols= , names_to= , values_to= )


## Deleting the X in front of the year values ----
# use mutate and parse_number


## Using pipes----
# join previous code lines into one code by pipe 
# pipe: %>% (Ctrl+Shift+M)

 

# checking the specific column value type
str(data_long$LPI)    
str(data_long$year)
# Check what different values we have in specific columns
unique(data_long$Entity) 
unique(data_long$year)


## Deleting unnecessary data using pipes ----
# delete unnesessary columns or values by filter and != operator or select with minus operator


# Connect all of the previous codes by pipes 








# 3. Plotting ---- 

## Histogram ----
# base R

# specify which data you need to use and which column by $ sign
hist(data) 
# specify what line you would like to add
abline()

# Adjust your histagram by adding titles and labels 
# use main, xlab, ylab, col...




# with ggplot2
# specify name of plot, data, aes functions
name <- ggplot(data, (aes()) + geom_histogram())



# beautify your histogram by adding geom_vline, colour, theme, title and labels...




# BARPLOT ----
# add a new column to your data - average of LPI using group_by and mutate



# specify name, data, aesthetics
(name <- ggplot(data, aes()) +
    geom_bar(position = position_dodge(), stat = "identity"))

 
# beautify the plot by labels, title, legend, theme... 
   






# BOXPLOT ----

# adjust name, data and aes
(name <- ggplot(data, aes()) + geom_boxplot())  


# Beautify by theme, title, labels, legend...





# SCATTERPLOT ----
# specify name, data and aes
(name <- ggplot(data, aes()) + geom_point())

# Beautify by adding colour, titles, labels, legend, theme...




# 4. Saving the plots ----

## Saving plots by their name ----
# change filepath according to yours
ggsave("figures/histogram.jpg", plot=LPI_hist)


## Saving plots right below their code with ggsave ----
ggsave("figures/boxplot1.jpg", width=7 , height= 3.5)    

## Saving by png() and dev.off() ----
png("figures/histogram1.png")    
dev.off()


