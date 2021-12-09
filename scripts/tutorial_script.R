# Data Science: Tutorial                       # note down information about course/Coding club and tutorial
# My first day in RStudio
# Zuzi Koscikova, zuzkakoscikova@gmail.com     # write down your name and contact
# 6 December 2021                              # date should not be in the format easily switched with European/American one


#install.packages("prettydoc")
library(prettydoc) 
#install.packages("remotes")
#remotes::install_github("rlesur/klippy")

  
  
# 1. Install packages ----    

# 4 dash lines after comment create the title
## Install packages ----
### Install packages ----
#### Install packages ----
##### Install Packages ----
###### Install packages ---- 
# you can add up to 6 subtitles adding more hashtags
# each will be moved to the more right 

# Install packages ----
# In this tutorial we will need only tidyverse package which include many others that are used in Data Science

# "install.packages" is a function we need to use for installing the required package. 
# in the brackets is name of the package in the quotations ""
## NOTE: DELETE THE HASHTAGS IN FRONT OF THE FOLLOWING CODE
#install.packages("tidyverse")  # package for data manipulation includes tidyr, dplyr, ggplot2, readr...

# Load the packages ----
# Load the packages using "library"
library(tidyverse)

# (Note: Normally it should work without using "" quotations when loading the packages
# however, if it is not working, try with them (this is working for me))


# 2. Starting with the code ----
# usually there will be some data provided in csv format you will need to use. 
# when doing tutorials of coding club tutorials, from the github repository you can download ZIP folder. 
# Extract ZIP folder to your course repository folder in the computer. 

## Loading the data ---- 

getwd()
data <- read.csv("data/LPIdata.csv")
# data: name of your dataframe
# <- : assign symbol 
# read.csv: function used for loading the data in csv format
# ("data/LPIdata.csv"): full filepath and name of the file (with its format) at the end 

## Checking the data ----

summary(data)
glimpse(data)

## Long format ----
# We will transform the wide dataframe into long format by moving all the year columns into one, where they will be below each other. 
# We need to check in the dataframe to see which columns are they in 

data_long <- pivot_longer(data, cols=3:49, names_to = "year", values_to = "LPI")

# data_long: name of new dataframe
# pivot_longer: operator we are using for transformation of the data
# in the bracket: (data: original data, 
#                  cols=: original columns which needed to be transformed, 
#                  names_to=: name of the new column, where all the names from original columns (so all #years) will be moved, 
#                  values_to=: name of the new column where all the values from original columns (so all the LPI values) will be moved)

## Deleting the X in front of the year values ----
data_long <- mutate(data_long, year = parse_number(year))

# data_long: name of the new dataframe
# mutate: operator by which we will create column "year"  
# in the brackets: (data, column = )
#        - data_long: We have the same, because we do not need to create another dataframe, we want only change our "data_long" one. 
#       - year: name of (new) column 
#       - parse_number: operator that will extract only numerical values from the (original) column (year)

## Using pipes----

data_long <- data %>% 
  pivot_longer(cols = 3:49,            # long format
               names_to = "year",
               values_to = "LPI") %>% 
  mutate (year = parse_number(year))   # deleting X in front of the years 

str(data_long$LPI)     # checking the specific column value type
str(data_long$year)


# Check what different values we have in specific columns
unique(data_long$Entity) # use unique for checking the values
unique(data_long$year)

## Deleting unnecessary data using pipes ----

data_long <- data_long %>% 
  filter(Entity != "Reptiles") %>% 
  filter(Entity != "World") %>%         # deleting unnecessary data from Entity column
  select(-c("Code"))                    # deleting useless column Code


# We also can connect all of the previous changes into one pipe
data_long <- data %>% 
  pivot_longer(cols = 3:49,               # changing to long format
               names_to = "year",
               values_to = "LPI") %>% 
  mutate (year = parse_number(year)) %>%  # deleting X in front of the years
  filter(Entity != "Reptiles") %>%        # deleting Reptile data
  filter(Entity != "World") %>%           # deleting World data
  select(-c("Code"))                      # deleting column Code


# 3. Plotting ---- 

## Histogram of the LPI 
# with base R
hist(data_long$LPI)                           # histogram of the LPI column from data_long dataset
hist(data_long$LPI, breaks = 30)              # try change number for breaks, it will change the distribution of bars
# with abline we can add horizontal (with h) or vertical (with v) lines on the plot. 
# we can plot median, mean or specific number 
abline(v=mean(data_long$LPI), col = "red")    # we can also add line of mean, median or other specific number, we can change colour
abline(h=10, col = "blue")


# Beautifying of base R histogram
par(cex.main = 1, cex.lab=0.9, cex.axis = 0.7)      
# size of the title (cex.main) and size of axis labels (cex.lab), size of axis values (cex.axis)
hist(data_long$LPI,                         # histogram of the LPI column from data_long dataset
     main= "LPI distribution (1970-2016)",  # plot title
     xlab= "LPI",                           # x-axis label
     ylab= "Frequency",                     # y-axis label
     col= "bisque",                         # changing colour
     breaks = 30)                           # specifying bars distribution
abline(v=mean(data_long$LPI), col="red3")   # adding mean line in red colour


# with ggplot2
# creating new plot "LPI_hist" by ggplot function, from the "data_long" data, where we are changing x axis name to LPI
# by geom_histogram we are specifying the type of the plot we want
(LPI_hist <- ggplot(data_long, aes(x = LPI)) +     
    geom_histogram())


# Beautifying
(LPI_hist <- ggplot(data_long, aes(x = LPI)) +                
    geom_histogram(binwidth = 5, colour = "dark green", fill = "light blue") +    # Changing the binwidth and colours of the bars.   
    geom_vline(aes(xintercept = mean(LPI)),                                # Adding a line for mean abundance
               colour = "orange", linetype = "dashed", size=1) +           # Changing the look of the line
    theme_classic() +                                         # Changing the theme to get rid of the grey background
    ylab("Count\n") +                                         # Changing the text of the y and x axis label
    xlab("\nLPI")  +                                          # \n adds a blank line between axis and text
    ggtitle("LPI distribution (1970-2016)") +                 # Adding plot title
    theme(axis.text = element_text(size = 12),                # Changing font size of axis labels and title
          plot.title = element_text(hjust=0.5, size = 12),         # hjust = moves to the right/left, vjust = moves up/down
          axis.title.x = element_text(size = 12, face = "plain"),  # face="plain" is the default, you can change it to italic, bold, etc. 
          axis.title.y = element_text(size = 12, face= "plain"),
          panel.grid = element_blank(),                       # Removing the grey grid lines
          plot.margin = unit(c(1,1,1,1), units = "cm")))      # adding a 1cm margin around the plot

# BARPLOT ----
# barplot of average LPI according to entities 

# Adding a new column to our data - average of LPI 
data_long <- data_long %>%    
  group_by(Entity) %>%            # grouping the data by Entity (does similar thing as having data in wide format)
  mutate(LPIaverage = mean(LPI))  # "mutate" creates new column with the name "LPIaverage" which values will be mean of LPI column

# creating the barplot called "LPI_aver_barplot" from "data_long" data. In our aesthetic we need to specify x and y axis (the names of the specific column used). 
# "fill" in aes tells R to colour the bars according to different Entities. 
(LPI_aver_barplot <- ggplot(data_long, aes(x = Entity, y=LPIaverage, fill=Entity))
  + geom_bar(position = position_dodge(), stat = "identity"))

# Beautifying
(LPI_aver_barplot <- ggplot(data_long, aes(x = Entity, y = LPIaverage, fill=Entity)) +
    geom_bar(position = position_dodge(), stat = "identity") +   # we need to use stat = "identity" when using "geom_bar" and own y-axis values 
    theme_classic() +
    ggtitle("Average LPI (1970-2016)") +                   # plot title
    ylab("LPI") +                                          # y-axis title
    xlab("Entity")  +                                      # x-axis title
    scale_y_continuous(expand = c(0,0)) +                  # deleting gap between x-axis and plot
    theme(axis.text.x = element_blank(),                   # deleting names of Entities form x-axis
          axis.text.y = element_text(size = 10),           # changing size of y-axis labels
          axis.title = element_text(size = 12, face = "plain"),   # adjusting axis titles size and format
          plot.title = element_text(size = 12, hjust=0.5),        # adjusting plot title size and position
          panel.grid = element_blank(),                           # deleting grid
          legend.text = element_text(size = 10, face = "italic"), # setting the font for the legend text
          legend.title = element_blank(),                         # removing the legend title
          legend.position = "right"))                             # setting legend position 


# BOXPLOT ----

# creating plot called "LPI_boxplot" by ggplot from "data_long" data. The aesthetic includes x axis and y axis specification. 
# "geom_boxplot() specifyies type of the plot
(LPI_boxplot <- ggplot(data_long, aes(Entity, LPI)) + geom_boxplot())  


# Beautifying
(LPI_boxplot <- ggplot(data_long, aes(Entity, LPI)) +             
    geom_boxplot(aes(fill = Entity)) +                             # adding colour of the boxes by Entity (fill=Entity)
    theme_classic() +                                              # changing theme
    ggtitle("LPI variation across location (1970-2016)")  +        # plot title
    ylab("LPI") +                                                  # y-axis title
    xlab("\nLocation")  +                                          # x-axis title
    theme(axis.text.x = element_blank(),                           # deleting x-axis labels (because we have it in the legend)
          axis.title = element_text(size = 14, face = "plain"),    # adjusting axis titles                    
          panel.grid = element_blank(),                            # removing the background grid lines  
          plot.title = element_text(size = 12, hjust=0.5),         # adjusting plot title   
          plot.margin = unit(c(1,1,1,1), units = , "cm"),          # adding a 1cm margin around the plot
          legend.text = element_text(size = 10, face = "italic"),  # setting the font for the legend text
          legend.title = element_blank(),                          # removing the legend title
          legend.position = "right"))                              # setting legend position 


# SCATTERPLOT
# scatterplot of LPI for each location

# creating a plot called "LPI_scatter" by ggplot from "data_long" data. We are specifying columns for x and y axis and the colour difference based on location
# geom_point() specifies type of the plot
(LPI_scatter <- ggplot (data_long, aes(x = year, y = LPI, colour = Entity)) + geom_point())

# Beautifying
(LPI_scatter <- ggplot(data_long, aes (x = year, y = LPI, colour =Entity)) +
    geom_point(size = 1.2) +                        # changing point size
    geom_smooth(method = "lm", se=FALSE) +          # adding linear model fit (method=lm), deleting standard error ribbons (se=false)
    theme_classic() +                                             # changing theme 
    ggtitle("Yearly LPI in different locations (1970-2016)")  +   # plot title
    ylab("LPI\n") +                                               # y-axis title
    xlab("\nYear")  +                                             # x-axis title
    theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # adjusting x-axis labels (size, angle, vjust and hjust - vertical and horizontal position)
          axis.text.y = element_text(size = 10),                    # adjusting y-axis labels
          axis.title = element_text(size = 12, face = "plain"),     # adjusting axes titles     
          panel.grid = element_blank(),                             # removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),           # adding a 1cm margin around the plot
          plot.title = element_text(size = 12, hjust=0.3),          # adjusting plot title 
          legend.text = element_text(size = 10, face = "italic"),   # setting the font for the legend text
          legend.title = element_blank(),                           # removing the legend title
          legend.position = "right"))                               # setting legend position 

# 4. Saving the plots ----

getwd()  # to remember your working directory to be able to specify where the plot should be saved 
## Saving plots by their name ----
ggsave("figures/histogram.jpg", plot=LPI_hist)
ggsave("figures/barplot.jpg", plot=LPI_aver_barplot, width=7) 
ggsave("figures/boxplot.jpg", plot=LPI_boxplot, width=7)
ggsave("figures/scatterplot.jpg", plot=LPI_scatter, width=7)

## Saving plots right below their code with ggsave ----
# Boxplot 
(LPI_boxplot <- ggplot(data_long, aes(Entity, LPI)) +             
    geom_boxplot(aes(fill = Entity)) +                            
    theme_classic() +                                              
    ggtitle("LPI variation across location (1970-2016)")  +       
    ylab("LPI") +                                                  
    xlab("\nLocation")  +                                            
    theme(axis.text.x = element_blank(),                           
          axis.title = element_text(size = 14, face = "plain"),                      
          panel.grid = element_blank(),                              
          plot.title = element_text(size = 12, hjust=0.5),           
          plot.margin = unit(c(1,1,1,1), units = , "cm"),          
          legend.text = element_text(size = 10, face = "italic"),  
          legend.title = element_blank(),                          
          legend.position = "right")) 
ggsave("figures/boxplot1.jpg", width=7, height= 3.5)     # adding ggsave code with specified dimensions and filepath + name of the figure

## Saving by png() and dev.off() ----
# this is used mostly for plots not made by ggplot2, but works for any. 
# write png("filepath/figure.png") before the figure code. 
# write dev.off() right after the code

# saving histogram made in base R
png("figures/histogram1.png")    # specify filepath where figure will be saved in same way as for ggplot2
par(cex.main = 1, cex.lab=0.9, cex.axis = 0.7)      
hist(data_long$LPI,                         
     main= "LPI distribution (1970-2016)",  
     xlab= "LPI",                          
     ylab= "Frequency",                     
     col= "bisque",                         
     breaks = 30)                           
abline(v=mean(data_long$LPI), col="red3") 
dev.off()

# saving boxplot made by ggplot2
png("figures/boxplot2.png", width=750)
(LPI_boxplot <- ggplot(data_long, aes(Entity, LPI)) +             
    geom_boxplot(aes(fill = Entity)) +                            
    theme_classic() +                                              
    ggtitle("LPI variation across location (1970-2016)")  +       
    ylab("LPI") +                                                  
    xlab("\nLocation")  +                                            
    theme(axis.text.x = element_blank(),                           
          axis.title = element_text(size = 14, face = "plain"),                      
          panel.grid = element_blank(),                              
          plot.title = element_text(size = 12, hjust=0.5),           
          plot.margin = unit(c(1,1,1,1), units = , "cm"),          
          legend.text = element_text(size = 10, face = "italic"),  
          legend.title = element_blank(),                          
          legend.position = "right")) 
dev.off()

