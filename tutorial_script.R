# Data Science: Tutorial                       # note down information about course/Coding club and tutorial
# My first day in RStudio
# Zuzi Koscikova, zuzkakoscikova@gmail.com     # write down your name and contact
# 6 December 2021                              # date should not be in the format easily switched with European/American one


# Install packages ----    
# 4 dash lines after comment create the title
## Install packages ----
### Install packages ----
#### Install packages ----
##### Install Packages ----
###### Install packages ---- 
# you can add up to 6 subtitles adding more hashtags
# each will be moved to the more right 

# Install packages ----
# Here are the most used R packages you will need in Data Science
# First, install packages 
# install.packages is a function we need to use for installing the required package. 
# in the brackets is name of the package in the quotations ""
install.packages("tidyverse") # package for data manipulation includes tidyr, dplyr, ggplot2, readr, ..
install.packages("ggthemes")     # extra themes, scales and geoms for "ggplot2"

# Load the packages ----
# Load the packages using "library"
# NOTE: Normally it should work without using "" quotations when loading the packages
# however, if it is not working, try with them (this is working for me)
library(tidyverse)
library(ggthemes)

#library(gridExtra)
#library(viridis) # will load colourblind friendly color palettes
#library(janitor) # will load simple functions to clean data sets

# Starting with the code ----
# usually there will be some data provided in csv format you will need to use. 
# when doing tutorials of coding club tutorials, from the github repository you can download ZIP folder. 
# Extract ZIP folder to your course repository folder in the computer. 
# 
getwd()
data <- read.csv("My Tutorial/LPIdata.csv")

# checking the data

summary(data)
glimpse(data)

# long format 
# first by one with description and then with pipes all together

data_long <- data %>% 
  pivot_longer(cols = 3:49,            # long format
               names_to = "year",
               values_to = "LPI") %>% 
  mutate (year = parse_number(year))   # deleting X

str(data_long$LPI)
str(data_long$year)

# removing code and reptiles and world , cos we wont use it and we want other things

data_long <- data_long %>% 
  filter(Entity != "Reptiles") %>% 
  filter(Entity != "World") %>%         # deleting useless 
  select(-c("Code"))                    # deleting useless column 

# check what we have
unique(data_long$Entity)
unique(data_long$year)


# this only in text as a additional function
#data_wide<- spread(data, Year, Living.Planet.Index)


# Plotting graphs ----

# histogram of the LPI 
# with base R
hist(data_long$LPI)                           #histogram of the LPI column from data_long dataset
hist(data_long$LPI, breaks = 30)              # try change number for breaks, it will change the distribution of bars
abline(v=mean(data_long$LPI), col = "blue")    # we can also add line of mean, you can change colour to other such as "green", "blue", "orange"...any.. here is the link for different colours for base R
# you can also change v (value) for median or 
abline(h=10, col = "red")
#with abline we can add horizontal (with h) or vertical (with v) lines on the plot. 
# we can specified median, mean or specific number, it also can be regression or 

# we can also change colours 
par(cex.main = 1, cex.lab=0.9, cex.axis = 0.7)      # size of the title (cex.main) and size of axis labels (cex.lab), size of axis values (cex.axis)
hist(data_long$LPI, 
     main= "Distribution of LPI over years 1970-2016 in different entities", 
     xlab= "LPI",
     ylab= "Frequency", 
     col= "bisque",
     breaks = 30)
abline(v=mean(data_long$LPI), col="red3")


# with ggplot2
(LPI_hist <- ggplot(data_long, aes(x = LPI)) +
    geom_histogram())

# beautifying
(LPI_hist <- ggplot(data_long, aes(x = LPI)) +                
    geom_histogram(binwidth = 5, colour = "dark green", fill = "light blue") +    # Changing the binwidth and colours
    geom_vline(aes(xintercept = mean(LPI)),                       # Adding a line for mean abundance
               colour = "orange", linetype = "dashed", size=1) +           # Changing the look of the line
    theme_minimal() +                                                        # Changing the theme to get rid of the grey background
    ylab("Count\n") +                                                   # Changing the text of the y axis label
    xlab("\nLPI")  +
    ggtitle("Distribution of LPI over years 1970-2016 in different entities") +     # \n adds a blank line between axis and text
    theme(axis.text = element_text(size = 12),  # Changing font size of axis labels and title
          plot.title = element_text(hjust=1, size = 1),
          axis.title.x = element_text(size = 12, face = "plain"),       # face="plain" is the default, you can change it to italic, bold, etc. 
          axis.title.y = element_text(size = 12, face= "plain"),
          panel.grid = element_blank(),  
          plot.margin = unit(c(1,1,1,1), units = "cm")))                                # Removing the grey grid lines

# barplot for Latin America and the Caribbean
# for this, we need to first, filter the data
LatinAmerica <- data_long %>% 
  filter(Entity %in%  "Latin America and the Caribbean")

#ggplot2
(LatinAmerica_barplot <- ggplot(LatinAmerica, aes(x = year, y = LPI)) +
    geom_bar(position = position_dodge(), stat = "identity", colour = "black", fill = "palegreen4", size=0.1 ) +
    theme_classic() +
    ggtitle("LPI values for Latin America and the Caribbean from 1970-2016") +
    scale_x_continuous(breaks = round(seq(min(LatinAmerica$year), max(LatinAmerica$year), by = 4),1), expand = c(0, 0)) +
    scale_y_continuous(expand=c(0,0)) +
    ylab("LPI\n") +                             
    xlab("Years")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1), # Angled labels, so text doesn't overlap
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12, face = "plain"), 
          plot.title = element_text(size = 12, hjust=0.5),
          panel.grid = element_blank()))


# http://sape.inf.usi.ch/quick-reference/ggplot2/colour link fro colours for ggplot

#boxplot
(LPI_boxplot <- ggplot(data_long, aes(Entity, LPI)) + geom_boxplot())

(PLI_boxplot <- ggplot(data_long, aes(Entity, LPI)) + 
    geom_boxplot(aes(fill = Entity)) +
    theme_bw() +
    ggtitle("LPI values for various Entity from 1970-2016")  +        # Adding custom colours for lines and points
    ylab("LPI\n") +                             
    xlab("\nCountry")  +
    theme(axis.text.x = element_text(size = 12, angle = 15 ,vjust = 1, hjust = 1),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),                                 # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm")))              # Adding a margin


(PLI_boxplot <- ggplot(data_long, aes(Entity, LPI)) + 
    geom_boxplot(aes(fill = Entity)) +
    theme_bw() +
    ggtitle("LPI values for various Entity from 1970-2016")  +        # Adding custom colours for lines and points
    ylab("LPI\n") +                             
    xlab("\nCountry")  +
    theme(axis.text.x = element_blank(),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),                                 # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),               # Adding a margin
          legend.text = element_text(size = 10, face = "italic"),         # Setting the font for the legend text
          legend.title = element_blank(),                                 # Removing the legend title
          legend.position = "right"))                                # Setting legend position - 0 is left/bottom, 1 is top/right


# scatterplot for each country


# ggplot2

(LPI_scatter <- ggplot (data_long, aes(x = year, y = LPI, colour = Entity)) +  # linking colour to a factor inside aes() ensures that the points' colour will vary according to the factor levels
    geom_point())

(LPI_scatter <- ggplot(data_long, aes (x = year, y = LPI, colour =Entity)) +
    geom_point(size = 1.5) +                                               # Changing point size
    geom_smooth(method = "lm", aes(fill = Entity), se=FALSE) +               # Adding linear model fit, colour-code by country, se=false 
    theme_bw() +
    ggtitle("LPI values for various Entity from 1970-2016")  + 
    ylab("LPI\n") +                             
    xlab("\nYear")  +
    theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),     # making the years at a bit of an angle
          axis.text.y = element_text(size = 10,
                                     axis.title = element_text(size = 12, face = "plain"),                        
                                     panel.grid = element_blank(),                                   # Removing the background grid lines               
                                     plot.margin = unit(c(1,1,1,1), units = , "cm"),                 # Adding a 1cm margin around the plot
                                     plot.title = element_text(size = 12, hjust=0.3),
                                     legend.text = element_text(size = 10, face = "italic"),         # Setting the font for the legend text
                                     legend.title = element_blank(),                                 # Removing the legend title
                                     legend.position = "right")))                                # Setting legend position - 0 is left/bottom, 1 is top/right



# saving the plots
# with ggplot2
getwd()
ggsave("My Tutorial/histogram.jpg", plot=LPI_hist)
# or 

(LPI_hist <- ggplot(data_long, aes(x = LPI)) +                
    geom_histogram(binwidth = 7, colour = "dark green", fill = "light blue") +    # Changing the binwidth and colours
    geom_vline(aes(xintercept = mean(LPI)),                       # Adding a line for mean abundance
               colour = "orange", linetype = "dashed", size=1) +           # Changing the look of the line
    theme_bw() +                                                        # Changing the theme to get rid of the grey background
    ylab("Count\n") +                                                   # Changing the text of the y axis label
    xlab("\nLPI")  +
    ggtitle("Distribution of LPI over years 1970-2016 in different entities") +     # \n adds a blank line between axis and text
    theme(axis.text = element_text(size = 12),  # Changing font size of axis labels and title
          plot.title = hjust=1, size = 1,
          axis.title.x = element_text(size = 14, face = "plain"),       # face="plain" is the default, you can change it to italic, bold, etc. 
          panel.grid = element_blank()))                              # Removing the grey grid lines
ggsave("My Tutorial/histogram.jpg", plot=LPI_hist)




scale_fill_manual(values = c("#EE7600", "#00868B")) +     # Adding custom colours for lines and points            # Adding custom colours for solid geoms (ribbon)
  scale_colour_manual(values = c("#EE7600", "#00868B"), 
                      scale_fill_manual(values = c("#EE7600", "#00868B", "red", )) +               # Adding custom colours for solid geoms 
                        scale_colour_manual(values = c("#EE7600", "#00868B")) +   
                        str(LatinAmerica$year)
                      write.csv(data_wide, "LPIdata.csv", row.names = FALSE)