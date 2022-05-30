################################################################################################################################################################
# 2. (a). Create a data frame/table called stories.summary whose rows are the stories and whose
# columns are: number of houses, mean house price, median house price, and maximum house price.
################################################################################################################################################################

# loading packages 
library(readxl)
library(summarytools)
library(dplyr)

# Checking and setting current working directory

getwd()
setwd("C:/Users/91882/Desktop/IIMT-Day1/Term 4/Modelling with R/Group Assignment 1/Question 2 - Housing Analysis") # set your own working directory here

# reading excel data

housing_data <- read_excel("Housing.xlsx")
View(housing_data)

# creating new data frame to summarize price details based on number of stories 

stories.summary <- housing_data %>%
group_by(stories) %>%
summarise(Number_Of_Houses = n(), Mean_House_Price = mean(Price,na.rm=TRUE)
,Median_House_Price = median(Price,na.rm=TRUE),Maximum_House_Price = max(Price,na.rm = TRUE))


# view the new data frame in grid

View(stories.summary)

# saving stories.summary as csv

write.csv(stories.summary, file = "Stories.summary.csv", row.names = FALSE)


################################################################################################################################################################
# 2. (b).  Order the data by highest area and print the top 10 observations.
################################################################################################################################################################

top_10_houses_by_area = housing_data %>% arrange(desc(area)) %>% top_n(n=10,wt=area)
View(top_10_houses_by_area)

# print top 10 house details by area

# knitr::kable will print top_10_houses_by_area table as character so that it can be passed as parameter within cat

top_10 <- knitr::kable(top_10_houses_by_area)

cat("Top 10 houses By Area : ",top_10,sep = "\n")


################################################################################################################################################################
# 2. (c). Does the relationship between price and area same for all stories segments? (Hypothesis 
# testing is not required; subjective arguments based on the chart or summary is enough)
################################################################################################################################################################

# installing car package for scatter plot group wise with regression line

# install.packages("car")

# load car package

library(car)


# Plotting scatter plot to understand relationship between Price and Area across Stories Type

scatterplot(Price ~ area | stories, data = housing_data)

# As observed the relationship between price and area for all stories segments are positively correlated, i.e., as area increases , Price
# also increases.

# Finding the magnitude of correlation between Price and Area across Stories below:

# creating data frame for two columns: stories and correlation between price and area across each storey type

table_Correlation_price_and_area_across_stories <- housing_data %>%
  group_by(stories) %>%
  summarize(Correlation_Between_Price_and_Area = cor(Price,area))

# view relation as data table in separate window

View(table_Correlation_price_and_area_across_stories)

# print Correlation table on R console

Corelation_matrix <- knitr::kable(table_Correlation_price_and_area_across_stories)

cat("Relation Between Price and Area Across Stories : ",Corelation_matrix,sep = "\n")

# From the above result, we see that the highest correlation between Price and Area is for Stories 1 = 0.583(approx.)
# Lowest correlation between Price and Area is for Stories 4 = 0.395 (approx.)

# Correlation behavior is approximately same for stories 2(0.547 approx.) and 3(0.572 approx.) as in case of stories 1.




