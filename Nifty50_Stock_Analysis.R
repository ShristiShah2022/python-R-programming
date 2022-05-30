##############################################################################################################################################################
# 1. (a). Download the weekly Closing price for all 50 stocks and NIFTY 50 index (last three                                                                 #
#         years or from 01-05-2019 to 01-05-2022). Calculate the returns.                                                                                    #        
##############################################################################################################################################################

# get the current working directory
getwd()

# set the working directory
setwd("C:/Users/91882/Desktop/IIMT-Day1/Term 4/Modelling with R/Group Assignment 1") # set your own working directory here

# Loading required packages
library(tidyquant)
library(readxl)
library(dplyr)

# Importing Excel Data into Data frame
Stock_data <- read_excel("NIFTY50.xlsx")
#View(Stock_data)

# Fetching all NSE stock ticker symbols
ticker_names <- Stock_data[,3]

# Defining start time and end time for analysis
time_from <- "2019-05-01" ####### Starting date
time_to <- "2022-05-01" ####### Ending date

# Get stock prices for Nifty 50 stocks for given time period
stock_prices <- ticker_names %>%
  tq_get(get = "stock.prices",  from = time_from, to = time_to) 

# Sub setting only closed prices along with ticker, date
stock_prices_closed_price <- stock_prices[,c(1,2,6)] 
#View(stock_prices_closed_price)

# Fetching only Weekly closed price for each Ticker
stocks.weekly <- stock_prices_closed_price %>%
  group_by(YAHOO_TICKER) %>%
  tq_transmute(select = close, mutate_fun = to.weekly, indexAt = "lastof")  

# Calculating simple return for each stock.
df_simple <- stocks.weekly %>%
group_by(YAHOO_TICKER) %>%
mutate(simple_returns = (close - lag(close))/close) 

# Calculating compounded return for each stock.
df_compound <- df_simple %>%
  group_by(YAHOO_TICKER) %>%
  mutate(compounded_returns = log(1 + simple_returns))

# To view table in RStudio
View(df_compound)

# saving returns data as csv
write.csv(df_compound, file = "Stocks.Nifty50.Weekly.Returns.Data.csv", row.names = FALSE)

##############################################################################################################################################################
# 1. (b). Compute the mean and standard deviation of the returns for each stock.                                                                             #
##############################################################################################################################################################

stocks.mean.SD.Summary <- df_compound %>%
  group_by(YAHOO_TICKER) %>%
  summarise(mean_simple_return = mean(simple_returns, na.rm=TRUE), SD_simple_return = sd(simple_returns, na.rm=TRUE),
            mean_compounded_return = mean(compounded_returns, na.rm=TRUE), SD_compounded_return = sd(compounded_returns, na.rm=TRUE))

# To view table in RStudio
View(stocks.mean.SD.Summary)

# saving returns data as csv
write.csv(stocks.mean.SD.Summary, file = "Stocks.Nifty50.Summary.Data.csv", row.names = FALSE)


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
setwd("C:/Users/91882/Desktop/IIMT-Day1/Term 4/Modelling with R/Group Assignment 1") # set your own working directory here

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

# 1. From the above result, we see that the highest correlation between Price and Area is for Stories 1 = 0.583(approx.)
# 2. Lowest correlation between Price and Area is for Stories 4 = 0.395 (approx.)

# 3. Correlation behavior is approximately same for stories 2(0.547 approx.) and 3(0.572 approx.) as in case of stories 1.


################################################################################################################################################################
# 3. (a).  Refer KohliODI.xlsx, Compute yearly batting average (You need to add the code for the data cleaning).                                               #
################################################################################################################################################################

# loading required packages
library(readxl)
library(dplyr)
library(summarytools)

# Checking and setting current working directory

getwd()
setwd("C:/Users/91882/Desktop/IIMT-Day1/Term 4/Modelling with R/Group Assignment 1") # set your own working directory here

# reading excel data
kohli_odi <- read_excel("KohliODI.xlsx")
View(kohli_odi)

# steps for data cleaning -- removing records where team did not bat or Kohli did not bat
kohli_TDNB_DNB <- subset(kohli_odi, Runs == 'TDNB' |  Runs == 'DNB' ) 

kohli_batting_df =  kohli_odi%>%
  anti_join(kohli_TDNB_DNB,by="ODINO")

# Add new column for year in new data frame

colnames(kohli_batting_df)[13] <- "Date" # renaming Start Date to Date

kohli_batting_df$Year <- format(kohli_batting_df$Date, format="%Y") 

kohli_batting_df$Runs <- as.numeric(kohli_batting_df$Runs) # converting runs from character to numeric

glimpse(kohli_batting_df) # verify the data type for Runs

# Creating new data frame with batting average details
Kohli.Batting.summary <- kohli_batting_df %>%
  group_by(Year)  %>%
  summarise(Number_Of_Matches_Played = n(),Total_Yearly_Runs = sum(Runs,na.rm=TRUE)
            ,Yearly_Batting_Average =   (Total_Yearly_Runs/ Number_Of_Matches_Played) )

# view the new data frame in grid
View(Kohli.Batting.summary)

# saving stories.summary as csv
write.csv(Kohli.Batting.summary, file = "Kohli.Batting.summary.csv", row.names = FALSE)

################################################################################################################################################################
# 3. (b).  Construct a bar graph including Dismissal and Inns. Give your comments.                                                                                  #
################################################################################################################################################################
# Loading packages for plotting 
library(ggplot2)

# Bar plot for two categorical variables
ggplot(kohli_batting_df, aes(x = Inns, fill = Dismissal)) +geom_bar(position = "dodge")

##########
#Comments#
##########

# 1. Highest Dismissal for both the innings is Kohli got "Caught"
# 2. if Kohli plays in 2nd innings,then next possibility if not caught can be "Not Out"
# 3. Getting "bowled" is the 2nd highest reason for dismissal in both the innings
# 4. in 2nd innings, no one has taken Kohli's wicket
# 5. Chances of run out is more in 1st innings compared to 2nd innings
# 6. Chances of getting lbw is more in 2nd innings compared to 1st innings











