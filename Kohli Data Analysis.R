################################################################################################################################################################
# 3. (a).  Refer KohliODI.xlsx, Compute yearly batting average (You need to add the code for the data cleaning).                                               #
################################################################################################################################################################

# loading required packages
library(readxl)
library(dplyr)
library(summarytools)

# Checking and setting current working directory

getwd()
setwd("C:/Users/91882/Desktop/IIMT-Day1/Term 4/Modelling with R/Group Assignment 1/Question 3 - Kohli ODI Analysis") # set your own working directory here

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





