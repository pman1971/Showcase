### CLEAN WORKSPACE AND SOURCE FUNCTIONS ####
# Clear all objects
rm(list=ls())

# Clear all plots
dev.off()

# Library packages
library(tidyverse)  
library(data.table)
library(rvest)    
library(stringr)   
library(lubridate)
library(hrbrthemes)
library(gganimate)


# PRODUCE ANIMATION OF TEAMS LADDER POSITION FOR THE WHOLE SEASON

# Create a list for all rounds results

# Number of rounds to analyse
roundNoTotal= 25

clubLadderList= list()

for(roundNo in 1:roundNoTotal)
{
  print(paste('Web scraping round', roundNo))
  # Create url for round to webscrape
  urlPage= "https://www.nrl.com/ladder/?competition=111&season=2021&round="
  urlPage= paste0(urlPage, roundNo)
  
  # Read webpage
  webPage= read_html(urlPage)
  
  # Read node that contains the ladder data
  ladderSection= webPage %>% 
    # The relevant tag
    html_nodes('.u-spacing-mt-medium')
  
  # Convert HMLT to text to enable string search
  clubsData= as.character(ladderSection[[1]])
  
  # Extract data for each club
  # The club names in the text reflect the current ladder position
  textSearch= 'clubProfileUrl&quot;:&quot;/clubs/'
  clubPos= gregexpr(pattern= textSearch, clubsData)[[1]]
  # Extract the first few characters of club name
  clubLadder= str_sub(clubsData, clubPos + 34, clubPos + 39)
  # Create dataframe
  clubLadderDF= data.frame(Club= clubLadder,
                           Round= roundNo,
                           Position= 1:16)
  # Append to list
  clubLadderList[[roundNo]]= clubLadderDF
}

setwd("~/Documents/Showcase/NRL stats")
#saveRDS(clubLadderList, 'clubLadderList.rds')

# Bind list of dataframes into one large dataframe
if(length(clubLadderList)>0)
{
  clubLadderListDF= as.data.frame(rbindlist(clubLadderList))
} else
{
  # If NRL site has removed webpages load previously created data
  clubLadderList= readRDS('clubLadderList.rds')
  clubLadderListDF= as.data.frame(rbindlist(clubLadderList))
}

# Create proper team names
clubsUnique= unique(clubLadderListDF$Club)
Team= c('Roosters',
        'Panthers',
        'Raiders',
        'Knights',
        'Sharks',
        'Warriors',
        'Eels',
        'Storm',
        'Rabbitohs',
        'Broncos',
        'Titans',
        'Dragons',
        'Bulldogs',
        'Wests Tigers',
        'Cowboys',
        'Sea Eagles')

clubsDF= data.frame(Club= clubsUnique,
                    Team)

# Check team names
print(clubsDF)

# Add proper team name to dataframe with
clubLadderListDF=
  clubLadderListDF %>%
  inner_join(clubsDF, by= c('Club'))

# Use official team logo colours for plot
# This was manually sourced from multiple places on the web
cols= c('#6f163d',
        '#0054a4',
        '#002b5c',
        '#e2231b',
        '#006eb5',
        '#ee3524',
        '#000000',
        '#08800f',
        '#00ac5b',
        '#00305e',
        '#6f163d',
        '#00a9d8',
        '#632390',
        '#a7a9ac',
        '#231f20',
        '#f68b1f')

clubLadderListDF$logoColour= cols

# Create plot using ggplot package
animacion= 
  clubLadderListDF %>%
  # Plot position by round and group by team
  ggplot(aes(x=Round, y=Position, group= Team, color=Team)) +
  # Add a line to the plot
  geom_line() +
  # Add a leading point to the plot on the line
  geom_point() +
  # Colour each line corresponding to team logo
  scale_color_manual(values= cols) +
  # Reverse the y- axis so number 1 is at the top
  scale_y_reverse() +
  # Draw on top of the plot a specific highlighted line for the Mighty Eagles!
  geom_line(data = subset(clubLadderListDF, Team == 'Sea Eagles'), 
            size = 1, color = '#6f163d') +
  ggtitle("NRL ladder") +
  theme_ipsum() +
  ylab("Position") +
  # Reveal each round no for the season
  transition_reveal(Round)

# Render the plot into a GIF animation
# Duration was set to 20 secs after trying different values
animate(animacion, width = 700, height = 432, fps = 25, duration = 20, rewind = FALSE)

# Once plot has rendered save the file by right clicking on the GIF

# PERFORM PCA ANALYSIS ON EACH TEAMS STATS

### CLEAN WORKSPACE AND SOURCE FUNCTIONS ####
# Clear all objects
rm(list=ls())

# Clear all plots
dev.off()

# Library packages
library(tidyverse)  
library(data.table)
library(rvest)    
library(stringr)   
library(lubridate)
library(factoextra)
library(fpc)
library(useful)

# Read page with total summary stats
urlPage= "https://www.foxsports.com.au/nrl/nrl-premiership/stats/teams"

# Read webpage
webPage= read_html(urlPage)
tables= webPage %>% html_table()

# Foxsports dynamically adds beting information so structure always changes
# Identify table with 16 records representing all the NRL teams
nrlTeamNo= 16
summaryTeamTotal= tables[[which(sapply(tables, nrow)== nrlTeamNo)]]

setwd("~/Documents/Showcase/NRL stats")
# saveRDS(summaryTeamTotal, 'summaryTeamTotal.rds')
# summaryTeamTotal= readRDS('summaryTeamTotal.rds')

# Prepare data
teamName= gsub('[[:digit:]]+', '', summaryTeamTotal$Name)
teamName= paste0(1:16, '.', teamName)

# Prepare data for PCA- matrix with row names
mydata = summaryTeamTotal[,2:ncol(summaryTeamTotal)]

# Remove field goals
mydata$`FG%`= NULL

# Source metrics file which stores the legends for each NRL metric
source('metrics.R')

names(mydata)= metric

# Get rid of commas
mydata= 
  mydata %>%
  map_df(str_replace, pattern = ",", replacement = "") %>%
  map_df(as.numeric)

# Put team names into dataframe row for PCA
row.names(mydata) = teamName

# Standardize data so that no variable dominates
mydata <- scale(mydata)

# PCA
res.pca <- prcomp(mydata, scale = TRUE)
fviz_eig(res.pca)

plot1= fviz_pca_biplot(res.pca, alpha.var="contrib",
                       pointsize = "cos2",
                       col.var= as.factor(legendDF$statType))
plot1$labels$size= 'Importance'
plot1$labels$colour= 'Metric'
plot1$labels$alpha= 'Contribution'

# Return PCA plot by team and metrics
plot1

# Note that teams together are more alike

# Note Panthers who excel at Kicking Game and Kick Return used this in Grand Final to beath Rabbitohs
