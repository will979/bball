install.packages("devtools")
library(pipeR)
library(XML)
library(plyr)
library(ggplot2)
library(NHANES)
library(dplyr)
library(oilabs)
library(vtreat)
library(xgboost)
library(ranger)
library(magrittr)
library(rvest)
library(lubridate)
library(methods)
library(Rcrawler)
library(httr)
library(purrr)
library(RCurl)
library(git2r)
library(hms)   #adds hms function for converting string to time
library(tidyr)
library(readr)
library(gtools) #appened data frames


#filterbasicnba <- read_csv("R/filterbasicnba.csv", col_types = cols(MP = col_character(),date_played = col_character(), date_playeddate = col_date(format = "%Y-%m-%d")))
#write.csv(rawbasicnba, file = "rawbasicnba.csv")
#startruntime <- Sys.time()



#######Team Brevs######
#Brevs need to be in caps so not to get a 404 error
atlantic <- c("TOR", "BOS", "NYK", "BRK", "PHI")
central <- c("CLE", "IND", "DET", "CHI", "MIL")
south_east <- c("MIA", "ATL", "CHO", "WAS","ORL")
north_west <- c("OKC", "POR", "UTA", "DEN", "MIN")
pacific <- c("GSW", "LAC", "SAC", "PHO", "LAL")
south_west <- c("SAS", "DAL", "MEM", "HOU", "NOP")

all_teams <- c(atlantic, central, south_east, north_west, pacific, south_west)

startruntime <- Sys.time()

######THE DATE#######
#year.2015.start <- as.Date("2018-10-15", format = "%Y-%m-%d")
#at.this.point.in.time <- as.Date("2019-01-10", format = "%Y-%m-%d") #change to today()

#Manual Start Date
#theStartDate <- year.2015.start

#Auto Start Date grabs from the date_played column of the basic stats database and add one day
theStartDate <- as.Date(max(raw.data.basic.database$date_played),format = "%Y%m%d")+1
at.this.point.in.time <- as.Date(today(), format = "%Y-%m-%d")


####### URL LIST AND WEB SCRAPE ######
# Create list variable to store URLs
mainlist <- list()

# Used to pick team from all_teams variable 
t <- 1

# CREATES A LIST OF EVERYDAY AND TEAM 
while (theStartDate <= at.this.point.in.time) {
  url_nba <- paste0("https://www.basketball-reference.com/boxscores/",as.character(format(theStartDate,"%Y%m%d")),"0",all_teams[t],".html")
  if(t <= length(all_teams)){
    
    mainlist[url_nba] <- url_nba # Adds generated URL to list
    t <- t+1
  }else if(t > length(all_teams)){  # adds 1 day to date and resets t = 0 actually should be able to change it to 1
    t <- 1
    theStartDate <- theStartDate + 1
  }
}

# Create list to store URLS
mainliststatus <- list()

# Unlist URLS so GET function will work
unlistmainlist <- unlist(mainlist)

# Gets status code from URL list to see if the webpage exist
for (i in 1:length(unlistmainlist)){
  f <-  GET(unlistmainlist[[i]])
  if(f$status_code== "200"){
    mainliststatus[i] <- f
    (i/length(unlistmainlist)*100) %>% round(2) %>% paste(.,"%",sep = "") %>% print()
  }
}

endruntimegeturl <-  Sys.time()
urlruntime <- endruntimegeturl - startruntime
print(urlruntime)

# Unlist 200 status URLs to get rid of NULLS
goodurls <- unlist(mainliststatus)


####  IGNORE, IF STATEMENTS BELOW WORK #### 
#raw.data.basic.database <- data.frame(matrix(ncol = 25, nrow = 0))   #used to establish data frame !!!HASH OUT AFTER FIRST USE
#raw.data.advance.database <- data.frame(matrix(ncol = 20, nrow = 0))  #used to establish data frame !!!HASH OUT AFTER FIRST USE

####  CREATES MASTER DATAFRAME FOR DATABASE  ####
if(exists("raw.data.basic.database")){
  break
}else{
  raw.data.basic.database <- data.frame(matrix(ncol = 26, nrow = 0))
}

if(exists("raw.data.advance.database")){
  break
}else{
  raw.data.advance.database <- data.frame(matrix(ncol = 21, nrow = 0))
}


###### Collecting and Merging Stats #####
for (i in 1:length(goodurls)){  #####
  
  NBA.url <- read_html(goodurls[i])
  
  (i/length(goodurls)*100) %>% round(2) %>% paste(.,"%",sep = "") %>% print()
  
  nba.stats <- NBA.url %>%
    html_nodes("table") %>%   #h1 for team names
    html_table()
  #html_text()
  nba.team.name <- NBA.url %>%
    html_nodes("h2")%>%
    html_text()
  
  nba.stats[[1]]["date_played"] <- as.character(substr(goodurls[i],48,55))  #gets date from the url and puts it into vector
  nba.stats[[2]]["date_played"] <- as.character(substr(goodurls[i],48,55))
  nba.stats[[3]]["date_played"] <- as.character(substr(goodurls[i],48,55))
  nba.stats[[4]]["date_played"] <- as.character(substr(goodurls[i],48,55))
  
  nba.stats[[1]]["team_name"] <- nba.team.name[4] #baisc    gets team name from h2 tag
  nba.stats[[2]]["team_name"] <- nba.team.name[4] #advance
  nba.stats[[3]]["team_name"] <- nba.team.name[6] #basic
  nba.stats[[4]]["team_name"] <- nba.team.name[6] #advance
  
  nba.stats[[1]]["opp_name"] <- nba.team.name[6] #basic     gets opp name from h2 tag
  nba.stats[[2]]["opp_name"] <- nba.team.name[6] #advance
  nba.stats[[3]]["opp_name"] <- nba.team.name[4] #basic
  nba.stats[[4]]["opp_name"] <- nba.team.name[4] #advance
  
  nba.stats[[1]]["home_away"] <- "away" #basic away     is determined by position on the web page.they put away team first.
  nba.stats[[2]]["home_away"] <- "away" #advance away
  nba.stats[[3]]["home_away"] <- "home" #basic home
  nba.stats[[4]]["home_away"] <- "home" #advance home
  
  nba.stats[[1]]["URL"] <- goodurls[i]   #keeps urls with data set so i do not have to keep a "goodurl" list
  nba.stats[[2]]["URL"] <- goodurls[i]
  nba.stats[[3]]["URL"] <- goodurls[i]
  nba.stats[[4]]["URL"] <- goodurls[i]
  
  basiccolumnnames <- c("Starters","MP","FG","FGA","FG_PERCENT","THREE_POINT","THREE_POINT_ATT","THREE_POINT_PERCENT","FT","FTA","FT_PERCENT","ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS","+/-","date_played","team_name","opp_name","home_away","URL")
  
  advancecolumnnames1 <- c("Starters","MP","TS_Percent","eFG_Percent","ThreePAr","FTr","ORB_Percent","DRB_Percent","TRB_Percent","AST_Percent","STL_Percent","BLK_Percent","TOV_Percent","USG_Percent","ORtg","DRtg","date_played","team_name","opp_name","home_away","URL")
  
  nba.stats[1] <- lapply(nba.stats[1], setNames, basiccolumnnames)  #changes column names in list
  nba.stats[3] <- lapply(nba.stats[3], setNames, basiccolumnnames)  #changes column names in list
  nba.stats[2] <- lapply(nba.stats[2], setNames, advancecolumnnames1)#changes column names in list
  nba.stats[4] <- lapply(nba.stats[4], setNames, advancecolumnnames1)#changes column names in list
  
  basic.away.dataframe.1  <- as.data.frame(nba.stats[1])  #pulls away teams basic stats and places in data frame
  advance.away.dataframe.2  <- as.data.frame(nba.stats[2])#pulls away teams advance stats and places in data frame
  basic.home.dataframe.3  <- as.data.frame(nba.stats[3])  #pulls home teams basic stats and places in data frame
  advance.home.dataframe.4  <- as.data.frame(nba.stats[4])#pulls home teams advance stats and places in data frame
  
  colnames(basic.away.dataframe.1) <- basiccolumnnames     #ensures the column names are right. the was a problem with "+/-"
  colnames(advance.away.dataframe.2) <- advancecolumnnames1 #ensures the column names are right.
  colnames(basic.home.dataframe.3) <- basiccolumnnames     #ensures the column names are right. the was a problem with "+/-"
  colnames(advance.home.dataframe.4) <- advancecolumnnames1 #ensures the column names are right.
  
  raw.data.basic <- merge.data.frame(basic.away.dataframe.1,basic.home.dataframe.3, all = T) #merges the two basic stats data frames together
  raw.data.advance <- merge.data.frame(advance.away.dataframe.2,advance.home.dataframe.4,all = T)  #merges the two advance stats column frames together
  
  colnames(raw.data.basic.database) <- basiccolumnnames  #adds column names to the basic stat database
  raw.data.basic.database <- merge(raw.data.basic,raw.data.basic.database,all = T)  #merges all basic stats into on data frame
  
  colnames(raw.data.advance.database) <-advancecolumnnames1  #adds column names to the advance stat database
  raw.data.advance.database <- merge(raw.data.advance,raw.data.advance.database,all = T)  #merges all advance stats into on data frame
 
  
}

endruntimewebscrape <-  Sys.time()
webscraperuntime <- endruntimewebscrape - startruntime
print(webscraperuntime)
######  SAVES UPDATED CSV FILE  #####
write.csv(raw.data.basic.database, paste0(today(),"raw_baisc_database.csv"))
write.csv(raw.data.advance.database, paste0(today(),"raw_advance_database.csv"))

########## BASIC STATS ##########
###!!!date_played column causes error when changing values to 0!!!!### 
filteredlistbasic<- filter(raw.data.basic.database,Starters != "Starters" & Starters != "Reserves" & Starters != "Team Totals")

#2015-2016
#October 27, 2015 - April 13, 2016
#April 16, 2016 - May 30, 2016 (Playoffs)
#June 2, 2016 - June 19, 2016 (Finals)

#2016-2017
#October 25, 2016 - April 12, 2017
#April 15 - May 25, 2017 (Playoffs)
#June 1-12, 2017 (Finals)

#2017-2018
#October 17, 2017 - April 11, 2018
#April 14 - May 28, 2018 (Playoffs)
#May 31 - June 8, 2018 (Finals)

#2018-2019
#Regular: October 16, 2018 - April 10, 2019
#Playoffs: April 13 - May 2019
#Finals: May - June 2019


#use to get rid of starters and team totals row

filteredlistbasic[filteredlistbasic == "Did Not Play"]<- 0     #gets rid of did not play and replaces with 0
filteredlistbasic[filteredlistbasic == "Player Suspended"]<- 0 
filteredlistbasic[filteredlistbasic == ""]<- 0
filteredlistbasic[filteredlistbasic == "Did Not Dress"]<- 0
filteredlistbasic[filteredlistbasic == "Not With Team"]<- 0

#filteredlistbasic[,3:21] <- sapply(filteredlistbasic[,3:21],as.numeric)

x <- filteredlistbasic %>%
      filter(Starters == "Zaza Pachulia" & date_played > "20171017" & date_played <"20180411" )





########## ADVANCE STATS ##########

filteredlistadvance <- filter(raw.data.advance.database,Starters != "Starters" & Starters != "Reserves" & Starters != "Team Totals") #use to get rid of starters and team totals row



filteredlistadvance[filteredlistadvance == "Did Not Play"]<- 0     #gets rid of did not play and replaces with 0
filteredlistadvance[filteredlistadvance == "Player Suspended"]<- 0 
filteredlistadvance[filteredlistadvance == ""]<- 0
filteredlistadvance[filteredlistadvance == "Did Not Dress"]<- 0
filteredlistadvance[filteredlistadvance == "Not With Team"]<- 0

###### FANDUEL STUFF #######

#FANDUEL SCORING
threepoint <- 1
assist <- 1.5
blocks <- 3
fieldgoalmade <- 2
freethrowmade <- 1
rebound <- 1.2
steals <- 3
turnovers <- -1

#CALUCLATE FANTASY POINTS
filteredlistbasic <- mutate(filteredlistbasic, fanpoints = ((threepoint*THREEPOINT)+(assist*AST)+(blocks*BLK)+(fieldgoalmade*FG)+(freethrowmade*FT)+(rebound*TRB)+(steals*STL)+turnovers*TOV))

basketballrefteamnames <- all_teams
fanduelteamnames <- unique(c(FanDuel_NBA_2019_01_21_32145_players_list$Team, FanDuel_NBA_2019_01_21_32145_players_list$Opponent))
fanduelteamnames <-  c(fanduelteamnames,"TOR","POR","OKC","LAC","DAL","MIN","PHO")

