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
startruntime <- Sys.time()

#Team Brevs
#Brevs need to be in caps so not to get a 404 error
atlantic <- c("TOR", "BOS", "NYK", "BRK", "PHI")
central <- c("CLE", "IND", "DET", "CHI", "MIL")
south_east <- c("MIA", "ATL", "CHO", "WAS","ORL")
north_west <- c("OKC", "POR", "UTA", "DEN", "MIN")
pacific <- c("GSW", "LAC", "SAC", "PHO", "LAL")
south_west <- c("SAS", "DAL", "MEM", "HOU", "NOP")

all_teams <- c(atlantic, central, south_east, north_west, pacific, south_west)


year.2017.start <- as.Date("2017-10-18", format = "%Y-%m-%d")
year.2017.end <- as.Date(today(), format = "%Y-%m-%d")

theDate <- year.2017.start

# Create list variable to store URLs
mainlist <- list()

# Used to pick team from all_teams variable 
t <- 1


while (theDate <= year.2017.end) {
  url_nba <- paste0("https://www.basketball-reference.com/boxscores/",as.character(format(theDate,"%Y%m%d")),"0",all_teams[t],".html")
  if(t <= length(all_teams)){
    
  mainlist[url_nba] <- url_nba # Adds generated URL to list
  t <- t+1
  }else if(t > length(all_teams)){  # adds 1 day to date and resets t = 0 actually should be able to change it to 1
    t <- 1
    theDate <- theDate + 1
  }
}

# Create list to store URLS
mainliststatus <- list()

# Unlist URLS so GET function will work
unlistmainlist <- unlist(mainlist)

# Gets status code from URL list
for (i in 1:length(unlistmainlist)){
  f <-  GET(unlistmainlist[[i]])
  if(f$status_code== "200"){
    mainliststatus[i] <- f
  }
}

# Unlist 200 status URLs to get rid of NULLS
goodurls <- unlist(mainliststatus)

# Used to add one to goodurl list and xpath
rawlistadvance <- list()    # List to store raw advance data
xpathstart <- '//*[@id="box_'
xpathend <- '_advanced"]'   #!!!!! Change advanced to basic to get basic table stats!!!!!!


for (i in 1:length(goodurls)) {  #!!!!!should be length(goodurls)

  # !!!! If scrape breaks check that Xpath has not changed or if length of URL changed affecting substring !!!!!
  
#xpath.ball <- paste('//*[@id="box_',as.character(tolower(substring(goodurls[i],57,59))),'_basic"]')
xpathmiddle <- as.character(tolower(substring(goodurls[i],57,59))) # Get team name from URL
  xpath.ball <- paste(xpathstart,xpathmiddle,xpathend,sep = "") # Make sure to have sep="" or it will put spaces in string.  Combines variables to make Xpath
  
  NBA.url <- read_html(goodurls[[i]])
  
  
  nba.stats <- NBA.url%>%
    html_nodes(xpath = xpath.ball) %>%
    html_table(header = TRUE)  #change from false to true to check for headers
   #nba.stats <- lapply(nba.stats, cbind, date_played = substr(goodurls[2],48,55)) #use to combine list and and date played column
   #nba.stats <- lapply(nba.stats, cbind, team_name = substring(goodurls[2],57,59))#use to add team name to data set
  rawlistadvance[i] <- nba.stats
  rawlistadvance[[i]]["date_played"] <- as.character(substr(goodurls[i],48,55))   #use to add date played column
  rawlistadvance[[i]]["team_name"] <- as.character(substring(goodurls[i],57,59))   ##use to add team name to data set
  
  }


combinerawlistadvance <- do.call("rbind", rawlistadvance) #use this to combine list into data frame

#advancecolumnnames <- combinerawlistadvance[1,]  ##used this to make advance column names


colnames(combinerawlistadvance) <- advancecolumnnames[,1:18]  # asigns proper names to columns

filteredlistadvance<- filter(combinerawlistadvance,Starters != "Starters" & Starters != "Reserves" & Starters != "Team Totals") #use to get rid of starters and team totals row



filteredlistadvance[filteredlistadvance == "Did Not Play"]<- 0     #gets rid of did not play and replaces with 0
filteredlistadvance[filteredlistadvance == "Player Suspended"]<- 0 
filteredlistadvance[filteredlistadvance == ""]<- 0

endruntime <-  Sys.time()
runtime <- endruntime - startruntime
print(runtime)