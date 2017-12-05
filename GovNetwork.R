library(dplyr)
library(stringdist)
library(ggplot2)
library(plotly)
library(SciencesPo)
library(statnet)
library(lubridate)
library(data.table)


gov<-read.csv("/Users/andiedonovan/GovTerm.csv", header=TRUE, sep=";", stringsAsFactors = FALSE, blank.lines.skip = TRUE)
colnames(gov)<-c("Name", "State", "Year", "Party")
#rows<-nrow(gov)
#cols<-ncol(gov)
#View(gov)

gov$Year<- as.character(gov$Year)
split_yr = do.call("rbind", strsplit(gov$Year, "-"))
split_yr<- gsub("\\(", replacement="", split_yr)
split_yr<- gsub("\\)", replacement="", split_yr)
gov <- cbind(gov, split_yr)
colnames(gov)<-c("Name", "State", "Year", "Party", "Start", "End")
gov <- subset(gov, select = -Year )
gov$Term <- interval(as.Date(gov$End, format='%Y'), as.Date(gov$Start, format='%Y')) 
View(gov)

### not working
YRS<- matrix(0, nrow=300, ncol=2)
#all_yrs = seq(1789, 2017, by=1)
for (i in 1789:2017){  # for each year
  for (j in gov[j,]){  # for each row in gov (name)
    if (i %within% gov$term){ # if year within term interval
      YRS[j,1]<- gov[j,1] # add row j, col 1 (name of gov) to YRS
    }
  }
}

matt<-as.matrix(gov) # turn the table into a matrix object
matt2<-as.matrix(gov) # create dummy copy for comparison
adj<-matrix(0, nrow=rows, ncol=rows) # empty matrix
mib<-network.initialize(rows) # create network object

## seeing which politicians live in the same state
for (i in 1:rows){
  for (j in 1:rows){
    if (matt[i,2] == matt2[j,2]) {
      if (i==j){
        adj[i,j] = adj[i,j]
      }
      else if (i!=j){
        adj[i,j] = adj[i,j]+1
      }
    }
  }
}

## seeing which politicians are of the same party
for (i in 1:rows){
  for (j in 1:rows){
    if (matt[i,4] == matt2[j,4]){
      if (i==j){
        adj[i,j] = adj[i,j]
      }
      else if (i!=j){
        adj[i,j] = adj[i,j]+1
      }
    }
  }
}

## Therefore, people of the same state have one pt, people of the same party have one pt, people that share both have 2 points, and same person or nothing in common has zero pts

View(adj) # zeros on diagonals, looks good

mib<-network.initialize(40) # create a sample network object 
adj2 = adj[1:40, 1:40] # only use first 40 observations to look at object
mib[adj2==1] <- 1
mib[adj2==2] <- 2
plot(mib) # very dense network since there are so few factors to connect nodes on 
# most of the points are probably from the same state since the selection was not randomized


# ---------- other data set ------------ #
g<-read.csv("/Users/andiedonovan//Governors.csv", header=TRUE, sep=";", stringsAsFactors = TRUE, blank.lines.skip = TRUE)

g$Term.Start<-parse_date_time(g$Term.Start, c("%b %d, %Y", "%d-%m-%y")) # missing some values
# some issues with century not supplied (assumes 21st century)
g$Term.End<-parse_date_time(g$Term.End, c("%b %d, %Y", "%d-%m-%y")) # missing some values

StartYr<-format(g$Term.Start,"%Y")
EndYr<-format(g$Term.End,"%Y")

int<-interval(g$Term.Start, g$Term.End) # do not have end date for current governors

g$Term.Interval<-int
head(g)

period<- seq(1775, 1800, by=1)

gov.period1 = seq(1775, 1800, by=1)
gov.period2 = seq(1800, 1825, by=1)

period <- function (x) {
  p = x + 25 # x = start year, p = end year
  period.range = seq(x, p, by=1)
}

period.gov = array(500)

period(j)
for (i in 1:length(g)){
  if (gov[i,4] %in% (min(period.range):max(period.range))){
    period.gov <- gov[i,4]
  }
}

gov$Start <- as.numeric(gov$Start)
gov$End <- as.numeric(gov$End)

gov.period1 = array(500)

while (gov$Start < gov$End){
  if (year %in% (gov[i,4]:gov[i,5])){
    gov.period1 <- gov[i,]
  }
}

gov$Term <- year(as.POSIXlt.numeric(gov$Term, tz='UTC', origin= "1960-01-01"))


dt <- data.table(date = as.IDate(pt))

dt[, YR := 0.0 ]                        # I am using a numeric for year here...

dt[ date >= as.IDate("2002-09-01") & date <= as.IDate("2003-08-31"), YR := 1 ]
dt[ date >= as.IDate("2003-09-01") & date <= as.IDate("2004-08-31"), YR := 2 ]
dt[ date >= as.IDate("2004-09-01") & date <= as.IDate("2005-08-31"), YR := 3 ]

df <- gov

df$Period <- NA
for(i in 1:25){
  interval <- get(paste0("Period_", i))
  index <-which(as.POSIXlt(df$Start) %within% interval)
  df$Period[index] <- i
}


