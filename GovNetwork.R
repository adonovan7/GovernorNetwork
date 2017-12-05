gov<-read.csv("~/GovTerm.csv", header=TRUE, sep=";", stringsAsFactors = FALSE, blank.lines.skip = TRUE)
colnames(gov)<-c("Name", "State", "Year", "Party")

### Split up term dates ###
gov$Year<- as.character(gov$Year)
split_yr = do.call("rbind", strsplit(gov$Year, "-"))
split_yr<- gsub("\\(", replacement="", split_yr)
split_yr<- gsub("\\)", replacement="", split_yr)
gov <- cbind(gov, split_yr)

colnames(gov)<-c("Name", "State", "Year", "Party", "Start", "End")
gov <- subset(gov, select = -Year )
#gov$Term<-NA
#gov$Term <- interval(as.Date(gov$End, format='%Y'), as.Date(gov$Start, format='%Y'))

gov$Start <- as.character(gov$Start)
gov$End <- as.character(gov$End)
gov$Start <- as.numeric(gov$Start)
gov$End <- as.numeric(gov$End)
max(gov$End - gov$Start) # 16 year term
gov$Group<-NA
gov[,7:22]<-NA

gov$Start <- as.character(gov$Start)
gov$End <- as.character(gov$End)
gov$Start <- as.numeric(gov$Start)
gov$End <- as.numeric(gov$End)
for (i in 1:nrow(gov)){
  myseq <- seq(gov[i,4], gov[i,5], by=1)
  for (j in 1:length(myseq)){
    a = 6+j
    gov[i,a] <- myseq[j]
  }
}

# creating columns with year # of full term in office
names(gov)[7:23] <- paste("Year", seq(1, 16, by=1), sep="")

period1 <- seq(from=1775, to=1824, by =1)
period2 <- seq(from=1825, to=1874, by =1)
period3 <- seq(from=1875, to=1924, by =1)
period4 <- seq(from=1925, to=1974, by =1)
period5 <- seq(from=1975, to=2025, by =1)

### splitting into periods 1, 2, 3, 4, or 5
for (j in 1:nrow(gov)){ 
  for (i in 7:22){
    if (gov[j,i] %in% period1 & is.na(gov[j,6])==TRUE){
      gov[j,6] <- "1"
    }
    else if (gov[j,i] %in% period2 & is.na(gov[j,6])==TRUE){
      gov[j,6]<- "2"
    }
    else if (gov[j,i] %in% period3 & is.na(gov[j,6])==TRUE){
      gov[j,6]<- "2"
    }
    else if (gov[j,i] %in% period3 & is.na(gov[j,6])==TRUE){
      gov[j,6]<- "3"
    }
    else if (gov[j,i] %in% period4 & is.na(gov[j,6])==TRUE){
      gov[j,6]<- "4"
    }
    else if (gov[j,i] %in% period5 & is.na(gov[j,6])==TRUE){
      gov[j,6]<- "5"
    }
  }
}

##########################
# Create network matrix #
##########################

#for (x in 1:5){
x=2  
  y <- gov[gov$Group==x,] ### need to filter
  View(y)
  gov.mat<-as.matrix(y) # turn the table into a matrix object
  gov.mat2<-as.matrix(y) # create dummy copy for comparison
  rows<-nrow(y)
  cols<-ncol(y)
  View(gov.mat)
  adj<-matrix(0, nrow=rows, ncol=rows) # empty matrix; adjacency matrix
  gov.net<-network.initialize(rows) # create network object
  
  #check if same state
  for (i in 1:rows){
    for (j in 1:rows){
      if (gov.mat[i,2] == gov.mat2[j,2]) {  
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
      if (gov.mat[i,3] == gov.mat2[j,3]){
        if (i==j){
          adj[i,j] = adj[i,j]
        }
        else if (i!=j){
          adj[i,j] = adj[i,j]+1
        }
      }
    }
  }
  
  mib<-network.initialize(nrow(adj))
  mib[adj==1] <- 1
  mib[adj==2] <- 2
  plot(mib)
  
 # }

################ 
# network object 
################

mib<-network.initialize(nrow(adj))
mib[adj==1] <- 1
mib[adj==2] <- 2
plot(mib)
