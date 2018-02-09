# Load the data
PB2010 <- read.csv('./Data/BP Apprehensions 2010.csv')
PB2017 <- read.csv('./Data/PB Apprehensions 2017.csv')
monthly <- read.csv('./Data/PB monthly summaries.csv')

# Clean the data
PB2010 <- cbind(Sector=PB2010[,1],PB2010[,5:ncol(PB2010)],PB2010[,2:4])

PB2017 <- PB2017[,-14]
PB2017 <- PB2017[-10,]
PB2017 <- cbind(Sector=PB2010$Sector,as.data.frame(sapply(PB2017[2:ncol(PB2017)],function(x){as.integer(gsub(',','',x))})))
PB2017 <- cbind(Sector=PB2017[,1],PB2017[,5:ncol(PB2017)],PB2017[,2:4])

monthly <- cbind(year=monthly[,1],monthly[,5:ncol(monthly)],monthly[,2:4])

# Save cleaned data
saveRDS(PB2010,file="PB Apprehensions 2010.rds")
saveRDS(PB2017,file='PB Apprehensions 2017.rds')
saveRDS(monthly,file='PB monthly summaries.rds')


# Compare by month

month_graph <- function(PB1,PB2,n){
    month <- c('January','Feburary','March','April','May','June','July','August',
               'September','October','November','December')
    time <- which(month==n)
    m <- rbind(PB1[,time+1],PB2[,time+1])
    rownames(m) <- c('2010','2017')
    colnames(m) <- PB2010$Sector
    barplot(as.matrix(m),col=c('darkblue','red'),xlab='Sector',ylab='Number of Apprehensions',
            main=paste('Total Number of Illegal Alien Apprehensions in', n,sep = ' '),
            beside = TRUE,las=2,cex.names = 0.7)
    legend("topleft", 
           legend = rownames(m), 
           fill = c("darkblue", "red"))
}

month_graph(PB2010,PB2017,'January')

# Compare by sector

sector_graph <- function(PB1,PB2,n){
    sector <- as.character(PB2010$Sector)
    s <- which(sector==n)
    m <- rbind(PB1[s,-1],PB2[s,-1])
    rownames(m) <- c('2010','2017')
    colnames(m) <- c('January','Feburary','March','April','May','June','July','August',
                     'September','October','November','December')
    barplot(as.matrix(m),col=c('yellow','green'),xlab='Month',ylab='Number of Apprehensions',
            main=paste('Total Number of Illegal Alien Apprehensions in',n,sep = ' '),
            beside = TRUE, las=2)
    legend("topright", 
           legend = rownames(m), 
           fill = c("yellow", "green"))
}

sector_graph(PB2010,PB2017,'Big Bend')

# Sector with most apprehensions in 2010
PB2010$Total <- apply(PB2010[,-1],1,sum)
most_2010 <- as.character(PB2010$Sector[PB2010$Total==max(PB2010$Total)])

# Sector with most apprehensions in 2017
PB2017$Total <- apply(PB2017[,-1],1,sum)
most_2017 <- as.character(PB2017$Sector[PB2017$Total==max(PB2017$Total)])

# Three months periods

new_month <- subset(monthly[,-1])

c('Jan-Mar','Feb-Apr','Mar-May','Apr-Jun','May-Jul','Jun-Aug','Jul-Sep','Aug-Oct','Sep-Nov','Oct-Dec')
three <- function(x) {
  for (i in (1:ncol(x)-2)) {
    
  }
}