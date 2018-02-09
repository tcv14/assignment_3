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
saveRDS(PB2010,file="./Data/PB Apprehensions 2010.rds")
saveRDS(PB2017,file='./Data/PB Apprehensions 2017.rds')
saveRDS(monthly,file='./Data/PB monthly summaries.rds')

# Compare by month

# write a function that would produce barplots by month
# PB1 and PB2 should be dataframes, and the function is designed to let PB2010 = PB1 and PB2017 = PB2
# n should be a string that indicates the month of the graph

month_graph <- function(PB1,PB2,n){
    month <- c('January','Feburary','March','April','May','June','July','August',
               'September','October','November','December')
    time <- which(month==n)
    m <- rbind(PB1[,time+1],PB2[,time+1])
    rownames(m) <- c('2010','2017')
    colnames(m) <- PB2010$Sector
    barplot(as.matrix(m),col=c('darkblue','red'),ylab='Number of Apprehensions',
            main=paste('Total Number of Illegal Alien Apprehensions in', n,sep = ' '),
            beside = TRUE,las=2,cex.names = 0.7)
    legend("topleft", 
           legend = rownames(m), 
           fill = c("darkblue", "red"))
}

# an example of the function above
month_graph(PB2010,PB2017,'January')

# Compare by sector

# write a function that would produce barplots by sector
# PB1 and PB2 should be dataframes, and the functions is designed to take PB2010 as PB1 and PB2017 as PB2
# n should be a string that indicates the sector of the graph

sector_graph <- function(PB1,PB2,n){
    sector <- as.character(PB2010$Sector)
    s <- which(sector==n)
    m <- rbind(PB1[s,-1],PB2[s,-1])
    rownames(m) <- c('2010','2017')
    colnames(m) <- c('January','Feburary','March','April','May','June','July','August',
                     'September','October','November','December')
    barplot(as.matrix(m),col=c('yellow','green'),ylab='Number of Apprehensions',
            main=paste('Total Number of Illegal Alien Apprehensions in',n,sep = ' '),
            beside = TRUE, las=2)
    legend("topright", 
           legend = rownames(m), 
           fill = c("yellow", "green"))
}

# an example of the function above

sector_graph(PB2010,PB2017,'Big Bend')

# Sector with most apprehensions in 2010
PB2010$Total <- apply(PB2010[,-1],1,sum)
most_2010 <- as.character(PB2010$Sector[PB2010$Total==max(PB2010$Total)])

# Sector with most apprehensions in 2017
PB2017$Total <- apply(PB2017[,-1],1,sum)
most_2017 <- as.character(PB2017$Sector[PB2017$Total==max(PB2017$Total)])

# Three months periods

# By sector


# # Time series data
# ts <- as.vector(t(monthly[,-1]))
# ts3 <- ts(ts2, start = c(2000,10), frequency=12)


# ts.plot(ts1)

# monthly data mean
monthly$mean <- apply(monthly[,-1],1,mean)



# three_period <- function(x) {
#   period <- data.frame()
#   for (i in (1:nrow(new_month))) {
#     for (j in (1:(ncol(new_month)-2))){
#       period[i,j] <- new_month[i,j]+new_month[i,j+1]+new_month[i,j+2]
#     }
#   }
#   colnames(period) <- c('Jan-Mar','Feb-Apr','Mar-May','Apr-Jun','May-Jul','Jun-Aug','Jul-Sep',
#                         'Aug-Oct','Sep-Nov','Oct-Dec')
#   rownames(period) <- monthly$year
#   period
# }
# 
# three_period()