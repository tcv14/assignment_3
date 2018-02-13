# Load the data
PB2010 <- read.csv('./Data/BP Apprehensions 2010.csv')
PB2017 <- read.csv('./Data/PB Apprehensions 2017.csv')
monthly <- read.csv('./Data/PB monthly summaries.csv')

# Clean the data
PB2017 <- PB2017[,-14]
PB2017 <- PB2017[-10,]
PB2017 <- cbind(Sector=PB2010$Sector,as.data.frame(sapply(PB2017[2:ncol(PB2017)],function(x){as.integer(gsub(',','',x))})))

# Save cleaned data
saveRDS(PB2010,file="./Data/PB Apprehensions 2010.rds")
saveRDS(PB2017,file='./Data/PB Apprehensions 2017.rds')
saveRDS(monthly,file='./Data/PB monthly summaries.rds')



# PART A plots

# Compare by month

# write a function that would produce barplots by month
# PB1 and PB2 should be dataframes, and the function is designed to let PB2010 = PB1 and PB2017 = PB2
# n should be a string that indicates the month of the graph

month_graph <- function(PB1,PB2,n){
    month <- c('October','November','December','January','Feburary','March','April','May','June','July','August',
               'September')
    time <- which(month==n)
    m <- rbind(PB1[,time+1],PB2[,time+1])
    rownames(m) <- c('2010','2017')
    colnames(m) <- PB2010$Sector
    barplot(as.matrix(m),col=c('darkblue','red'),ylab='Number of Apprehensions',
            main=paste('Total Number of Illegal Alien Apprehensions in', n,sep = ' '),
            beside = TRUE,las=2,cex.names = 0.6)
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
    colnames(m) <- c('October','November','December','January','Feburary','March','April','May','June','July','August',
                    'September')
    barplot(as.matrix(m),col=c('yellow','green'),ylab='Number of Apprehensions',
            main=paste('Total Number of Illegal Alien Apprehensions in',n,sep = ' '),
            beside = TRUE, las=2)
    legend("topright", 
           legend = rownames(m), 
           fill = c("yellow", "green"))
}

# an example of the function above

sector_graph(PB2010,PB2017,'Big Bend')


# PART B t-test by sector

# Sector with most apprehensions in 2010
PB2010$Total <- apply(PB2010[,-1],1,sum)
most_2010 <- PB2010[PB2010$Total==max(PB2010$Total),]
most_2010_s <- as.character(PB2010$Sector[PB2010$Total==max(PB2010$Total)]) # gives the name of the sector

# Sector with most apprehensions in 2017
PB2017$Total <- apply(PB2017[,-1],1,sum)
most_2017 <- PB2017[PB2017$Total==max(PB2017$Total),]
most_2017_s <- as.character(PB2017$Sector[PB2017$Total==max(PB2017$Total)]) # gives the name of the sector

# Combine these two rows as a new dataframe
most <- as.data.frame(t(rbind(most_2010,most_2017)))
colnames(most) <- c('2010','2017')
most <- most[-1,]
most <- most[-13,]
most$`2010` <- as.numeric(as.character(most$`2010`))
most$`2017` <- as.numeric(as.character(most$`2017`))

# a variance test first
var.test(most$`2010`,most$`2017`) # the result of the test is that two samples have same variance

# then the initial t-test
t.test(most$`2010`,most$`2017`,paired=FALSE,var.equal = TRUE) 
# with a significance level of 0.05, we have to reject the null hypothesis and conclude that there are differences in mean

# a second t-test to find out if the mean level of apprehension increases from 2010 to 2017
t.test(most$`2010`,most$`2017`,paired=FALSE,var.equal = TRUE,alternative='greater')
# with a significance level of 0.05, we have to reject the null hypothesis and conclude that the mean level in 2010 is greater than the mean level in 2017



# PART C t-test for three month period

# Three months periods

# write a function that would return a dataframe with the total number of apprehensions in 3-month period
# object should be a data frame, particularly, should be either PB2010 or PB2017

three_period <- function(object) {
  period <- data.frame()
  for (i in (1:nrow(object))) {
    for (j in (1:(ncol(object)-3))){
      period[i,j] <- object[i,j+1]+object[i,j+2]+object[i,j+3]
    }
  }
  colnames(period) <- c('Oct-Dec','Nov-Jan','Dec-Feb','Jan-Mar','Feb-Apr','Mar-May','Apr-Jun',
                        'May-Jul','Jun-Aug','Jul-Sep')
  rownames(period) <- PB2017$Sector
  period
}

t_month_2010 <- three_period(PB2010[,1:13])
t_month_2017 <- three_period(PB2017[,1:13])

# Calculate the total apprehension for each three month period and assign a name for the new row
t_month_2010 <- rbind(t_month_2010,colSums(t_month_2010))
rownames(t_month_2010) <- c(rownames(t_month_2010)[-length(rownames(t_month_2010))],'Total')

t_month_2017 <- rbind(t_month_2017,colSums(t_month_2017))
rownames(t_month_2017) <- c(rownames(t_month_2017)[-length(rownames(t_month_2017))],'Total')

# Find the maximum apprehension level for each year and return the time period
max_2010 <- t_month_2010[,t_month_2010[length(t_month_2010),]==max(t_month_2010[length(t_month_2010),])]
max_2017 <- t_month_2017[,t_month_2017[length(t_month_2017),]==max(t_month_2017[length(t_month_2017),])]

max_2010_period <- colnames(t_month_2010)[t_month_2010[length(t_month_2010),]==max(t_month_2010[length(t_month_2010),])] # gives the exact time period of the max level apprehension
max_2017_period <- colnames(t_month_2017)[t_month_2017[length(t_month_2017),]==max(t_month_2017[length(t_month_2017),])] # gives the exact time period of the max level apprehension

# Combine these two samples together under a new data frame
max_three <- as.data.frame(cbind(max_2010,max_2017))

# Perform a variance test first
var.test(max_three$max_2010,max_three$max_2017) # the result of the variance test is that two samples having the same variance

# then do a two-sided t-test
t.test(max_three$max_2010,max_three$max_2017,paired = FALSE,var.equal = TRUE)
# with a significance level of 0.05, we can conclude that means of these two samples are different

# then do a one-sided test to see whether there is an increase in apprehension from 2010 to 2017
t.test(max_three$max_2010,max_three$max_2017,paired=FALSE,var.equal = TRUE,alternative = 'less')
# with a significance level of 0.05, we can conclude that mean level of apprehension in 2017 is greater than the mean level of apprehension in 2010
# thus we can say that there is an increae in meal apprehension level from 2010 to 2017



# PART D time series data

# order the dataset so that the year is in ascending order
monthly <- monthly[order(monthly$year),]

# yearly average apprehension
monthly$mean <- apply(monthly[,-1],1,mean)

ts1 <- monthly[,2:13]
ts2 <- as.vector(t(ts1))
ts3 <- ts(ts2, start = c(2000,1), frequency=12) # convert the fiscal year into normal calendar year

tsplot <- function(ts) {
  ts.plot(ts, gpars=list(xlab="Fiscal Year", ylab="Apprehensions", lty=c(1:3)),col='blue',main='Time Series Data for Monthly Apprehensions')
  label <- as.character(seq(from=2000,to=2017))
  for (i in 1:18) {
    segments(monthly$year[i],monthly$mean[i],monthly$year[i]+1,monthly$mean[i],col='red')
    text(x=monthly$year[i]+0.9,y=monthly$mean[i],pos=4,labels=label[i],col='red',cex=0.5,font=2)
  }
  abline(v=2000:2017,col='grey',lty=3)
  legend('topright',lty=1,col='red',legend = 'Average Apprehensions for year 20xx',cex = 0.75)
}

tsplot(ts3)
