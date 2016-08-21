file_loc <- "./"
library(stringr)
require(XML)

library(ggplot2)
library(ggthemes)
library(reshape2)
library(Cairo)
#library(cairoDevice)


ng = list(
  list(4,list(5,6),list("Clinton","Sanders"),"http://www.realclearpolitics.com/epolls/2016/president/us/2016_democratic_presidential_nomination-3824.html"),
  list(4,list(4,5,6),list("Trump","Cruz","Kasich"),"http://www.realclearpolitics.com/epolls/2016/president/us/2016_republican_presidential_nomination-3823.html"))

g=length(ng)

empty = data.frame("Poll" = character(), "Date" = character(), "Sample" = character(),  "Votes" = numeric(), "poll_date"=factor())
pa = empty

for(k in 1:g) {
  ing = ng[[k]]
  a = ing[[1]]
  b2 = ing[[2]]
  candidatename = ing[[3]]
  myURL = ing[[4]]
  
  tables <- readHTMLTable(myURL) # read the poll web page
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))  #format to table
  poll <- tables[[a]]
  
  poll <- poll[-1,]   #remove top line which is a summary of entire period
  #colnames(poll)[b2] <- candidatename # remove periods from these column names
  
  #convert the poll dates to r dates
  poll_date <- substr(poll$Date,1,str_locate(poll$Date,"-")-2)  
  poll_date <- as.Date(poll_date,"%m/%d")  # ** not scalable ** breaks at year boundary
  
  #plot(poll_date)
  #title(main = candidatename)
  
  jun7 = as.Date("7/7","%m/%d")
  
  years_back = 0
  
  si2=nrow(poll)
  p2 = poll_date
  for(i in 1:nrow(poll)) {
    if (i != 1)
      if (poll_date[i] > jun7)
        if (poll_date[i-1] < jun7)
          if (poll_date[min(i+1, nrow(poll))] > jun7)  
          {
            years_back = years_back + 1
            if(years_back==2)
              si2=i
          }
    p2[i] <- poll_date[i] - 365*years_back #approx
  }
  
  #just one data point out of order
  
  poll_date = p2
  
  #plot(poll_date)
  #title(main = candidatename)
  
  
  print(si2)
  poll <- poll[1:si2,]
  poll_date <- poll_date[1:si2]
  
  poll <- cbind(poll,poll_date)
  
  ncan = length(candidatename)
  for (j in 1:ncan) 
    {
    candidate = replicate(nrow(poll),as.character(candidatename[j]))
    e=as.numeric(c(1:3,b2[j]))
    poll2 <- cbind(poll[,e],candidate)
    colnames(poll2)[4] = "Votes"
    poll2$poll_date = poll_date
    pa = rbind(pa,poll2)
  }
}


d=pa


f = d$Votes
d$Votes = as.numeric(levels(f))[f]

# png(filename="Std_PNG_cairo.png",
#     type="cairo",
#     units="in",
#     width=5,
#     height=4,
#     pointsize=12,
#     res=96)
svg(filename="figure/favor4-f1-partisan.svg",
    width=5,
    height=4,
    pointsize=12)

#Cairo()

#par(cex=2)
#theme_set(theme_igrey(base_size = 10))

d2 = d
d2$candidate = factor(d2$candidate, levels = c("Kasich","Trump","Cruz","Sanders","Clinton"))
qplot(as.Date(poll_date),Votes, data=d2, geom = c('point'),alpha=I(.01),colour=candidate,xlab="Date",ylab="Partisan Votes %\n(realclearpolitics.com)",main="Democrats Voted for Clinton\nRepublicans Voted for Trump")+guides(colour = guide_legend(override.aes = list(size=4)))  +  geom_smooth(method='loess',lwd=I(2),span=.4,se=FALSE)+ theme_igray(base_size = 15) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#+ theme(plot.title = element_text(size=20, face="bold",family="Times New Roman"))


dev.off()


write.csv(d, file = paste(file_loc, "Poll4.csv",sep = ""),row.names=TRUE, na="") 
#,StringsAsFactors=F

