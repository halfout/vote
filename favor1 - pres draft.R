
dothis <- function (a,b,c,candidatename,myURL) {
#file_loc <- "C:/Data/"  #for testing
file_loc <- "./"

library(stringr)
#adapted from http://stackoverflow.com/questions/1395528/scraping-html-tables-into-r-data-frames-using-the-xml-package

#import the most current polling information into a data table for analysis
require(XML)
#myURL <- "http://www.realclearpolitics.com/epolls/other/trump_favorableunfavorable-5493.html"

tables <- readHTMLTable(myURL) # read the poll web page
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))  #format to table 
#a=which.max(n.rows)

dfPoll <- tables[[a]]  #assign the poll data to a data frame

write.csv(dfPoll, file = paste(file_loc, "Poll.csv",sep = ""),row.names=TRUE, na="") 


#########33
#suppressMessages(library(dplyr))
#suppressWarnings(library(tidyr))



suppressWarnings(library(stringr))

poll <- read.csv(file = paste(file_loc,"Poll.csv",sep = ""), stringsAsFactors = FALSE) #read the poll data into memory
poll <- dfPoll
#poll <- poll[1:20,]  #limit dataset to timeframe of available press sentiment analysis (hard coded)
poll <- poll[-1,]   #remove top line which is a summary of entire period
colnames(poll)[b:c] <- c("Favorable","Unfavorable") # remove periods from these column names

#convert the poll dates to r dates
poll_date <- substr(poll$Date,1,str_locate(poll$Date,"-")-2)  
poll_date <- as.Date(poll_date,"%m/%d")  # ** not scalable ** breaks at year boundary


si=nrow(poll)
print('nrow')
print(si)
for(i in 2:nrow(poll)) {
  if (poll_date[i] > as.Date("7/7","%m/%d")) {
    if (poll_date[i-1] < as.Date("7/7","%m/%d")) {
      si = i
      break}}
}

if(candidatename=="Johnson")
  si=nrow(poll)+1

for(i in 1:nrow(poll)) {
  if (i >= si) {
    poll_date[i] <- poll_date[i] - 365
  }
}

si2 = nrow(poll)

there_yes = 0
for(i in (si):nrow(poll)) {
  print(i)
  if(i==si)
  {}
  else{
  if (poll_date[i] > as.Date("7/7", "%m/%d")) {
    if (poll_date[i-1] < as.Date("7/7","%m/%d")) {
      si2 = i
      there_yes=1
      break}}}
}

# 
# if(there_yes) {
#   for(i in 1:nrow(poll)) {
#     if (i >= si2) {
#       poll_date[i] <- poll_date[i] - 365
#     }
#   }
#   
# }

print(si2)
poll <- poll[1:si2,]
poll_date <- poll_date[1:si2]

poll <- cbind(poll,poll_date)

candidate = replicate(nrow(poll),candidatename)
poll <- cbind(poll,candidate)

# f = poll$Favorable
# poll$Favorable = as.numeric(levels(f))[f]
# 
# f = poll$Unavorable
# poll$Unavorable = as.numeric(levels(f))[f]



# #add the week number to each row so that we can analyze by week
# poll <- mutate(poll,start_date = as.Date("2016-01-01")) #add column holding first day of year
# week_no <- as.numeric(round(difftime(poll$poll_date, poll$start_date, units = "weeks"),digits = 0)) #calc week of year
# poll <- cbind(poll,week_no) #add week of year column to daily sentiment data frame
# 
# #calculate average sentiment by week
# fav <- poll %>% 
#   group_by(week_no) %>% 
#   summarise(fav_week  = round(mean(Favorable), digits = 0))
# unfav <- poll %>% 
#   group_by(week_no) %>% 
#   summarise(unfav_week  = round(mean(Unfavorable), digits = 0))
# 
# poll_weekly <- cbind(fav, unfav$unfav_week)
# colnames(poll_weekly) <- c("week_no","fav_pct","unfav_pct") #change the column names to be more legible
# str(poll_weekly)


#######
#p = list(poll,poll_date)
poll
}

pa <- list(dothis(4,4,5,"Trump","http://www.realclearpolitics.com/epolls/other/trump_favorableunfavorable-5493.html"),  dothis(2,4,5,"Sanders","http://www.realclearpolitics.com/epolls/other/sanders_favorableunfavorable-5263.html"), dothis(4,4,5,"Clinton","http://www.realclearpolitics.com/epolls/other/clinton_favorableunfavorable-1131.html"),
           dothis(2,4,5,"Kasich","http://www.realclearpolitics.com/epolls/other/kasich_favorableunfavorable-4260.html"),
           dothis(2,4,5,"Rubio","http://www.realclearpolitics.com/epolls/other/rubio_favorableunfavorable-3467.html"),
           dothis(2,4,5,"Cruz","http://www.realclearpolitics.com/epolls/other/cruz_favorableunfavorable-3887.html"),
           dothis(2,4,5,"Carson","http://www.realclearpolitics.com/epolls/other/carson_favorableunfavorable-5295.html"),
           dothis(2,4,5,"Christie","http://www.realclearpolitics.com/epolls/other/christie_favorableunfavorable-3471.html"),
           dothis(2,4,5,"O Malley","http://www.realclearpolitics.com/epolls/other/omalley_favorableunfavorable-3475.html"),
           dothis(2,4,5,"Stein","http://www.realclearpolitics.com/epolls/other/stein_favorableunfavorable-5979.html"),
           dothis(2,4,5,"Johnson","http://www.realclearpolitics.com/epolls/other/johnson_favorableunfavorable-5843.html"))
           


suppressWarnings(library(ggplot2))

#p=ggplot()

ploti = function (i) {  poll <- pa[[i]]
                        #poll_date <- pa[[i]][[2]]
                        
                        
                        #p <- p+ 
                        ggplot(poll, aes(as.Date(poll_date), y = value, color = variable)) + 
                          geom_line(aes(y = Favorable, col = "Favorable")) +
                          geom_line(aes(y = Unfavorable, col = "Unfavorable"))
}

for(i in 1:length(pa)) {
  ploti(i)
}
# ggplot(pa[[2]], aes(as.Date(poll_date), x = value, color = variable)) + 
#   geom_line(aes(y = Favorable, col = "Favorable")) +
#   geom_line(aes(y = Unfavorable, col = "Unfavorable"))


library(ggplot2)
library(reshape2)

# df = pa
# df_melt = melt(df, id.vars = 'poll_date')
# ggplot(df_melt, aes(x = date, y = value)) + 
#   geom_line() + 
#   facet_wrap(~ variable, scales = 'free_y', ncol = 1)


plot(pa[[1]]$poll_date,pa[[1]]$Favorable)
points(pa[[1]]$poll_date,pa[[1]]$Unfavorable)
points(pa[[2]]$poll_date,pa[[2]]$Favorable)
points(pa[[2]]$poll_date,pa[[2]]$Unfavorable)

d = rbind(pa[[1]],pa[[2]],pa[[3]],pa[[4]],pa[[5]],pa[[6]],pa[[7]],pa[[8]],pa[[9]],pa[[10]],pa[[11]])
ggplot(d, aes(as.Date(d$poll_date), y=value, color = variable)) + 
  geom_line(aes(y = Favorable, col = "Favorable")) +
  geom_line(aes(y = Unfavorable, col = "Unfavorable"))


f = d$Favorable
d$Favorable = as.numeric(levels(f))[f]

f = d$Unfavorable
d$Unfavorable = as.numeric(levels(f))[f]

d$gain = (d$Favorable - d$Unfavorable)/(d$Favorable + d$Unfavorable)*100
d$net = (d$Favorable - d$Unfavorable)

qplot(as.Date(poll_date),net, data=d, geom = c('smooth'),colour=candidate,method='loess',lwd=2,span=.5,se=FALSE)
#, method = "lm"

library(zoo)
