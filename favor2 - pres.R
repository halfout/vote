file_loc <- "./"
library(stringr)
require(XML)

library(ggplot2)
library(ggthemes)
library(reshape2)
library(Cairo)
library(cairoDevice)


ng = list(
  list(2,4,5,"Sanders","http://www.realclearpolitics.com/epolls/other/sanders_favorableunfavorable-5263.html"), 
  list(2,4,5,"Kasich","http://www.realclearpolitics.com/epolls/other/kasich_favorableunfavorable-4260.html"),
  list(2,4,5,"Carson","http://www.realclearpolitics.com/epolls/other/carson_favorableunfavorable-5295.html"),
  list(2,4,5,"Rubio","http://www.realclearpolitics.com/epolls/other/rubio_favorableunfavorable-3467.html"),
  list(4,4,5,"Clinton","http://www.realclearpolitics.com/epolls/other/clinton_favorableunfavorable-1131.html"),
  list(2,4,5,"Cruz","http://www.realclearpolitics.com/epolls/other/cruz_favorableunfavorable-3887.html"),
  list(4,4,5,"Trump","http://www.realclearpolitics.com/epolls/other/trump_favorableunfavorable-5493.html"),
list(2,4,5,"Walker","http://www.realclearpolitics.com/epolls/other/walker_favorableunfavorable-3570.html"),
list(4,4,5,"Romney","http://www.realclearpolitics.com/epolls/other/romney_favorableunfavorable-1134.html"),
list(2,4,5,"Ryan","http://www.realclearpolitics.com/epolls/other/ryan_favorableunfavorable-3468.html"),
list(2,4,5,"Paul","http://www.realclearpolitics.com/epolls/other/paul_favorableunfavorable-3716.html"),
list(2,4,5,"Christie","http://www.realclearpolitics.com/epolls/other/christie_favorableunfavorable-3471.html"),
list(2,4,5,"Bush","http://www.realclearpolitics.com/epolls/other/bush_favorableunfavorable-3470.html"),
list(2,4,5,"O Malley","http://www.realclearpolitics.com/epolls/other/omalley_favorableunfavorable-3475.html"),
list(2,4,5,"Stein","http://www.realclearpolitics.com/epolls/other/stein_favorableunfavorable-5979.html"),
list(2,4,5,"Johnson","http://www.realclearpolitics.com/epolls/other/johnson_favorableunfavorable-5843.html"),
list(2,4,5,"Huckabee","http://www.realclearpolitics.com/epolls/other/huckabee_favorableunfavorable-1132.html"),
list(2,4,5,"Walker","http://www.realclearpolitics.com/epolls/other/walker_favorableunfavorable-3570.html"),
list(2,4,5,"Santorum","http://www.realclearpolitics.com/epolls/other/santorum_favorableunfavorable-3104.html"),
list(1,4,5,"Warren","http://www.realclearpolitics.com/epolls/other/warren_favorableunfavorable-4261.html"),
list(2,4,5,"Webb","http://www.realclearpolitics.com/epolls/other/webb_favorableunfavorable-5264.html"),
list(2,4,5,"Jindal","http://www.realclearpolitics.com/epolls/other/jindal_favorableunfavorable-3472.html"),
list(2,4,5,"Cuomo","http://www.realclearpolitics.com/epolls/other/cuomo_favorableunfavorable-3474.html"))

# Fiorina, Pataki, Perry

g=length(ng)

empty = data.frame("Poll" = character(), "Date" = character(), "Sample" = character(),  "Favorable" = numeric(),  "Unfavorable" = numeric(), "Spread" = factor(),  "poll_date"=factor())
pa = empty

for(i in 1:g) {
  ing = ng[[i]]
  a = ing[[1]]
  b = ing[[2]]
  c = ing[[3]]
  candidatename = ing[[4]]
  myURL = ing[[5]]
  
  tables <- readHTMLTable(myURL) # read the poll web page
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))  #format to table
  poll <- tables[[a]]
  
  poll <- poll[-1,]   #remove top line which is a summary of entire period
  colnames(poll)[b:c] <- c("Favorable","Unfavorable") # remove periods from these column names
  
  #convert the poll dates to r dates
  poll_date <- substr(poll$Date,1,str_locate(poll$Date,"-")-2)  
  poll_date <- as.Date(poll_date,"%m/%d")  # ** not scalable ** breaks at year boundary
  
  #plot(poll_date)
  #title(main = candidatename)
  
  jun7 = as.Date("7/7","%m/%d")
  
  years_back = 0
  if(candidatename=="Walker" || candidatename=="Paul" || candidatename=="Romney" || candidatename=="Huckabee" || candidatename=="Santorum" || candidatename=="Webb" || candidatename=="Jindal")
    years_back = 1
  
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
  if(candidatename=="Ryan"){
    inde = which(p2 == "2016-10-14")
    p2[inde] = p2[inde] -365
  }
  poll_date = p2
  
  #plot(poll_date)
  #title(main = candidatename)
  
  
  print(si2)
  poll <- poll[1:si2,]
  poll_date <- poll_date[1:si2]
  
  poll <- cbind(poll,poll_date)
  
  candidate = replicate(nrow(poll),candidatename)
  poll <- cbind(poll,candidate)
  
  pa = rbind(pa,poll)
}


d=pa


f = d$Favorable
d$Favorable = as.numeric(levels(f))[f]

f = d$Unfavorable
d$Unfavorable = as.numeric(levels(f))[f]



#d$gain = (d$Favorable - d$Unfavorable)/(d$Favorable + d$Unfavorable)*100
d$future = d$Favorable/(d$Favorable + d$Unfavorable)*100
d$net = (d$Favorable - d$Unfavorable)


write.csv(d, file = paste(file_loc, "Poll2.csv",sep = ""),row.names=TRUE, na="") 