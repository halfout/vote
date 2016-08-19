
# <> This symbol indicates places in the code to change things


require(ggplot2)
require(ggthemes)

a=read.csv("input_favor.csv")
b=read.csv("input_versus.csv")

i=11  # <>  1 through 12, some together
# 4 and 5 are together
#i=c(4,5)
#i=c(7,8)
#i=c(10,12)
# <>

#for(i in 1:3) {
  #ggplot(a,aes(x=candidate,y=Favorable,fill=field))+geom_bar(stat="identity",position="dodge") + theme_igray() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  #ggplot(b,aes(x=candidate,y=votes,fill=field))+geom_bar(stat="identity") + theme_igray() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(~ field)
  
  a$votes = a$Favorable
  b$race = paste("match:",b$versus,"    voters:",b$field,sep="")
  
  b1 = b[b$poll_id %in% i,c("poll_date","state","votes","candidate","party","race")]
  a1 = a[a$poll_id %in% i & a$field == "all",c("poll_date","state","votes","candidate","party","not.sure")]
  
  b1$type = "first rank"
  b1$not.sure=0
  
  if(nrow(a1)==0) { # no approval poll to go with the first-rank poll
    d = b1
  } else {
    a1$type = "approval"
    a1$race = "approval"
    d = rbind(a1,b1)
  }
  if (0){
    # sort by party (and then maybe votes)
    e=unique(d[,c("candidate", "party")])  
    #http://stackoverflow.com/questions/9944816/unique-on-a-dataframe-with-only-selected-columns
    f=unique(e[with(e, order(party,candidate)), "candidate"])
    f=unique(d[with(d, order(party,-votes,candidate)), "candidate"])
    # move not sure to the end
    ns = c("not sure - D","not sure - R")
    x <- setdiff(f, ns)
    x <- append(x, values = ns)
    f=x
    d$candidate = factor(d$candidate, levels = f)
  } else {
    if (0) { # <> 
      oldd=d
      d = d[-which(d$candidate=="not sure"),]
    }
    d$candidate = paste( d$candidate, d$party, sep = " - ")
    if (1) {# customize
      if(9 %in% i) {
        f=unique(d[with(d, order(-votes,candidate)), "candidate"])
      } else {
        f=unique(d[with(d, order(party,-votes,candidate)), "candidate"])
      }
    }
    if (0) {
      ns = c("not sure - D","not sure - R")
      x <- setdiff(f, ns)
      x <- append(x, values = ns)
      f=x
    } 
    if (1) {
      ns = c("not sure - A")
      x <- setdiff(f, ns)
      x <- append(x, values = ns)
      f=x
    }
    d$candidate = factor(d$candidate, levels = f)
  }
  
  if (1) { # make R red, D blue
    g = unique(d$party)
    nl = length(g)
    if (nl==2) g = c("R","D")
    else if (nl==3) g = c("R","A","D")
    else if (nl==4) g = c("R","A","D","I")
    else if (nl>4) {
      g = setdiff(g, "R")
      g = append("R",g)
      g = setdiff(g, "D")
      if(nl>3)
      {
        g = append(g,"D",after=nl-2)
      } else {
        g = append(g,"D",after=nl-1)
      }
    }
    d$party = factor(d$party, levels = g)
  } else {
    d$party = factor(d$party, levels = sort(levels(d$party),decreasing=T))
  }
  
  mtemp = min(which(b$poll_id %in% i))
  
  if (1) {
    title1 = b[mtemp,"state"]
    upol = unique(d$poll_date)
    for(pol in upol){
      title1 = paste(title1,pol,sep=' - ')
    }
    
  } else {
    title1 = paste(b[mtemp,"state"],b[mtemp,"poll_date"],sep = " - ")
    
    if (length(i) > 1) {
      title1 = paste(title1," and more dates")
    }
  }
  ggplot(d, aes(x=candidate,y=votes,fill=party))+geom_bar(stat="identity",position="dodge") + facet_wrap(~ race) + theme_gray() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = title1) + ylab("Votes %") + xlab("Candidate")
  #+ geom_errorbar(aes(ymin = votes, ymax = votes + not.sure))
  # old way #  + facet_wrap(~ type + race)
  # <> "not sure" bar is optional
#}