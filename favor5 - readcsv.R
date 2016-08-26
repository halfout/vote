a = read.csv("Poll2.csv")
b = read.csv("Poll4.csv")
e = read.csv("Poll8.csv")
g = read.csv("Poll3.csv")


require(ggplot2)
require(ggthemes)
require(Cairo)

# data munging

a1=a
b1=b

a1$votes = a1$Favorable
a1$type = "approval"
b1$votes=b1$Votes
b1$type = "first rank"

b1 = b1[,c("poll_date","votes","candidate","type")]
a1 = a1[,c("poll_date","votes","candidate","type")]

d = rbind(a1,b1)

e1=e
e1$type = "first rank"
#e1[["Start.Date"]] <- as.Date(e1[["Start.Date"]])
names(e1)[names(e1) == 'Start.Date'] = 'poll_date'
e1 = e1[,c("poll_date","votes","candidate","type")]
h = rbind(a1,e1)


g1=g
g1$type = "approval"
names(g1)[names(g1) == 'Start.Date'] = 'poll_date'
names(g1)[names(g1) == 'Favorable'] = 'votes'
g1 = g1[,c("poll_date","votes","candidate","type")]
m = rbind(g1,e1)

m3=m
d3=d
m3$source="Huffington Post"
d3$source="Real Clear Politics"
n = rbind(m3,d3)
write.csv(n, file = paste(file_loc, "Poll5.csv",sep = ""),row.names=TRUE, na="")

# todo next: add the other calculated variables, like "net" and "future"



# plots



svg(filename="figure/favor5-fig1-net-5.svg",
    width=8,
    height=5,
    pointsize=12)

a2 = a[a$candidate == "Trump" | a$candidate =="Clinton" | a$candidate =="Sanders" | a$candidate =="Kasich" | a$candidate =="Cruz",]
#  | a$candidate =="Carson" | a$candidate =="Rubio"
a2$candidate = factor(a2$candidate, levels = c("Kasich","Trump","Cruz","Sanders","Clinton"))
#a2$candidate = factor(a2$candidate, levels = c("Sanders","Kasich","Clinton","Trump","Cruz"))
qplot(as.Date(poll_date),net, data=a2, geom = c('point'),alpha=I(.5),colour=candidate,lwd=I(2),xlab="Date",ylab="Net (Yes - No)",main="Favorability - Nationwide (realclearpolitics.com)\nPeople Like Sanders & Kasich - They Hate Clinton, Cruz, & Trump") + theme_gray(base_size=13) +guides(colour = guide_legend(override.aes = list(size=4))) + theme(plot.title = element_text(size=16, face="bold",family="Times New Roman")) +  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE)+  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")
#span=.7,se=FALSE,
dev.off()


# do plot of favorability


#,method='loess',lwd=I(2),span=.7,se=FALSE
#a2$candidate = factor(a2$candidate, levels = c("Sanders","Clinton","Kasich","Carson","Rubio","Trump","Cruz"))



svg(filename="figure/favor5-fig1-likes-5.svg",
    width=5,
    height=4,
    pointsize=12)

a2 = a[a$candidate == "Trump" | a$candidate =="Clinton" | a$candidate =="Sanders" | a$candidate =="Kasich" | a$candidate =="Cruz",]
a2$candidate = factor(a2$candidate, levels = c("Kasich","Trump","Cruz","Sanders","Clinton"))
qplot(as.Date(a2$poll_date),Favorable, data=a2,ylim=c(0,75),geom = c('point'),alpha=I(.01),colour=candidate,xlab="Date",ylab="Favorability Rating %\n(realclearpolitics.com)",main="America Likes Sanders and Kasich Better\nThan Clinton and Trump") +  geom_smooth(method='loess',lwd=I(2),span=.55,se=FALSE)+guides(colour = guide_legend(override.aes = list(size=4))) + theme_igray() +  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")
#theme(plot.title = element_text(size=20, face="bold",family="Times New Roman"))


dev.off()

svg(filename="figure/favor5-fig1-future-5.svg",
    width=5,
    height=4,
    pointsize=12)

a2 = a[a$candidate == "Trump" | a$candidate =="Clinton" | a$candidate =="Sanders" | a$candidate =="Kasich" | a$candidate =="Cruz",]
a2$candidate = factor(a2$candidate, levels = c("Kasich","Trump","Cruz","Sanders","Clinton"))
qplot(as.Date(a2$poll_date),future, data=a2,ylim=c(0,75),geom = c('point'),alpha=I(.01),colour=candidate,xlab="Date",ylab="Favorability Rating %\n(realclearpolitics.com)",main="America Likes Sanders and Kasich Better\nThan Clinton and Trump") +  geom_smooth(method='loess',lwd=I(2),span=.55,se=FALSE)+guides(colour = guide_legend(override.aes = list(size=4))) + theme_igray() +  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")
#theme(plot.title = element_text(size=20, face="bold",family="Times New Roman"))


dev.off()

svg(filename="figure/favor5-fig1-likes-7.svg",
    width=7,
    height=5,
    pointsize=12)

a2 = a[a$candidate == "Trump" | a$candidate =="Clinton" | a$candidate =="Sanders" | a$candidate =="Kasich" | a$candidate =="Cruz" | a$candidate =="Rubio" | a$candidate =="Carson",]
a2$candidate = factor(a2$candidate, levels = c("Sanders","Kasich","Clinton", "Carson", "Rubio","Trump","Cruz"))
qplot(as.Date(poll_date),Favorable, data=a2,ylim=c(0,75),geom = c('point'),alpha=I(.5),colour=candidate,xlab="Date",ylab="Favorability Rating %\n(realclearpolitics.com)",main="America Likes Sanders and Kasich Better\nThan Clinton and Trump") +  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE)+guides(colour = guide_legend(override.aes = list(size=4))) + theme_igray()  +  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

#theme(plot.title = element_text(size=20, face="bold",family="Times New Roman"))


dev.off()



#d2 = d[d$candidate == "Trump" | d$candidate =="Clinton" | d$candidate =="Sanders" | d$candidate =="Kasich" | d$candidate =="Cruz",]
#d2$candidate = factor(d2$candidate, levels = c("Kasich","Trump","Cruz","Sanders","Clinton"))

d2 = d[d$candidate == "Trump" | d$candidate =="Clinton" | d$candidate =="Sanders" | d$candidate =="Kasich" | d$candidate =="Cruz" | d$candidate =="Rubio" | d$candidate =="Carson",]
d2$candidate = factor(d2$candidate, levels = c("Sanders","Kasich","Clinton", "Carson", "Rubio","Trump","Cruz"))

svg(filename="figure/favor5-fr-a.svg",
    width=7,
    height=4,
    pointsize=12)
qplot(as.Date(d2$poll_date),votes, data=d2,ylim=c(0,75),geom = c('point'),alpha=I(.01),colour=candidate,xlab="Date",ylab="Votes %\n(realclearpolitics.com)",main="America Likes Sanders and Kasich Better Than Clinton and Trump\nBut Democrats Voted Clinton And Republicans Voted Trump") +  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE)+guides(colour = guide_legend(override.aes = list(size=4))) + theme_igray()  +  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y") + facet_wrap(~ type) + theme(panel.margin=unit(2,"lines"))


dev.off()

#


m2 = m[m$candidate == "Trump" | m$candidate =="Clinton" | m$candidate =="Sanders" | m$candidate =="Kasich" | m$candidate =="Cruz" | m$candidate =="Rubio" | m$candidate =="Carson",]
m2$candidate = factor(m2$candidate, levels = c("Sanders","Kasich","Clinton", "Carson", "Rubio","Trump","Cruz"))

svg(filename="figure/favor5-fr-a-huff.svg",
    width=7,
    height=4,
    pointsize=12)
qplot(as.Date(m2$poll_date),votes, data=m2,ylim=c(0,75),geom = c('point'),alpha=I(.01),colour=candidate,xlab="Date",ylab="Votes %\n(Huffpo Pollster)",main="America Likes Sanders and Kasich Better Than Clinton and Trump\nBut Democrats Voted Clinton And Republicans Voted Trump") +  geom_smooth(method='loess',lwd=I(2),span=.9,se=FALSE)+guides(colour = guide_legend(override.aes = list(size=4))) + theme_igray()  +  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y") + facet_wrap(~ type) + theme(panel.margin=unit(2,"lines"))


dev.off()


# all candidates

can = c("Kasich", "Carson", "Rubio", "Cruz", "Trump","Walker","Paul","Christie","Bush","Huckabee","Santorum","Webb","Jindal")
d2 = d[d$candidate %in% can,]
d2 = d2[d$type=="approval",]
f=unique(d2[with(d2, order(-votes)), "candidate"])
d2$candidate = factor(d2$candidate, levels = f)
mean(d2$votes,na.rm=T)

mean(aggregate(d2$votes, by=list(d2$candidate), FUN=mean, na.rm=T)[[2]])

svg(filename="figure/favor5-republicans.svg",
    width=7,
    height=5,
    pointsize=12)
qplot(as.Date(d2$poll_date),votes, data=d2,ylim=c(0,75),geom = c('point'),alpha=I(.01),colour=candidate,xlab="Date",ylab="Votes %\n(realclearpolitics.com)",main="Republican Candidates Average Approval was at 27%") +  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE)+guides(colour = guide_legend(override.aes = list(size=4))) + theme_igray()  +  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y") 

dev.off()


# all candidates

can = c("Kasich", "Carson", "Rubio", "Cruz", "Trump","Walker","Paul","Christie","Bush","Huckabee","Santorum","Webb","Jindal")
d2 = d[d$candidate %in% can,]
d2 = d2[d$type=="approval",]
f3=unique(d2[with(d2, order(-votes)), "candidate"])
f3=na.omit(f3)
f2 = unique(e[,"candidate"])
f = c(as.character(f3), setdiff(as.character(f2),as.character(f3)))
h2 = h[h$candidate %in% can,]

h2$candidate = factor(h2$candidate, levels = f)
#mean(d2$votes,na.rm=T)

median(aggregate(e$votes, by=list(e$candidate), FUN=mean, na.rm=T)[[2]])

svg(filename="figure/favor5-republicans-fr-v-approve.svg",
    width=7,
    height=5,
    pointsize=12)
qplot(as.Date(h2$poll_date),votes, data=h2,ylim=c(0,75),geom = c('point'),alpha=I(.01),colour=candidate,xlab="Date",ylab="Votes %\n(realclearpolitics.com)",main="Republican Candidates Median Approval was at 29%\nBut Their Median Vote was at 5%") +  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE)+guides(colour = guide_legend(override.aes = list(size=4))) + theme_igray()  +  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y") + facet_wrap(~ type) + theme(panel.margin=unit(2,"lines"))

dev.off()



# repubicans from huffpo

can = c("Kasich", "Carson", "Rubio", "Cruz", "Trump","Walker","Paul","Christie","Bush","Huckabee","Santorum","Jindal","Perry","Fiorina","Gilmore","Graham","Pataki")
m2 = m[m$candidate %in% can,]
g2 = g1[as.Date(g1$poll_date) > as.Date("2015-01-01"),]
f2 = aggregate(g2$votes, by=list(g2$candidate), FUN=mean, na.rm=T)
f=unique(f2[with(f2, order(-x)), 1])
#f=unique(m2[with(m2, order(-votes)), "candidate"])
#f=unique(g2[with(g2, order(-votes)), "candidate"])

m2$candidate = factor(m2$candidate, levels = f)
#mean(d2$votes,na.rm=T)

median(aggregate(e1$votes, by=list(e1$candidate), FUN=mean, na.rm=T)[[2]])
median(aggregate(g1$votes, by=list(g1$candidate), FUN=mean, na.rm=T)[[2]])

svg(filename="figure/favor5-republicans-fr-v-approve-huff.svg",
    width=7,
    height=5,
    pointsize=12)
qplot(as.Date(m2$poll_date),votes, data=m2,ylim=c(0,75),geom = c('point'),alpha=I(.01),colour=candidate,xlab="Date",ylab="Votes %\n(source: HuffPo Pollster)",main="Republican Candidates Median Approval was at 28%\nBut Their Median Vote was at 5%") +  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE)+guides(colour = guide_legend(override.aes = list(size=4))) + theme_igray()  +  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y") + facet_wrap(~ type) + theme(panel.margin=unit(2,"lines"))

dev.off()



# all (7) candidates from huffpo

can = c("Kasich", "Carson", "Rubio", "Cruz", "Trump","Clinton","Sanders")
m2 = m[m$candidate %in% can,]
g2 = g1[as.Date(g1$poll_date) > as.Date("2015-01-01"),]
f2 = aggregate(g2$votes, by=list(g2$candidate), FUN=mean, na.rm=T)
f=unique(f2[with(f2, order(-x)), 1])
#f=unique(m2[with(m2, order(-votes)), "candidate"])
#f=unique(g2[with(g2, order(-votes)), "candidate"])

m2$candidate = factor(m2$candidate, levels = f)
#mean(d2$votes,na.rm=T)

median(aggregate(e1$votes, by=list(e1$candidate), FUN=mean, na.rm=T)[[2]])
median(aggregate(g1$votes, by=list(g1$candidate), FUN=mean, na.rm=T)[[2]])

svg(filename="figure/favor5-all-r-and-d-fr-v-approve-huff.svg",
    width=7,
    height=5,
    pointsize=12)
qplot(as.Date(m2$poll_date),votes, data=m2,ylim=c(0,75),geom = c('point'),alpha=I(.01),colour=candidate,xlab="Date",ylab="Votes %\n(HuffPo Pollster)",main="temp title") +  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE)+guides(colour = guide_legend(override.aes = list(size=4))) + theme_igray()  +  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y") + facet_wrap(~ type) + theme(panel.margin=unit(2,"lines"))

dev.off()






# bar charts - involves a lot of averaging and sorting

if(F) { # <> different time periods
  fromDate = "2016-02-01"
  ylabel = "Votes %\nAverage Over Primary Season, Feb-Jun"
  fn1 = "figure/favor5-republicans-fr-v-approve-huff-bar.svg"
} else {
  fromDate = "2016-03-01"
  ylabel = "Votes %\nAverage Over Primary Season, Mar-Jun"
  fn1 = "figure/favor5-republicans-fr-v-approve-huff-bar-mar.svg"
}

can = c("Kasich", "Carson", "Rubio", "Cruz", "Trump","Walker","Paul","Christie","Bush","Huckabee","Santorum","Jindal","Perry","Fiorina","Gilmore","Graham","Pataki")
#can=c("Bush","Rubio")
m2 = m[m$candidate %in% can,]

if(T) {
  g2 = g1[as.Date(g1$poll_date) > as.Date("2015-01-01"),]
  f3 = aggregate(g2$votes, by=list(g2$candidate), FUN=mean, na.rm=T)
  f=unique(f3[with(f3, order(-x)), 1])
  m2$candidate = factor(m2$candidate, levels = f)
} 

m2 = m2[as.Date(m2$poll_date) > as.Date(fromDate),]  # <>
m2 = m2[as.Date(m2$poll_date) < as.Date("2016-06-14"),]  # try changing this

f2 = aggregate(m2$votes, by=list(m2$candidate,m2$type), FUN=mean, na.rm=T)

names(f2)[names(f2) == 'Group.1'] = 'candidate'
names(f2)[names(f2) == 'Group.2'] = 'type'
names(f2)[names(f2) == 'x'] = 'votes'

#f2$ord = with(f2,order(type,-votes))


f2$ranks<-with(f2, ave(votes, type, FUN=function(x) rank(-x)))

f=unique(f2[with(f2, order(type,-votes)), 1])
#f=unique(f2[with(f2[f2$type=="approval",], order(-votes)), 1])
f2$candidate = factor(f2$candidate, levels = f)

svg(filename=fn1,
    width=7,
    height=5,
    pointsize=12)
ggplot(f2, aes(x=ranks,y=votes,fill=candidate))+geom_bar(stat="identity") + facet_wrap(~ type) + theme_igray(base_size=15) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + labs(title = "Vote-Splitting Skews the Vote Count") + ylab(ylabel) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
dev.off()



# table
library(gridExtra)
svg("figure/favor5-republicans-table.svg", height=3.5, width=3)
f4=xtabs(round(votes,1) ~ candidate + type,f2,sparse=T)
f4[f4 == 0] <- NA
grid.table(f4)
dev.off()

# try another table
# library(gplots)
# mm <- as.matrix(f4, ncol = 2)
# 
# heatmap.2(x = mm, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
#           cellnote = mm, notecol = "black", notecex = 2,
#           trace = "none", key = FALSE, margins = c(7, 11))
# 
# 
# library(plotrix)
# 
# # while plotrix is loaded anyway:
# # set colors with color.scale
# # need data as matrix*
# testdf=f4
# mm <- as.matrix(testdf, ncol = 2)
# cols <- color.scale(mm, extremes = c("red", "yellow"))
# 
# par(mar = c(0.5, 1, 2, 0.5))
# # create empty plot
# plot(1:10, axes = FALSE, xlab = "", ylab = "", type = "n")
# 
# # add table
# addtable2plot(x = 1, y = 1, table = testdf,
#               bty = "o", display.rownames = TRUE,
#               hlines = TRUE, vlines = TRUE,
#               bg = cols,
#               xjust = 2, yjust = 1, cex = 3)

# future approval

can = c("Kasich", "Carson", "Rubio", "Cruz", "Trump","Walker","Paul","Christie","Bush","Huckabee","Santorum","Webb","Jindal")

a2 = a[a$candidate %in% can,]
f=unique(a2[with(a2, order(-future)), "candidate"])
a2$candidate = factor(a2$candidate, levels = f)

svg(filename="figure/favor5-republicans-future.svg",
    width=7,
    height=5,
    pointsize=12)
qplot(as.Date(a2$poll_date),future, data=a2,ylim=c(0,75),geom = c('point'),alpha=I(.01),colour=candidate,xlab="Date",ylab="Upvotes / (Upvotes+Downvotes) %\n(realclearpolitics.com)",main="America Likes Sanders and Kasich Better Than Clinton and Trump\nBut Democrats Voted Clinton And Republicans Voted Trump") +  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE)+guides(colour = guide_legend(override.aes = list(size=4))) + theme_igray()  +  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y") 

dev.off()

