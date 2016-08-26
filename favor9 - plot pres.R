
# todo next: add the other calculated variables, like "net" and "future"
# 

require(ggplot2)
require(ggthemes)
require(Cairo)
require(rsvg)
library(extrafont)
if (0) {
  font_import(pattern="[T/t]imes")
  font_import(pattern="[G/g]eorgia")
  fonts()
}
loadfonts(device="win")


# in future, separate this file from the other one
file_loc = "./"
n = read.csv(file = paste(file_loc, "Poll5.csv",sep = ""))
n$poll_date = as.Date(n$poll_date)


a = with(n, n[source == "Real Clear Politics" & type == "approval",])
anet = with(n, n[source == "Real Clear Politics" & type == "net",])
s = with(n, n[source == "Real Clear Politics" & type == "future",])
b = with(n, n[source == "Real Clear Politics" & type == "first rank",])
d = with(n, n[source == "Real Clear Politics" & (type == "first rank" | type == "approval"),])
g = with(n, n[source == "Huffington Post" & type == "approval",])
e = with(n, n[source == "Huffington Post" & type == "first rank",])
m = with(n, n[source == "Huffington Post",])
e1=e
g1=g
# h is r and e
h = with(n, n[(source == "Real Clear Politics" & type == "approval") | (source == "Huffington Post" & type == "first rank"),])


# plots


plot_style_1 = function (p) {
  if (0) {
    p +
      theme_igray(base_size=13) +
      guides(colour = guide_legend(override.aes = list(size=4))) + 
      theme(panel.margin=unit(2,"lines")) +
      theme(plot.background = element_rect(fill = "beige")) + 
      theme(legend.background = element_rect(fill = "beige")) + 
      scale_color_brewer(palette="Set1") +
      theme(plot.title = element_text(size=16, face="bold",family="Georgia"))
  } else {
    p +
      theme_igray(base_size=13) +
      guides(colour = guide_legend(override.aes = list(size=4))) + 
      theme(panel.margin=unit(2,"lines"))
  }
}


filename = "figure/favor5-fig1-net-5"
svg(filename=paste0(filename,".svg"),
    width=8,
    height=5,
    pointsize=12)

can5 = c("Kasich","Trump","Cruz","Sanders","Clinton")
can5 = c("Sanders","Kasich","Clinton","Trump","Cruz")
a2 = anet[ anet$candidate %in% can5 ,]
a2$candidate = factor(a2$candidate, levels = can5)
a2$candidate = factor(a2$candidate, levels = can5)

p = ggplot(a2, aes(x=poll_date,y=votes, color=candidate)) +
        geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE)+ 
        geom_point(alpha=I(.5),lwd=I(2)) + 
        labs(title = "Favorability - Nationwide (realclearpolitics.com)\nPeople Like Sanders & Kasich - They Hate Clinton, Cruz, & Trump") + 
        ylab("Net (Yes - No)") + 
        xlab("Date")   + 
        scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

plot_style_1(p)
       
#qplot(poll_date,net, data=a2, geom = c('point'),alpha=I(.5),colour=candidate,lwd=I(2),xlab="Date",ylab="Net (Yes - No)",main="Favorability - Nationwide (realclearpolitics.com)\nPeople Like Sanders & Kasich - They Hate Clinton, Cruz, & Trump") + theme_gray(base_size=13) +guides(colour = guide_legend(override.aes = list(size=4))) + theme(plot.title = element_text(size=16, face="bold",family="Times New Roman")) +  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE)+  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")  + theme(plot.background = element_rect(fill = "beige")) + theme(legend.background = element_rect(fill = "beige")) + scale_fill_brewer(palette="Dark2")
#span=.7,se=FALSE,
dev.off()
rsvg_png(paste0(filename,".svg"),paste0(filename,".png"))






# do plot of favorability


#,method='loess',lwd=I(2),span=.7,se=FALSE
#a2$candidate = factor(a2$candidate, levels = c("Sanders","Clinton","Kasich","Carson","Rubio","Trump","Cruz"))


filename = "figure/favor5-fig1-likes-5"
svg(filename=paste0(filename,".svg"),
    width=5,
    height=4,
    pointsize=12)

# filter and sort
can5 = c("Kasich","Trump","Cruz","Sanders","Clinton")
can5 = c("Sanders","Kasich","Clinton","Trump","Cruz")
a2 = a[a$candidate %in% can5,]
a2$candidate = factor(a2$candidate, levels = can5)

p = ggplot(a2, aes(x=poll_date,y=votes, color=candidate)) + 
        ylim(0,75) +
        labs(title = "America Likes Sanders and Kasich Better\nThan Clinton and Trump") + 
        ylab("Favorability Rating %\n(realclearpolitics.com)") + 
        xlab("Date") +
        geom_smooth(method='loess',lwd=I(2),span=.55,se=FALSE) +
        scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

plot_style_1(p)

#qplot(as.Date(a2$poll_date),Favorable, data=a2,ylim=c(0,75),geom = c('point'),alpha=I(.01),colour=candidate,xlab="Date",ylab="Favorability Rating %\n(realclearpolitics.com)",main="America Likes Sanders and Kasich Better\nThan Clinton and Trump") +  geom_smooth(method='loess',lwd=I(2),span=.55,se=FALSE)+guides(colour = guide_legend(override.aes = list(size=4))) + theme_igray() +  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")
#theme(plot.title = element_text(size=20, face="bold",family="Times New Roman"))


dev.off()
rsvg_png(paste0(filename,".svg"),paste0(filename,".png"))



filename = "figure/favor5-fig1-future-5"
svg(filename=paste0(filename,".svg"),
    width=5,
    height=4,
    pointsize=12)


# filter and sort
can5 = c("Trump","Clinton","Sanders","Kasich","Cruz")
can5 = c("Kasich","Trump","Cruz","Sanders","Clinton")
s2 = s[s$candidate %in% can5,]
s2$candidate = factor(s2$candidate, levels = can5)


# need to get future

p = ggplot(s2, aes(x=poll_date,y=votes, color=candidate)) + 
  ylim(0,75) +
  labs(title = "America Likes Sanders and Kasich Better\nThan Clinton and Trump") + 
  ylab("Favorability Rating %\n(realclearpolitics.com)") + 
  xlab("Date") +
  geom_smooth(method='loess',lwd=I(2),span=.55,se=FALSE) +
  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

plot_style_1(p)

#qplot(as.Date(a2$poll_date),future, data=a2,ylim=c(0,75),geom = c('point'),alpha=I(.01),colour=candidate,xlab="Date",ylab="Favorability Rating %\n(realclearpolitics.com)",main="America Likes Sanders and Kasich Better\nThan Clinton and Trump") +  geom_smooth(method='loess',lwd=I(2),span=.55,se=FALSE)+guides(colour = guide_legend(override.aes = list(size=4))) + theme_igray() +  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")
#theme(plot.title = element_text(size=20, face="bold",family="Times New Roman"))


dev.off()
rsvg_png(paste0(filename,".svg"),paste0(filename,".png"))




filename = "figure/favor5-fig1-likes-7"
svg(filename=paste0(filename,".svg"),
    width=7,
    height=5,
    pointsize=12)

# filter and sort
can7 = c("Sanders","Kasich","Clinton","Carson","Rubio","Trump","Cruz")
can7 = c("Sanders","Kasich","Clinton","Trump","Cruz","Carson","Rubio")
a2 = a[a$candidate %in% can7,]
a2$candidate = factor(a2$candidate, levels = can7)

p = ggplot(a2, aes(x=poll_date,y=votes, color=candidate)) + 
  ylim(0,75) +
  labs(title = "America Likes Sanders and Kasich Better\nThan Clinton and Trump") + 
  ylab("Favorability Rating %\n(realclearpolitics.com)") + 
  xlab("Date") +
  geom_point(alpha=I(.5),lwd=I(2)) +
  geom_smooth(method='loess',lwd=I(2),span=.55,se=FALSE)+ 
  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

plot_style_1(p)

#qplot(poll_date,Favorable, data=a2,ylim=c(0,75),geom = c('point'),alpha=I(.5),colour=candidate,xlab="Date",ylab="Favorability Rating %\n(realclearpolitics.com)",main="America Likes Sanders and Kasich Better\nThan Clinton and Trump") +  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE)+guides(colour = guide_legend(override.aes = list(size=4))) + theme_igray()  +  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

#theme(plot.title = element_text(size=20, face="bold",family="Times New Roman"))


dev.off()
rsvg_png(paste0(filename,".svg"),paste0(filename,".png"))







can5 = c("Kasich","Trump","Cruz","Sanders","Clinton")
d2 = d[d$candidate %in% can5,]
d2$candidate = factor(d2$candidate, levels = can5)

filename = "figure/favor5-fr-a"
svg(filename=paste0(filename,".svg"),
    width=7,
    height=4,
    pointsize=12)

p = ggplot(d2, aes(x=poll_date,y=votes, color=candidate)) + 
  ylim(0,75) +
  labs(title = "America Likes Sanders and Kasich Better Than Clinton and Trump\nBut Democrats Voted Clinton And Republicans Voted Trump") + 
  ylab("Votes %\n(realclearpolitics.com)") + 
       xlab("Date") +
       geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE) +
       facet_wrap(~ type) + 
       scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

plot_style_1(p)

#qplot(as.Date(d2$poll_date),votes, data=d2,ylim=c(0,75),geom = c('point'),alpha=I(.01),colour=candidate,xlab="Date",ylab="Votes %\n(realclearpolitics.com)",main="America Likes Sanders and Kasich Better Than Clinton and Trump\nBut Democrats Voted Clinton And Republicans Voted Trump") +  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE)+guides(colour = guide_legend(override.aes = list(size=4))) + theme_igray()  +  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y") + facet_wrap(~ type) + theme(panel.margin=unit(2,"lines"))


dev.off()
rsvg_png(paste0(filename,".svg"),paste0(filename,".png"))





#


m2 = m[m$candidate == "Trump" | m$candidate =="Clinton" | m$candidate =="Sanders" | m$candidate =="Kasich" | m$candidate =="Cruz" | m$candidate =="Rubio" | m$candidate =="Carson",]
m2$candidate = factor(m2$candidate, levels = c("Sanders","Kasich","Clinton", "Carson", "Rubio","Trump","Cruz"))

filename = "figure/favor5-fr-a-huff"
svg(filename=paste0(filename,".svg"),
    width=7,
    height=4,
    pointsize=12)
p = ggplot(m2, aes(x=poll_date,y=votes, color=candidate)) + 
  ylim(0,75) +
  labs(title = "America Likes Sanders and Kasich Better Than Clinton and Trump\nBut Democrats Voted Clinton And Republicans Voted Trump") + 
  ylab("Votes %\n(HuffPo Pollster)") + 
  xlab("Date") +
  geom_smooth(method='loess',lwd=I(2),span=.9,se=FALSE) +
  facet_wrap(~ type) + 
  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

plot_style_1(p)

#qplot(as.Date(m2$poll_date),votes, data=m2,ylim=c(0,75),geom = c('point'),alpha=I(.01),colour=candidate,xlab="Date",ylab="Votes %\n(Huffpo Pollster)",main="America Likes Sanders and Kasich Better Than Clinton and Trump\nBut Democrats Voted Clinton And Republicans Voted Trump") +  geom_smooth(method='loess',lwd=I(2),span=.9,se=FALSE)+guides(colour = guide_legend(override.aes = list(size=4))) + theme_igray()  +  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y") + facet_wrap(~ type) + theme(panel.margin=unit(2,"lines"))


dev.off()
rsvg_png(paste0(filename,".svg"),paste0(filename,".png"))






# all candidates

can = c("Kasich", "Carson", "Rubio", "Cruz", "Trump","Walker","Paul","Christie","Bush","Huckabee","Santorum","Webb","Jindal")
d2 = d[d$candidate %in% can,]
d2 = d2[d$type=="approval",]
f=unique(d2[with(d2, order(-votes)), "candidate"])
d2$candidate = factor(d2$candidate, levels = f)
mean(d2$votes,na.rm=T)

mean(aggregate(d2$votes, by=list(d2$candidate), FUN=mean, na.rm=T)[[2]])

filename = "figure/favor5-republicans"
svg(filename=paste0(filename,".svg"),
    width=7,
    height=5,
    pointsize=12)

p = ggplot(d2, aes(x=poll_date,y=votes, color=candidate)) + 
  ylim(0,75) +
  labs(title = "Republican Candidates Average Approval was at 27%") + 
  ylab("Votes %\n(realclearpolitics.com)") + 
  xlab("Date") +
  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE) +
  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

plot_style_1(p)

#qplot(as.Date(d2$poll_date),votes, data=d2,ylim=c(0,75),geom = c('point'),alpha=I(.01),colour=candidate,xlab="Date",ylab="Votes %\n(realclearpolitics.com)",main="Republican Candidates Average Approval was at 27%") +  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE)+guides(colour = guide_legend(override.aes = list(size=4))) + theme_igray()  +  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y") 

dev.off()
rsvg_png(paste0(filename,".svg"),paste0(filename,".png"))





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

filename = "figure/favor5-republicans-fr-v-approve"
svg(filename=paste0(filename,".svg"),
    width=7,
    height=5,
    pointsize=12)

p = ggplot(h2, aes(x=poll_date,y=votes, color=candidate)) + 
  ylim(0,75) +
  labs(title = "Republican Candidates Median Approval was at 29%\nBut Their Median Vote was at 5%") + 
  ylab("Votes %\n(realclearpolitics.com)") + 
  xlab("Date") +
  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE) +
  facet_wrap(~ type) + 
  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

plot_style_1(p)

#qplot(as.Date(h2$poll_date),votes, data=h2,ylim=c(0,75),geom = c('point'),alpha=I(.01),colour=candidate,xlab="Date",ylab="Votes %\n(realclearpolitics.com)",main="Republican Candidates Median Approval was at 29%\nBut Their Median Vote was at 5%") +  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE)+guides(colour = guide_legend(override.aes = list(size=4))) + theme_igray()  +  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y") + facet_wrap(~ type) + theme(panel.margin=unit(2,"lines"))

dev.off()
rsvg_png(paste0(filename,".svg"),paste0(filename,".png"))






# all candidates from huffpo

can = c("Kasich", "Carson", "Rubio", "Cruz", "Trump","Walker","Paul","Christie","Bush","Huckabee","Santorum","Jindal","Perry","Fiorina","Gilmore","Graham","Pataki")
m2 = m[m$candidate %in% can,]
g2 = g1[g1$poll_date > as.Date("2015-01-01"),]
f2 = aggregate(g2$votes, by=list(g2$candidate), FUN=mean, na.rm=T)
f=unique(f2[with(f2, order(-x)), 1])
#f=unique(m2[with(m2, order(-votes)), "candidate"])
#f=unique(g2[with(g2, order(-votes)), "candidate"])

m2$candidate = factor(m2$candidate, levels = f)
#mean(d2$votes,na.rm=T)

median(aggregate(e1$votes, by=list(e1$candidate), FUN=mean, na.rm=T)[[2]])
median(aggregate(g1$votes, by=list(g1$candidate), FUN=mean, na.rm=T)[[2]])

filename = "figure/favor5-republicans-fr-v-approve-huff"
svg(filename=paste0(filename,".svg"),
    width=7,
    height=5,
    pointsize=12)


p = ggplot(m2, aes(x=poll_date,y=votes, color=candidate)) + 
  ylim(0,75) +
  labs(title = "Republican Candidates Median Approval was at 28%\nBut Their Median Vote was at 5%") + 
  ylab("Votes %\n(HuffingtonPost/Pollster)") + 
  xlab("Date") +
  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE) +
  facet_wrap(~ type) + 
  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

plot_style_1(p)

#qplot(as.Date(m2$poll_date),votes, data=m2,ylim=c(0,75),geom = c('point'),alpha=I(.01),colour=candidate,xlab="Date",ylab="Votes %\n(realclearpolitics.com)",main="Republican Candidates Median Approval was at 28%\nBut Their Median Vote was at 5%") +  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE)+guides(colour = guide_legend(override.aes = list(size=4))) + theme_igray()  +  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y") + facet_wrap(~ type) + theme(panel.margin=unit(2,"lines"))

dev.off()
rsvg_png(paste0(filename,".svg"),paste0(filename,".png"))






# all (7) candidates from huffpo

can = c("Kasich", "Carson", "Rubio", "Cruz", "Trump","Clinton","Sanders")
m2 = m[m$candidate %in% can,]
g2 = g1[g1$poll_date > as.Date("2015-01-01"),]
f2 = aggregate(g2$votes, by=list(g2$candidate), FUN=mean, na.rm=T)
f=unique(f2[with(f2, order(-x)), 1])
#f=unique(m2[with(m2, order(-votes)), "candidate"])
#f=unique(g2[with(g2, order(-votes)), "candidate"])

m2$candidate = factor(m2$candidate, levels = f)
#mean(d2$votes,na.rm=T)

median(aggregate(e1$votes, by=list(e1$candidate), FUN=mean, na.rm=T)[[2]])
median(aggregate(g1$votes, by=list(g1$candidate), FUN=mean, na.rm=T)[[2]])

filename = "figure/favor5-all-r-and-d-fr-v-approve-huff"
svg(filename=paste0(filename,".svg"),
    width=7,
    height=5,
    pointsize=12)

p = ggplot(m2, aes(x=poll_date,y=votes, color=candidate)) + 
  ylim(0,75) +
  labs(title = "temp title") + 
  ylab("Votes %\n(HuffPo Pollster)") + 
  xlab("Date") +
  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE) +
  facet_wrap(~ type) + 
  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

plot_style_1(p)

#qplot(as.Date(m2$poll_date),votes, data=m2,ylim=c(0,75),geom = c('point'),alpha=I(.01),colour=candidate,xlab="Date",ylab="Votes %\n(HuffPo Pollster)",main="temp title") +  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE)+guides(colour = guide_legend(override.aes = list(size=4))) + theme_igray()  +  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y") + facet_wrap(~ type) + theme(panel.margin=unit(2,"lines"))


dev.off()
rsvg_png(paste0(filename,".svg"),paste0(filename,".png"))







# bar charts - involves a lot of averaging and sorting

flag.time = T

if(flag.time) { # <> different time periods
  fromDate = "2016-02-01"
  ylabel = "Votes %\nAverage Over Primary Season, Feb-Jun"
  fn1 = "figure/favor5-republicans-fr-v-approve-huff-bar"
} else {
  fromDate = "2016-03-01"
  ylabel = "Votes %\nAverage Over Primary Season, Mar-Jun"
  fn1 = "figure/favor5-republicans-fr-v-approve-huff-bar-mar"
}

can = c("Kasich", "Carson", "Rubio", "Cruz", "Trump","Walker","Paul","Christie","Bush","Huckabee","Santorum","Jindal","Perry","Fiorina","Gilmore","Graham","Pataki")
#can=c("Bush","Rubio")
m2 = m[m$candidate %in% can,]

if(T) {
  g2 = g1[g1$poll_date > as.Date("2015-01-01"),]
  g2 = g2[g2$candidate %in% can,]
  f3 = aggregate(g2$votes, by=list(g2$candidate), FUN=mean, na.rm=T)
  f=unique(f3[with(f3, order(-x)), 1])
  m2$candidate = factor(m2$candidate, levels = f)
} 

m2 = m2[m2$poll_date > as.Date(fromDate),]  # <>
m2 = m2[m2$poll_date < as.Date("2016-06-14"),]  # try changing this

f2 = aggregate(m2$votes, by=list(m2$candidate,m2$type), FUN=mean, na.rm=T)

names(f2)[names(f2) == 'Group.1'] = 'candidate'
names(f2)[names(f2) == 'Group.2'] = 'type'
names(f2)[names(f2) == 'x'] = 'votes'

#f2$ord = with(f2,order(type,-votes))


f2$ranks<-with(f2, ave(votes, type, FUN=function(x) rank(-x)))

f=unique(f2[with(f2, order(type,-votes)), 1])
#f=unique(f2[with(f2[f2$type=="approval",], order(-votes)), 1])
f2$candidate = factor(f2$candidate, levels = f)

filename = fn1
svg(filename=paste0(filename,".svg"),
    width=7,
    height=5,
    pointsize=12)
p = ggplot(f2, aes(x=ranks,y=votes,fill=candidate))+
  geom_bar(stat="identity") + 
  facet_wrap(~ type) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(title = "Vote-Splitting Skews the Vote Count") + 
  ylab(ylabel)

plot_style_1(p) + 
  theme_igray(base_size=15) + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

dev.off()
rsvg_png(paste0(filename,".svg"),paste0(filename,".png"))



# table
if(flag.time) {
  library(gridExtra)
  filename = "figure/favor5-republicans-table"
  svg(paste0(filename,".svg"), height=3.5, width=3)
  f2$type <- factor(f2$type)
  f4=xtabs(round(votes,1) ~ candidate + type,f2,sparse=T)
  f4[f4 == 0] <- NA
  grid.table(f4)
  dev.off()
  rsvg_png(paste0(filename,".svg"),paste0(filename,".png"))
}

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
f=unique(a2[with(a2, order(-votes)), "candidate"])
a2$candidate = factor(a2$candidate, levels = f)

filename = "figure/favor5-republicans-future"
svg(filename=paste0(filename,".svg"),
    width=7,
    height=5,
    pointsize=12)

p = ggplot(a2, aes(x=poll_date,y=votes, color=candidate)) + 
  ylim(0,75) +
  labs(title = "America Likes Sanders and Kasich Better\nThan Clinton and Trump") + 
  ylab("Upvotes / (Upvotes+Downvotes)\n(realclearpolitics.com)") + 
  xlab("Date") +
  guides(colour = guide_legend(override.aes = list(size=4))) +
  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE) +
  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

plot_style_1(p)

#qplot(as.Date(a2$poll_date),votes, data=a2,ylim=c(0,75),geom = c('point'),alpha=I(.01),colour=candidate,xlab="Date",ylab="Upvotes / (Upvotes+Downvotes) %\n(realclearpolitics.com)",main="America Likes Sanders and Kasich Better Than Clinton and Trump\nBut Democrats Voted Clinton And Republicans Voted Trump") +  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE)+guides(colour = guide_legend(override.aes = list(size=4))) + theme_igray()  +  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y") 


dev.off()
rsvg_png(paste0(filename,".svg"),paste0(filename,".png"))

# + theme(plot.background = element_rect(fill = "beige")) + family="Times"
# + theme(text=element_text(family="Times"))