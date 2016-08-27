
# todo next: add the other calculated variables, like "net" and "future"
# 

require(ggplot2)
require(ggthemes)
require(Cairo)
require(rsvg)

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

stylefile="favor10 - style.R"
if (file.exists(stylefile)) {
  source(stylefile)
} else {
  library(extrafont)
  if (0) {
    font_import(pattern="[T/t]imes")
    font_import(pattern="[G/g]eorgia")
    fonts()
  }
  loadfonts(device="win")
  
  plot_style_1 = function (p) {
    if (1) {
      cols = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999','#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5','#ffed6f')
      if (1) {
        cols = cols[c(-6,-11)]
      } else if (0) {
        cols = cols[c(7:10)]
      }
      c1 = "beige"
      c2 = "#EFEFCA"
      c1 = "#F0EBDE"
      c2 = "#E9DDB4"
      p +
        theme_igray(base_size=13) +
        guides(colour = guide_legend(override.aes = list(size=4))) + 
        theme(panel.margin=unit(2,"lines")) +
        theme(plot.background = element_rect(fill = c1)) + 
        theme(legend.background = element_rect(fill = c1)) + 
        theme(strip.background = element_rect(fill = c2)) + 
        theme(panel.grid.major = element_line(colour = c1)) +
        scale_colour_manual(values = cols) +
        theme(text = element_text(family="Georgia"))
    } else {
      p +
        theme_igray(base_size=13) +
        guides(colour = guide_legend(override.aes = list(size=4))) + 
        theme(panel.margin=unit(2,"lines"))
    }
  }
}
#scale_color_brewer(palette="Set3") +

can7 = c("Kasich", "Carson", "Rubio", "Cruz", "Trump","Clinton","Sanders")
can7 = c("Sanders","Clinton","Carson","Trump","Rubio","Cruz","Kasich")
can5 = c("Sanders","Kasich","Clinton","Trump","Cruz")







filename = "figure/favor5-fig1-net-5"
svg(filename=paste0(filename,".svg"),
    width=8,
    height=5,
    pointsize=12)

#can5 = c("Kasich","Trump","Cruz","Sanders","Clinton")
#can5 = c("Sanders","Kasich","Clinton","Trump","Cruz")
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
       
dev.off()
rsvg_png(paste0(filename,".svg"),paste0(filename,".png"))






# do plot of favorability

filename = "figure/favor5-fig1-likes-5"
svg(filename=paste0(filename,".svg"),
    width=5,
    height=4,
    pointsize=12)

# filter and sort
#can5 = c("Kasich","Trump","Cruz","Sanders","Clinton")
#can5 = c("Sanders","Kasich","Clinton","Trump","Cruz")
a2 = a[a$candidate %in% can5,]
a2$candidate = factor(a2$candidate, levels = can5)

p = ggplot(a2, aes(x=poll_date,y=votes, color=candidate)) +
        labs(title = "America Likes Sanders and Kasich Better\nThan Clinton and Trump") + 
        ylab("Favorability Rating %\n(realclearpolitics.com)") + 
        xlab("Date") +
        geom_smooth(method='loess',lwd=I(2),span=.55,se=FALSE) +
        scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

plot_style_1(p) + 
  scale_y_continuous(breaks=seq(0, 100, 10),limits=c(0,65))

dev.off()
rsvg_png(paste0(filename,".svg"),paste0(filename,".png"))



filename = "figure/favor5-fig1-future-5"
svg(filename=paste0(filename,".svg"),
    width=5,
    height=4,
    pointsize=12)


# filter and sort
#can5 = c("Trump","Clinton","Sanders","Kasich","Cruz")
#can5 = c("Kasich","Trump","Cruz","Sanders","Clinton")
s2 = s[s$candidate %in% can5,]
s2$candidate = factor(s2$candidate, levels = can5)

p = ggplot(s2, aes(x=poll_date,y=votes, color=candidate)) + 
  labs(title = "America Likes Sanders and Kasich Better\nThan Clinton and Trump") + 
  ylab("Favorability Rating %\n(realclearpolitics.com)") + 
  xlab("Date") +
  geom_smooth(method='loess',lwd=I(2),span=.55,se=FALSE) +
  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

plot_style_1(p) + 
  scale_y_continuous(breaks=seq(0, 100, 10),limits=c(0,65))

dev.off()
rsvg_png(paste0(filename,".svg"),paste0(filename,".png"))




filename = "figure/favor5-fig1-likes-7"
svg(filename=paste0(filename,".svg"),
    width=7,
    height=5,
    pointsize=12)

# filter and sort
#can7 = c("Sanders","Kasich","Clinton","Carson","Rubio","Trump","Cruz")
#can7 = c("Sanders","Kasich","Clinton","Trump","Cruz","Carson","Rubio")
a2 = a[a$candidate %in% can7,]
a2$candidate = factor(a2$candidate, levels = can7)

p = ggplot(a2, aes(x=poll_date,y=votes, color=candidate)) + 
  labs(title = "America Likes Sanders and Kasich Better\nThan Clinton and Trump") + 
  ylab("Favorability Rating %\n(realclearpolitics.com)") + 
  xlab("Date") +
  geom_point(alpha=I(.5),lwd=I(2)) +
  geom_smooth(method='loess',lwd=I(2),span=.55,se=FALSE)+ 
  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

plot_style_1(p) + 
  scale_y_continuous(breaks=seq(0, 100, 10),limits=c(0,65))

dev.off()
rsvg_png(paste0(filename,".svg"),paste0(filename,".png"))







#can5 = c("Kasich","Trump","Cruz","Sanders","Clinton")
d2 = d[d$candidate %in% can5,]
d2$candidate = factor(d2$candidate, levels = can5)

filename = "figure/favor5-fr-a"
svg(filename=paste0(filename,".svg"),
    width=7,
    height=4,
    pointsize=12)

p = ggplot(d2, aes(x=poll_date,y=votes, color=candidate)) + 
  labs(title = "America Likes Sanders and Kasich Better Than Clinton and Trump\nBut Democrats Voted Clinton And Republicans Voted Trump") + 
  ylab("Votes %\n(realclearpolitics.com)") + 
       xlab("Date") +
       geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE) +
       facet_wrap(~ type) + 
       scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

plot_style_1(p) + 
  scale_y_continuous(breaks=seq(0, 100, 10),limits=c(0,65))
dev.off()
rsvg_png(paste0(filename,".svg"),paste0(filename,".png"))





#


m2 = m[m$candidate %in% can7,]
m2$candidate = factor(m2$candidate, levels = can7)
m2 = m2[m2$type %in% c("approval","first rank"),]

filename = "figure/favor5-fr-a-huff"
svg(filename=paste0(filename,".svg"),
    width=7,
    height=4,
    pointsize=12)
p = ggplot(m2, aes(x=poll_date,y=votes, color=candidate)) + 
  labs(title = "America Likes Sanders and Kasich Better Than Clinton and Trump\nBut Democrats Voted Clinton And Republicans Voted Trump") + 
  ylab("Votes %\n(HuffPo Pollster)") + 
  xlab("Date") +
  geom_smooth(method='loess',lwd=I(2),span=.9,se=FALSE) +
  facet_wrap(~ type) + 
  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

plot_style_1(p) + 
  scale_y_continuous(breaks=seq(0, 100, 10),limits=c(0,65))
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
  labs(title = "Republican Candidates Average Approval was at 27%") + 
  ylab("Votes %\n(realclearpolitics.com)") + 
  xlab("Date") +
  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE) +
  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

plot_style_1(p) + 
  scale_y_continuous(breaks=seq(0, 100, 10),limits=c(0,65))

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
  labs(title = "Republican Candidates Median Approval was at 29%\nBut Their Median Vote was at 5%") + 
  ylab("Votes %\n(realclearpolitics.com)") + 
  xlab("Date") +
  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE) +
  facet_wrap(~ type) + 
  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

plot_style_1(p) + 
  scale_y_continuous(breaks=seq(0, 100, 10),limits=c(0,65))

dev.off()
rsvg_png(paste0(filename,".svg"),paste0(filename,".png"))






# all candidates from huffpo

can = c("Kasich", "Carson", "Rubio", "Cruz", "Trump","Walker","Paul","Christie","Bush","Huckabee","Santorum","Jindal","Perry","Fiorina","Gilmore","Graham","Pataki")
m2 = m[m$candidate %in% can,]
m2 = m2[m2$type %in% c("approval","first rank"),]

g2 = g1[g1$poll_date > as.Date("2015-01-01"),]
g2 = g2[g2$type %in% c("approval","first rank"),]

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
  labs(title = "Republican Candidates Median Approval was at 28%\nBut Their Median Vote was at 5%") + 
  ylab("Votes %\n(HuffingtonPost/Pollster)") + 
  xlab("Date") +
  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE) +
  facet_wrap(~ type) + 
  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

plot_style_1(p) + 
  scale_y_continuous(breaks=seq(0, 100, 10),limits=c(0,65))

dev.off()
rsvg_png(paste0(filename,".svg"),paste0(filename,".png"))




# all (7) candidates from huffpo

# can7 = c("Kasich", "Carson", "Rubio", "Cruz", "Trump","Clinton","Sanders")
m2 = m[m$candidate %in% can7,]
m2 = m2[m2$type %in% c("approval","first rank"),]

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
  labs(title = "temp title") + 
  ylab("Votes %\n(HuffPo Pollster)") + 
  xlab("Date") +
  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE) +
  facet_wrap(~ type) + 
  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

plot_style_1(p) + 
  scale_y_continuous(breaks=seq(0, 100, 10),limits=c(0,65))


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
m2 = m[m$candidate %in% can,]

if(T) {
  g2 = g1[g1$poll_date > as.Date("2015-01-01"),]
  g2 = g2[g2$candidate %in% can,]
  g2 = g2[g2$type %in% c("approval","first rank"),]
  f3 = aggregate(g2$votes, by=list(g2$candidate), FUN=mean, na.rm=T)
  f=unique(f3[with(f3, order(-x)), 1])
  m2$candidate = factor(m2$candidate, levels = f)
} 

m2 = m2[m2$poll_date > as.Date(fromDate),]  # <>
m2 = m2[m2$poll_date < as.Date("2016-06-14"),]  # try changing this
m2 = m2[m2$type %in% c("approval","first rank"),]

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
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
#theme(base_size=15) + 

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
  labs(title = "America Likes Sanders and Kasich Better\nThan Clinton and Trump") + 
  ylab("Upvotes / (Upvotes+Downvotes)\n(realclearpolitics.com)") + 
  xlab("Date") +
  guides(colour = guide_legend(override.aes = list(size=4))) +
  geom_smooth(method='loess',lwd=I(2),span=.7,se=FALSE) +
  scale_x_date(limits = as.Date(c('2015-01-01','2016-06-15')),date_labels = "%b %y")

plot_style_1(p) + 
  scale_y_continuous(breaks=seq(0, 100, 10),limits=c(0,65))
dev.off()
rsvg_png(paste0(filename,".svg"),paste0(filename,".png"))







stack_up = c("approval","stack proportional low","stack proportional high","disapproval")
d_stack = with(n, n[type %in% stack_up,])
d_stack$type = factor(x=d_stack$type,levels=stack_up)

d_stack = with(d_stack, d_stack[! candidate %in% c("Warren","Cuomo","Ryan"),]) 

fromDate = "2016-02-01"
ylabel = "Votes %\nAverage Over Primary Season, Feb-Jun"
fn1 = "figure/favor5-projected approval"

# make the bars
d_stack = d_stack[d_stack$poll_date > as.Date(fromDate),] 
d_bar = with(d_stack, aggregate(votes, by=list(candidate,type), FUN=mean, na.rm=T))
# rename columns (cleanup)
names(d_bar)[names(d_bar) == 'Group.1'] = 'candidate'
names(d_bar)[names(d_bar) == 'Group.2'] = 'type'
names(d_bar)[names(d_bar) == 'x'] = 'votes'

# sort
if (0) {
  d_bar2=d_bar[d_bar$type=="approval",]
  f=unique(d_bar2[with(d_bar2, order(-votes)), 1])
} else if (0) {
  d_bar2=d_bar[d_bar$type=="disapproval",]
  f=unique(d_bar2[with(d_bar2, order(votes)), 1])
} else {
  d_bar1 = d_bar[d_bar$type=="approval" | d_bar$type=="stack proportional low",]
  d_bar2 = with(d_bar1, aggregate(votes, by=list(candidate), FUN=sum, na.rm=T))
  f=unique(d_bar2[with(d_bar2, order(-x)), 1])
}
#f=unique(d_bar[with(d_bar[d_bar$type=="approval",], order(-votes)), 1])
d_bar$candidate = factor(d_bar$candidate, levels = f)

# cols = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999','#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5','#ffed6f')
# cols = cols[c(7:10)]
# +
#   scale_colour_manual(values = cols)

cnames= c("Approval","Low","High",NA)
levels(d_bar$type) = cnames

filename = fn1
svg(filename=paste0(filename,".svg"),
    width=7,
    height=5,
    pointsize=12)
p = ggplot(d_bar, aes(x=candidate,y=votes,fill=type))+
  geom_bar(stat="identity",position="stack") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(title = "Projected Ratio of Approval/Disapproval") + 
  ylab(ylabel) + 
  scale_fill_brewer(direction=-1,
                    guide = guide_legend(reverse=T,
                                         title = ""))


plot_style_1(p) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

dev.off()
rsvg_png(paste0(filename,".svg"),paste0(filename,".png"))

#facet_wrap(~ source) + 






