# data munging

library(reshape2)

file_loc = "./"

a = read.csv("Poll2.csv")
b = read.csv("Poll4.csv")
e = read.csv("Poll8.csv")
g = read.csv("Poll3.csv")

# uniform column names
names(g)[names(g) == 'Start.Date'] = 'poll_date'
names(e)[names(e) == 'Start.Date'] = 'poll_date'
names(b)[names(b) == 'Votes'] = 'votes'

# pick only the relevant columns
a = a[,c("poll_date","Favorable","Unfavorable","candidate")]
g = g[,c("poll_date","Favorable","Unfavorable","candidate")]
e = e[,c("poll_date","votes","candidate")]
b = b[,c("poll_date","votes","candidate")]

# combine sources
a$source="Real Clear Politics"
b$source="Real Clear Politics"
e$source="Huffington Post"
g$source="Huffington Post"
ap = rbind(a,g)
fr = rbind(b,e)

# add extra columns
ap_original_columns = names(ap)
ap$approval = ap$Favorable
ap$disapproval = ap$Unfavorable
ap$max.future.approval = 100-ap$Unfavorable
ap$net.approval = ap$Favorable - ap$Unfavorable
ap$future.approval.split = .5 * (ap$Favorable + 100 - ap$Unfavorable)
ap$future.approval.proportional = 100 * ap$Favorable / (ap$Favorable + ap$Unfavorable)
ap$stack.proportional.low = ap$future.approval.proportional - ap$approval
ap$stack.proportional.high = ap$max.future.approval - ap$future.approval.proportional

# match the columns of first-rank and approval sets
# add type
fr$type = "first rank"
newnames = setdiff(names(ap),ap_original_columns)
ap_m = melt(ap,measure.vars = newnames)
ap_m$type = ap_m$variable
ap_m$votes = ap_m$value
# remove periods
levels(ap_m$type) <- gsub("\\.", " ", levels(ap_m$type))
#levels(data1$c) <- sub("_", "-", levels(data1$c))
ap_bind = ap_m[,names(fr)]

# combine to one dataset
n = rbind(fr,ap_bind)

write.csv(n, file = paste(file_loc, "Poll5.csv",sep = ""),row.names=TRUE, na="")