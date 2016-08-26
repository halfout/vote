a = read.csv("Poll2.csv")
b = read.csv("Poll4.csv")
e = read.csv("Poll8.csv")
g = read.csv("Poll3.csv")

file_loc = "./"


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


s1=a
s1$votes = s1$future
s1$type = "future"
s1 = s1[,c("poll_date","votes","candidate","type")]
s1$source = "Real Clear Politics"

anet=a
anet$votes = anet$net
anet$type = "net"
anet = anet[,c("poll_date","votes","candidate","type")]
anet$source = "Real Clear Politics"

n = rbind(n,s1,anet)

write.csv(n, file = paste(file_loc, "Poll5.csv",sep = ""),row.names=TRUE, na="")

