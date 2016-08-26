a = read.csv("http://elections.huffingtonpost.com/pollster/2016-national-gop-primary.csv")
names(a)[names(a) == 'Rand.Paul'] <- 'Paul'

e = read.csv("http://elections.huffingtonpost.com/pollster/2016-national-democratic-primary.csv")

# move columns 8 to 26 to one column, candidate

library(reshape2)
library(ggplot2)

b = melt(a, measure.vars = 8:26, na.rm=T)
f = melt(e, measure.vars = 8:16, na.rm=T)

b$party = "R"
f$party = "D"

g = rbind(f,b)


g[["Start.Date"]] <- as.Date(g[["Start.Date"]])
names(g)[names(g) == 'value'] <- 'votes'
names(g)[names(g) == 'variable'] <- 'candidate'
ggplot(g, aes(Start.Date, votes, color = candidate)) + geom_point() + geom_smooth() 


file_loc <- "./"
write.csv(g, file = paste(file_loc, "Poll8.csv",sep = ""),row.names=TRUE, na="")
