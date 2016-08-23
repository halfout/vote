a = read.csv("http://elections.huffingtonpost.com/pollster/2016-national-gop-primary.csv")
names(a)[names(a) == 'Rand.Paul'] <- 'Paul'

# move columns 8 to 26 to one column, candidate

library(reshape2)
library(ggplot2)

b = melt(a, measure.vars = 8:26, na.rm=T)


b[["Start.Date"]] <- as.Date(b[["Start.Date"]])
names(b)[names(b) == 'value'] <- 'votes'
names(b)[names(b) == 'variable'] <- 'candidate'
ggplot(b, aes(Start.Date, votes, color = candidate)) + geom_point() + geom_smooth() 


file_loc <- "./"
write.csv(b, file = paste(file_loc, "Poll8.csv",sep = ""),row.names=TRUE, na="")
