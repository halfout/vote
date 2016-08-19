# To overlay scatterplots in R

# import the required libraries
library(ggplot2)
library(reshape2)

# assign data
a1=rnorm(10)
a2=rnorm(10)
a3=rnorm(10)

# create a dataframe from combined data
# and set count to however many points are in each dataset
df = data.frame(a1, a2, a3, count = c(1:10))

# melt the dataframe
df.m = melt(df, measure.vars = c("a1","a2","a3"))

# take a look at what melt() does to get an idea of what is going on
df.m

# plot out the melted dataframe using ggplot
ggplot(df.m, aes(count, value, colour = variable)) + geom_point() + ylim(-3,3)

# swapping the axis
ggplot(df.m, aes(value, count, colour = variable)) + geom_point() + xlim(-3,3)
