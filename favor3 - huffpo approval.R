
library(ggplot2)

np = list( list( "Sanders", "http://elections.huffingtonpost.com/pollster/bernie-sanders-favorable-rating.csv"),
           list( "Kasich", "http://elections.huffingtonpost.com/pollster/john-kasich-favorable-rating.csv"),
           list( "Johnson", "http://elections.huffingtonpost.com/pollster/gary-johnson-favorable-rating.csv"),
           list( "Carson", "http://elections.huffingtonpost.com/pollster/ben-carson-favorable-rating.csv"),
           list( "Rubio", "http://elections.huffingtonpost.com/pollster/marco-rubio-favorable-rating.csv"),
           list( "Cruz", "http://elections.huffingtonpost.com/pollster/ted-cruz-favorable-rating.csv"),
           list( "Clinton", "http://elections.huffingtonpost.com/pollster/hillary-clinton-favorable-rating.csv"),
           list( "Trump", "http://elections.huffingtonpost.com/pollster/donald-trump-favorable-rating.csv"))

#list( "Stein", "http://elections.huffingtonpost.com/pollster/jill-stein-favorable-rating.csv"),

lp = length(np)

for(i in 1:lp){
  name = np[[i]][[1]]
  url = np[[i]][[2]]
  p = read.csv(url)
  candidate = replicate(nrow(p),name)
  p = cbind(p,candidate)
  if (i==1) {
    d = p }  else {
    d = rbind(d,p) }
}

d$net = (d$Favorable - d$Unfavorable)
#par(cex=2)
#theme_set(theme_gray(base_size = 20))

svg(filename="figure/favor3-fig1-huffpo.svg",
    width=7,
    height=5,
    pointsize=12)

qplot(as.Date(End.Date),Favorable, data=d, geom = c('point','smooth'),colour=candidate,lwd=I(2),xlab="Date",ylab="Favorable Rating %",main="Favorability - Nationwide \n(elections.huffingtonpost.com/pollster)\nPeople Like Sanders & Kasich - But Hate Clinton & Trump")+guides(colour = guide_legend(override.aes = list(size=4))) + theme_igray(base_size=15) + theme(plot.title = element_text(size=15, face="bold",family="Times New Roman")) + geom_abline(slope=0,intercept=1) +  geom_smooth(method='loess',lwd=I(2),span=.8,se=FALSE)  #+ xlim(as.Date("2015-08-08"),as.Date("2016-08-08"))

dev.off()

#,method='loess',span=.8,se=FALSE

write.csv(d, file = paste(file_loc, "Poll3_huff.csv",sep = ""),row.names=TRUE, na="") 
#,StringsAsFactors=F

