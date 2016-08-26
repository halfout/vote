
library(ggplot2)

np = list( list( "Sanders", "http://elections.huffingtonpost.com/pollster/bernie-sanders-favorable-rating.csv"),
           list( "Kasich", "http://elections.huffingtonpost.com/pollster/john-kasich-favorable-rating.csv"),
           list( "Johnson", "http://elections.huffingtonpost.com/pollster/gary-johnson-favorable-rating.csv"),
           list( "Carson", "http://elections.huffingtonpost.com/pollster/ben-carson-favorable-rating.csv"),
           list( "Rubio", "http://elections.huffingtonpost.com/pollster/marco-rubio-favorable-rating.csv"),
           list( "Cruz", "http://elections.huffingtonpost.com/pollster/ted-cruz-favorable-rating.csv"),
           list( "Trump", "http://elections.huffingtonpost.com/pollster/donald-trump-favorable-rating.csv"),
           list( "Clinton", "http://elections.huffingtonpost.com/pollster/hillary-clinton-favorable-rating.csv"),
           list( "Fiorina", "http://elections.huffingtonpost.com/pollster/carly-fiorina-favorable-rating.csv"),
           list( "Bush", "http://elections.huffingtonpost.com/pollster/jeb-bush-favorable-rating.csv"),
           list( "Christie", "http://elections.huffingtonpost.com/pollster/chris-christie-favorable-rating.csv"),
           list( "Huckabee", "http://elections.huffingtonpost.com/pollster/mike-huckabee-favorable-rating.csv"),
           list( "Jindal", "http://elections.huffingtonpost.com/pollster/bobby-jindal-favorable-rating.csv"),
           list( "O Malley", "http://elections.huffingtonpost.com/pollster/martin-o-malley-favorable-rating.csv"),
           list( "Paul", "http://elections.huffingtonpost.com/pollster/rand-paul-favorable-rating.csv"),
           list( "Santorum", "http://elections.huffingtonpost.com/pollster/rick-santorum-favorable-rating.csv"),
           list( "Walker", "http://elections.huffingtonpost.com/pollster/scott-walker-favorable-rating.csv"),
           list( "Gilmore", "http://elections.huffingtonpost.com/pollster/jim-gilmore-favorable-rating.csv"),
           list( "Graham", "http://elections.huffingtonpost.com/pollster/lindsey-graham-favorable-rating.csv"),
           list( "Pataki", "http://elections.huffingtonpost.com/pollster/george-pataki-favorable-rating.csv"),
           list( "Perry", "http://elections.huffingtonpost.com/pollster/rick-perry-favorable-rating.csv"))

#list( "Stein", "http://elections.huffingtonpost.com/pollster/jill-stein-favorable-rating.csv"),

# webb is missing


sources=do.call(rbind,np)
write.csv(x = sources,file = "sources3.csv")


lp = length(np)

for(i in 1:lp){
  name = np[[i]][[1]]
  url = np[[i]][[2]]
  p = read.csv(url)
  p$Other = NA
  p$candidate = name
  
  #p = cbind(p,candidate)
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

file_loc = "./"
write.csv(d, file = paste(file_loc, "Poll3.csv",sep = ""),row.names=TRUE, na="") 
#,StringsAsFactors=F

