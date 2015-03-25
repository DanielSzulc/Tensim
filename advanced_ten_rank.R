##new tennis ranking
v<-c(2000,1000,500,250)
r<-c(128,64,32,16,8,4,2,1)
d<-data.frame(round=rep(r,4),cat=rep(v,each=8)
d[,"adj"]<-((2000/d$cat)*(1+log2(d$round)/50))/2+0.5
d
##Sampras rank 1030
##Chang rank 985
##tournament cat 500 semifinal
##probability of Sampras's lose
pnorm(985,mean=1030,sd=200*2.54,lower.tail=T)
##probability of Chang's lose
pnorm(1030,mean=985,sd=200*2.54,lower.tail=T)
## jakie jest p, ze wynik Changa bedzie ponizej tego potrzebnego do pokonania Samprasa

##Sampras wygrywa, jego nowy mean ma byc taki zeby prawdopodobienstwo takiego zdarzenia
#wzroslo o 3 pproc
qnorm(0.46*0.97,1030,sd=200/2.54,lower.tail=FALSE)
##wynik 1040
## dla Changa 
qnorm(0.54*0.97,985,sd=200/2.54,lower.tail=FALSE)
#wynik 980
##to samo dla Agassi 1015 i Becker 985 (p porazki 47.6 do 52.4%)
pnorm(985,mean=1015,sd=200*2.54,lower.tail=T)
pnorm(1015,mean=985,sd=200*2.54,lower.tail=T)
##Becker wygrywa