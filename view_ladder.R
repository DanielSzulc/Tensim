VisualiseLadder<- function(tournament, year) {
     tmp <- ReadTournament(tournament = tournament, year= year)
     html<-paste("<html><head> <title>",tournament, year,,"</title></head>", 
                 sep=" ")
     styl<-"<style type=\"text/css\">
        div {
	float: left;
        position: relative;
	}
	</style>"
     h2<-paste("<h2>",tournament, year,"</h2>", sep=" ")
     body.inside <- " "
     html.end <-"</html>"
     rounds<-split(tmp, tmp$Rnd)
     
     for(i in length(rounds):1) {
             body.inside<-paste(body.inside,"<div style=\"top=50\"")
             sapply(rounds[[i]], function(x) )
     }
     coordinates<-tmp %>% group_by(Rnd) %>% mutate(coord=TMMN/max(TMMN)) %>% print
     par(mfrow=c(1,1)); with(coordinates,plot(Rnd,coord, pch=19, 
                                              xlim=rev(range(Rnd)), 
                                              ylim=range(TMMN/4)))     
             
}