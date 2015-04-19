VisualiseLadder<- function(tournament, year) {
     tmp <- ReadTournament(tournament = tournament, year= year)
     TOP <-20 #Constant determining how much next round is lower than previous
        part.1<-"<!doctype html>
<html>
<head>
     <title>Main Draw</title>
     
     
     
     <style type=\"text/css\">
     body {
     background-color: #f0f0f2;
     margin: 0;
     padding: 0;
     font-family: Helvetica, Arial, sans-serif;
     
     }
     .results {
     background-color: white;
     border-radius: 2px;
     float:left;
     position: relative;
     width: 100%;
     padding: 20px;
     
     }"
     
     rounds<-split(tmp, tmp$Rnd)
     part.2 <-"#r1 {
        	position: relative;
		float: left;
		width: 180px;}"
     for(i in 2:length(rounds)) {
             work <- paste("#r",i," {
        	position: relative;
		float: left;
		width: 180px;",
                           "top:",TOP*i,"px;}",sep="")
             
             part.2<-paste(part.2," ", work," ", sep="")
     }
     part.3 <-"p {
        		
		padding-bottom:10px;
	}
    </style>  "
     part.4 <- paste("</head>

<body>
<h1>",tournament," ",year,"</h1>
<div class=\"results\">", sep="")

     part.5 <- ""
     by.rounds<-split(tmp,tmp$Rnd)
     for (i in length(by.rounds):1) {
             p1<-paste("<div id=\"r",length(by.rounds)+1-i,"\">",sep="")
             p2<-""
             for (j in seq_along(by.rounds[[i]][[2]])) {
                     player.1 <-by.rounds[[i]][[2]][j]
                     player.2<-by.rounds[[i]][[3]][j]
                     p2a<-paste("<p>",LadderCheckName(player.1),", ",
                                LadderGetRanking(player.1,tournament,year),"<br/>", 
                                LadderCheckName(player.2),", ",
                                LadderGetRanking(player.2,tournament,year),"</p>",sep="") 
                     p2<-paste0(p2,p2a)
             }
             #p2<-paste("<p>",by.rounds[[i]][2],"<br/>",by.rounds[[i]][3],"</p>",sep="")
             p3<-"</div>"
             part.5<-paste0(part.5,p1,p2,p3)
     }
     part.6<-paste("<div style=\"float:left;, postion:relative, top:",length(rounds)*TOP,
                   "px;\">","<p><strong>",
                   LadderCheckName(subset(ladder$Pass,ladder$TMMN==1)),"</strong></p></div>", 
                   sep="")
     writeLines(text = c(part.1,part.2,part.3,part.4, part.5, part.6),con = "TEST.html")
     browseURL(url = "TEST.html")
         
             
}
LadderCheckName <- function(id_pl){
        name.country <- players[which(players$ID_play==id_pl),c("Surname","Country")]
        if (nrow(name.country)==0) {
                name.country<-"BYE"
        } else {
                name.country<-name.country
        }
        name.country<-paste(name.country, collapse=", ")
        
        name.country
}
LadderGetRanking <-function(id_pl, tournament, year){
        date<-filter(db_matches, Tournament==tournament, year(Date)==year)
        tour.started <- if(nrow(date)==0) {
                max(db_ranking$Date) 
        } else {
                min(date$Date)
        }
        #tour.started
        #print(tour.started)
        rank <- filter(db_ranking,Date<=tour.started) %>% 
                filter(Date==max(Date)) %>% filter(Id_pl==id_pl)
        if (nrow(rank)>0) {
                rank<-rank$Pos
        } else {
                rank<-NA
        }
                
        rank
        
}
        