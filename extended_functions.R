library(dplyr)
library(knitr)
library(tidyr)

qualify <-function(app_players,wildcards,total_draw=8,seeded=2) {
        selecting <- db_ranking %>% filter(Date==max(Date)) %>%
                filter(Id_pl %in% setdiff(app_players,wildcards)) %>%
                arrange(Pos)
        print(selecting)
        number.wildcards <- length(wildcards)
        who.seeded <- selecting[1:seeded,"Id_pl"]
        who.drawed <- c(selecting[(seeded+1):(total_draw - number.wildcards), 
                                  "Id_pl"], wildcards)
        who.drawed <-unique(who.drawed)
        print(who.drawed)
        print(c("Seeded: ", who.seeded," Draw: ",tmp<-sample(who.drawed)), 
              quote = FALSE)
        list(seeded=who.seeded, drawed=tmp)
}

number.win <-function(){
        number.win <- winners() %>% group_by(Win) %>% summarize(Numb = n()) %>% 
                arrange(desc(Numb)) %>% print
}
print.current.rank <-function(){
        library(knitr)
        library(dplyr)
        current.ranking<-db_ranking %>% filter(Date==max(Date))
        last.ranking<- db_ranking %>% filter(Date<max(Date)) %>%
                filter(Date==max(Date)) %>% select(Id_pl,Pos) %>% 
                rename(Last.Pos=Pos)
        working<-plyr::join(current.ranking,last.ranking,by = "Id_pl",)
       
        highest.ranking <- db_ranking %>% group_by(Id_pl) %>% 
                summarise(High = min(Pos), Low= max(Pos))
        working<-plyr::join(working,highest.ranking,by = "Id_pl",)
       
        working <- mutate(working, CHANGE=as.numeric(Last.Pos)-as.numeric(Pos))
        print(working)
        l1<-"```{r echo=FALSE}"
        l2<-"current.ranking<-db_ranking %>% filter(Date==max(Date))"
        l3<-"```"
        l2_1 <-"last.ranking<- db_ranking %>% filter(Date<max(Date)) %>%
                filter(Date==max(Date)) %>% select(Id_pl,Pos) %>% 
                rename(Last.Pos=Pos)
        working<-plyr::join(current.ranking,last.ranking,by = \"Id_pl\",)
       
        highest.ranking <- db_ranking %>% group_by(Id_pl) %>% 
                summarise(High = min(Pos), Low= max(Pos))
        working<-plyr::join(working,highest.ranking,by = \"Id_pl\",)
       
        working <- mutate(working, CHANGE=as.numeric(Last.Pos)-as.numeric(Pos))
        select(working,-Date,-Id_pl)%>% print"
        l2_2 <-"## Movers of the week  "
        l2_3 <- "working %>% select(Pos, Surname, CHANGE) %>%
        arrange(desc(CHANGE)) %>% filter(CHANGE !=0) %>% print"
        l1_2 <-"# Ranking as of ```r max(db_ranking$Date)```...................... ![logo](graphics/ATP.gif)"
        l4a<-"## DEFENDING RANKING POINTS in next two weeks:"
        l4b<-"DropPoints(max(db_ranking$Date))"        
        writeLines(c(l1_2,l1,l2,l2_1,l3,l2_2,l1,l2_3,l3,l4a,l1,l4b,l3),"rank_print.Rmd")
        knit2html("rank_print.Rmd")
        browseURL("rank_print.html")
        show_road_to_finals()
        ShowSeasonStats()
}
inactive <-function(player1, player2, turnier){
        category <- filter(db_tournaments,Name_of_Tournament==turnier) %>%
                select(Category)
        category <- category[[1]]
        name1 <- filter(players,ID_play==player1) %>% select(Surname)
        print(name1[[1]])
        matches<-merge(db_matches,db_tournaments,by.x = "Tournament", by.y = "Name_of_Tournament")
        matches1 <- filter(matches, Category==category) %>% 
        filter(Win==player1 | Los==player1)
        
        surnames<-select(players,Surname,ID_play)
       
        matches1 <- merge(matches1,surnames,by.x="Win",by.y="ID_play")
        matches1<-rename(matches1, Winner = Surname)
        matches1 <- merge(matches1,surnames,by.x="Los",by.y="ID_play")
        matches1<-rename(matches1, Loser = Surname)
        select(matches1,Tournament,Date, Rnd, Winner, Loser, Score) %>% 
        arrange(Date) %>% print
        # seccond player
        category <- category[[1]]
        name2 <- filter(players,ID_play==player2) %>% select(Surname)
        print(name2[[1]])
        matches<-merge(db_matches,db_tournaments,by.x = "Tournament", by.y = "Name_of_Tournament")
        matches2 <- filter(matches, Category==category) %>% 
                filter(Win==player2 | Los==player2)
        
        surnames<-select(players,Surname,ID_play)
        
        matches2 <- merge(matches2,surnames,by.x="Win",by.y="ID_play")
        matches2<-rename(matches2, Winner = Surname)
        matches2 <- merge(matches2,surnames,by.x="Los",by.y="ID_play")
        matches2<-rename(matches2, Loser = Surname)
        select(matches2,Tournament,Date, Rnd, Winner, Loser, Score) %>% 
                arrange(Date) %>% print


}
match_preview <- function (player1, player2,turnier){
       
        options<-"```{r set-options, echo=FALSE, cache=FALSE}
        options(width=130)
        ```"
        open<-"```{r echo=FALSE, fig.width=11}"
        close<-"```"
        n1<- filter(players,ID_play==player1) %>% select(Surname)
        
        n2<-filter(players,ID_play==player2) %>% select(Surname)
        name1<-paste("##",n1[[1]])
        name2<-paste("##",n2[[1]])
        play1 <-"category <- filter(db_tournaments,Name_of_Tournament==turnier) %>%
                select(Category)
        category <- category[[1]]
        
        
        matches<-merge(db_matches,db_tournaments,by.x = \"Tournament\", by.y = \"Name_of_Tournament\")
        matches1 <- filter(matches, Category==category) %>% 
        filter(Win==player1 | Los==player1)
        
        surnames<-select(players,Surname,ID_play)
       
        matches1 <- merge(matches1,surnames,by.x=\"Win\",by.y=\"ID_play\")
        matches1<-rename(matches1, Winner = Surname)
        matches1 <- merge(matches1,surnames,by.x=\"Los\",by.y=\"ID_play\")
        matches1<-rename(matches1, Loser = Surname)
        select(matches1,Tournament,Date, Rnd, Winner, Loser, Score) %>% 
        arrange(Date) %>% print"
        
        play2 <-"matches<-merge(db_matches,db_tournaments,by.x = \"Tournament\", by.y = \"Name_of_Tournament\")
        matches2 <- filter(matches, Category==category) %>% 
                filter(Win==player2 | Los==player2)
        
        surnames<-select(players,Surname,ID_play)
        
        matches2 <- merge(matches2,surnames,by.x=\"Win\",by.y=\"ID_play\")
        matches2<-rename(matches2, Winner = Surname)
        matches2 <- merge(matches2,surnames,by.x=\"Los\",by.y=\"ID_play\")
        matches2<-rename(matches2, Loser = Surname)
        select(matches2,Tournament,Date, Rnd, Winner, Loser, Score) %>% 
                arrange(Date) %>% print"
        play1_2 <-"PrintThisSeason(player1)
        PrintPastResults(player1, turnier)"
        play2_2 <-"PrintThisSeason(player2)
        PrintPastResults(player2, turnier)"
        plot.1.ranking <- "CompareRanking(player1, player2)"
        play1.breakdown <-"BreakRankingPoints(player1)"
        play2.breakdown <-"BreakRankingPoints(player2)"
        play1.name <- "players[which(players$ID_play==player1),\"Surname\"]"
        play2.name <- "players[which(players$ID_play==player2),\"Surname\"]"
        #writeLines(c(options, name1,open,play1,close,name2,open,play2,close,open, 
        #             play1_2,play2_2,close),"preview.Rmd")
        writeLines(c(options,open,plot.1.ranking,close,name1,open,play1.breakdown, 
                     play1_2,close,name2,open,play2.breakdown, 
                     play2_2,close),"preview.Rmd")
        knit2html("preview.Rmd",quiet = TRUE)
        browseURL("preview.html")
}


count_tournaments <- function(){
        library(tidyr)
        z_tours<-db_matches %>% select(Tournament, Win, Los, Surface) %>% 
                gather(Result,ID_play,Win:Los) %>% group_by(ID_play,Surface) %>%
                summarise(Tournaments = n_distinct(Tournament)) %>%
                spread(Surface,Tournaments) %>% group_by(ID_play) %>%
                summarise(Total=sum(Clay,Grass,Hard, na.rm=TRUE),Clay,Grass,Hard) %>%
                arrange(desc(Total))
        z_tours <- plyr::join(z_tours,select(players,ID_play, Surname, Country))
        print(select(z_tours, Surname, Country,Total, Clay,Grass, Hard))
}

recommend_tournament <-function(category=5, surface="Hard"){
        z_rank <<- db_ranking %>% filter(Date==max(Date)) %>%
                select(Id_pl,Pos,Pts) %>% rename(ID_play = Id_pl)
        z_players <- players %>% select(-c(Name,DOB,Skills))
        z_stats <- plyr::join(z_players,z_rank, type="left")
        
        
        z_stats <- z_stats %>% mutate(recover=aux_how_long_ago(Last_match), 
                                      est.fit=(1+recover/200)*Fitness) %>%
                group_by(ID_play) %>%
                mutate(est.fit=round(min(est.fit,1),2), 
                       pressure = aux_situation(pts = Pts), 
                       sur_factor = aux_surface(tour_surface = surface, 
                                                Surface_B, Surface_W), 
                       recommendation = est.fit * pressure * sur_factor * 
                               aux_prestige(category), 
                       recommendation = round(recommendation,2)) %>%
                select(recommendation, Surname,ID_play,Country, Pos, Pts, Last_match, 
                       recover, Fitness, est.fit, pressure, sur_factor)
        dropped <- DropPoints(as.Date(max(db_ranking$Date))) %>%
                group_by(ID_play) %>% summarise(points=sum(points, na.rm=TRUE)) %>%
                mutate(drop.f=round(1+(points/1300),3)) %>% select(ID_play, drop.f)
        
        z_stats <- plyr::join(z_stats, dropped, by="ID_play")
        
        z_stats <- z_stats %>% rename(reco=recommendation, Con=Country) %>% 
                mutate(drop.f=ifelse(is.na(drop.f),yes = 1,no = drop.f)) %>%
                mutate(reco=round(reco*drop.f,2))%>%
                arrange(desc(reco))
         z_stats       
        #print(z_stats, n=100)   # if > 100 players in database - remember to change it
}
aux_how_long_ago <-function(date){
        result <- as.numeric(as.Date(max(db_ranking$Date))- (as.Date(date)))
        round(result,2)
}
aux_situation <- function(pts){
        if(is.na(pts)) pts<-0
        pts.key <-sapply(c(1,2,8,9,15,16,30,31), function(x) 
                if((length(subset(z_rank$Pts,z_rank$Pos==x))>0)) {subset(z_rank$Pts,z_rank$Pos==x)[1]}
                   else {0})
        key.difference <- abs(pts-pts.key)
        key.difference <- key.difference[!is.na(key.difference & key.difference >0)]
        result<-min(key.difference)
        if(length(result)==0) {
                result<-1
        } else {
                result <-result
        }
        result <-(min(1,1/log(result+1,base = 100)))
        round(result,2)
}
aux_surface <- function(tour_surface,best_surface,worst_surface){
        kBEST <-1
        kWORST<-0.8
        kNEUTRAL<-0.9
        if(tour_surface==best_surface) {
                result<-kBEST
        } else if (tour_surface==worst_surface) {
                result <-kWORST
        } else {
                result<-kNEUTRAL
        }
        result
}
aux_prestige <-function(category=5){
        result<-1/log(category+1,base=3)/category^(0.3)+0.4/category
        result<-(min(result,1.4))
        result
}


matches_by_surface <- function(){
        z_matches_W<-db_matches %>% select(Tournament, Win, Los, Surface) %>% 
                gather(Result,ID_play,Win:Los) %>% group_by(ID_play,Surface,Result) %>%
                summarise(Count = n()) %>% filter(Result=="Win") %>%
                spread(Surface,Count) %>% group_by(ID_play) %>%
                mutate(Total_W = sum(Clay,Grass,Hard, na.rm = TRUE)) %>%
                rename(Clay_W = Clay, Grass_W = Grass, Hard_W= Hard)
                               
        z_matches_L<-db_matches %>% select(Tournament, Win, Los, Surface) %>% 
                gather(Result,ID_play,Win:Los) %>% group_by(ID_play,Surface,Result) %>%
                summarise(Count = n()) %>% filter(Result=="Los") %>%
                spread(Surface,Count) %>% group_by(ID_play) %>%
                mutate(Total_L = sum(Clay,Grass,Hard, na.rm = TRUE)) %>%
                rename(Clay_L = Clay, Grass_L = Grass, Hard_L= Hard) %>% 
                select(-Result)
        z_matches <- plyr::join(z_matches_W, z_matches_L, by = "ID_play", type = "full")         
                        
        z_matches <- plyr::join(z_matches,select(players,ID_play, Surname, Country), 
                                type= "right") %>% 
                filter(!is.na(Surname))
        print (select(z_matches,Surname,Country,Total_W, Total_L,Clay_W, 
                      Clay_L, Grass_W, Grass_L, Hard_W, Hard_L))
}

PrintThisSeason <-function(player) {
        tmp<-select_results(players = player) %>% arrange(Date) %>% 
                select(Date, Tournament, Rnd, Result, Score, Opponent, ID_play)
        tmp<-plyr::join(tmp,select(players, ID_play, Surname)%>% 
                               rename(Opponent=ID_play)) %>% 
                select(Tournament, Rnd, Result, Surname, Score, Date)
        print(tmp)
}
PrintPastResults <- function(player, tournament) {
        year<-year(ymd(curr_date))
        y <-integer(0)
        for (i in 1995:year){
               y<-c(y,i) 
        }
        
        tmp<-select_results(players = player, tournament = tournament, years = y) %>%
                select(Date, Tournament, Rnd, Result, Score, Opponent, ID_play) %>%
                arrange(Date)
        tmp<-plyr::join(tmp,select(players, ID_play, Surname)%>% 
                                rename(Opponent=ID_play)) %>% 
                select(Date, Rnd, Result, Surname, Score)
        print(tmp)
        tmp.best.res <- mutate(tmp,Year=year(Date)) %>% group_by(Year) %>% 
                summarise(Rnd=min(Rnd), Result=min(Result)) %>%
                filter(Rnd==min(Rnd))
        print(c("Appearances: ",length(unique(year(tmp$Date)))),quote=FALSE)
        tmp.record <- mutate(tmp,Year=year(Date)) %>% group_by(Result) %>%
                summarise(No=n()) %>% arrange(desc(Result))
        print(as.data.frame(tmp.record))
        print("Best result:", quote=FALSE)
        print(as.data.frame(tmp.best.res))
        
        
}
GetRecommendedPlayers <-function(tour=tournament) {
        cat <- subset(db_tournaments$Category, 
                      db_tournaments$Name_of_Tournament==tour)
        select.play <- recommend_tournament(cat, surface)
        should.play <-filter(select.play,reco>=0.5,recover>0)
        if(nrow(should.play)<10) {
                should.play <-filter(select.play,recover>0, est.fit>0.6) %>%
                        arrange(desc(reco)) %>%slice(1:12)
        }
        unlist(should.play[,3], use.names = FALSE)
}
MakeTournamentPreview <- function(tournament) {
        year<-year(ymd(curr_date))
        y <-integer(0)
        for (i in 1995:year){
                y<-c(y,i) 
        }
        
        tmp<-select_results(tournament = tournament, years = y) %>%
                select(Date, Tournament, Rnd, Result, Score, ID_play)
        
        tmp<-plyr::join(tmp,select(players, ID_play, Surname))%>%                                  
                select(Date, Rnd, Result, Surname, Score, ID_play)
        winners <- tmp %>% filter(Rnd==1, Result=="Win") %>% 
                mutate(Year=year(Date)) %>%                
                select(Year, Surname) %>% arrange(desc(Year))
        print("Past winners", quote=FALSE)
        print(winners)
        records <- tmp %>% group_by(ID_play, Surname, Result) %>% 
                summarise(Number=n()) %>% spread(Result, Number) %>% 
                group_by(ID_play) %>% 
                mutate(perc=round(Win/sum(Win,Los,na.rm = TRUE),3)) %>%
                ungroup %>% select(Surname,Win,Los,perc) %>% arrange(desc(perc))
        print(records)
}

ShowTournamentPreview <- function() {
        options<-"```{r set-options, echo=FALSE, cache=FALSE}
        options(width=130)
        ```"
        open<-"```{r echo=FALSE, fig.width=11}"
        close<-"```"
        make.preview<-"MakeTournamentPreview(tournament)"
        title <-paste("## ",tournament," ", sep=" ")
        writeLines(c(title,options,open,make.preview,close),"tour.Rmd")
        knit2html("tour.Rmd",quiet = TRUE)
        browseURL("tour.html")
}
ShowNumberOne <-function(this.year=FALSE) {
        tmp <- filter(db_ranking, Pos==1)
        tmp <- tmp %>%mutate(end.day=lead(Date,1)) %>% 
                mutate(days=as.numeric(ymd(end.day)-ymd(Date)))
        if(this.year==TRUE) {
                tmp<-filter(tmp,year(Date)==year(max(db_ranking$Date)))
        }
        total.weeks<-tmp %>% group_by(Id_pl,Surname) %>% 
                summarise(n.days=sum(days,na.rm=TRUE), 
                          weeks= floor(n.days/7)) %>% ungroup %>%
                arrange(desc(weeks)) %>%select(Surname,weeks)
        
        top.10<-filter(db_ranking, Pos<=10, Pos>0)
        rank.dates <-select(db_ranking, Date) %>% unique %>%
                mutate(end.day=lead(Date,1))        
        top.10<-plyr::join(top.10, rank.dates) %>% 
                mutate(days=as.numeric(ymd(end.day)-ymd(Date)))
        if(this.year==TRUE) {
                top.10<-filter(top.10,year(Date)==year(max(db_ranking$Date)))
        }
        top.10<-top.10 %>% group_by(Id_pl,Surname) %>% 
                summarise(n.days=sum(days,na.rm=TRUE), 
                          weeks= floor(n.days/7)) %>% ungroup %>%
                arrange(desc(weeks)) %>%select(Surname,weeks) %>% print(n=30)
        
        print(total.weeks)
}
GraphLeaderCharts <- function() {
        z<-filter(db_ranking, Pos==1)
        plot(ymd(z$Date), z$Pts, col=z$Id_pl, type="b", pch=19,xlab="",
             ylab="", main="All time")
        legend("bottomright",pch=19,col=unique(z$Id_pl), legend=unique(z$Surname), 
               cex=0.8,bty="n" )
        z<-filter(db_ranking, Pos==1, year(Date)==year(max(db_ranking$Date)))
        plot(ymd(z$Date), z$Pts, col=z$Id_pl, type="b", pch=19, xlab="",
             ylab="", main=year(max(db_ranking$Date)))
        legend("bottomright",pch=19,col=unique(z$Id_pl), legend=unique(z$Surname), cex=0.8,bty="n" )

}
CreateMatrixAchievment <- function(category, year){
        tours <- filter(db_tournaments,Category==category) %>%
                select(Name_of_Tournament)
        tours <- tours[[1]]
        tmp <- select_results(tournament = tours, years = year)
        
        tmp <- select(tmp, ID_play,Tournament,Rnd,Result) %>%  
                
                group_by(Tournament, ID_play) %>% 
                
                summarise (Achieve=min(Rnd), R=min(Result)) %>%
                mutate(Achieve=ifelse(R=="Win",0,Achieve)) %>% select(-R) %>% 
                ungroup %>%
                spread(key = Tournament,value = Achieve)
        tmp.rank<-filter(db_ranking, Date==max(Date)) %>% select(Id_pl,Pos) %>%
                rename(ID_play=Id_pl)
        tmp.surname <-select(players,ID_play,Surname)
        tmp<-plyr::join(tmp,tmp.rank, by="ID_play")
        tmp<-plyr::join(tmp,tmp.surname,by="ID_play")
        tmp<-select(tmp, Surname, Pos,2:(ncol(tmp)-2)) %>% arrange(Pos)

        invisible(tmp)
}
PrintMatrixAchievement <- function(category=2, year) {
        options<-"```{r set-options, echo=FALSE, cache=FALSE}
        options(width=130)
        ```"
        open<-"```{r echo=FALSE, fig.width=11, results=\"asis\"}"
        close<-"```"
        content <- paste("tmp<-CreateMatrixAchievment(",category,",",year,")",sep="")
        content2 <-paste("library(xtable)","tmp<-as.data.frame(tmp)", 
        "print(xtable(tmp, digits=0),type=\"html\",include.rownames=FALSE)", 
        sep="; ")
        print(content)
        writeLines(c(options,open,content,content2,close),"matrix_achieve.Rmd")
        knit2html("matrix_achieve.Rmd",quiet = TRUE)
        browseURL("matrix_achieve.html")
}