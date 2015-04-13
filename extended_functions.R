library(dplyr)
library(knitr)
qualify <-function(app_players,wildcards,total_draw=8,seeded=2) {
        selecting <- db_ranking %>% filter(Date==max(Date)) %>%
                filter(Id_pl %in% app_players | Id_pl %in% wildcards) %>%
                arrange(Pos)
        print(selecting)
        number.wildcards <- length(wildcards)
        who.seeded <- selecting[1:seeded,"Id_pl"]
        who.drawed <- c(selecting[(seeded+1):(total_draw - number.wildcards), "Id_pl"], wildcards)
        print(who.drawed)
        print(c("Seeded: ", who.seeded," Draw: ",sample(who.drawed)), 
              quote = FALSE)
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
        open<-"```{r echo=FALSE}"
        close<-"```"
        name1<-"***```r name1 <- filter(players,ID_play==player1) %>% select(Surname); print(name1[[1]])``` ***"
        
        name2<-"***```r name2 <- filter(players,ID_play==player2) %>% select(Surname);
        print(name2[[1]])``` ***"
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
        writeLines(c(options, name1,open,play1,close,name2,open,play2,close),"preview.Rmd")
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
                       recover, Fitness, est.fit, pressure, sur_factor) %>% 
                ungroup() %>% rename(reco=recommendation, Con=Country) %>%
                arrange(desc(reco))
                
        print(z_stats, n=100)   # if > 100 players in database - remember to change it
}
aux_how_long_ago <-function(date){
        result <- as.numeric(as.Date(max(db_ranking$Date))- (as.Date(date)))
        round(result,2)
}
aux_situation <- function(pts){
        if(is.na(pts)) pts<-0
        pts.key <-sapply(c(1,2,8,9,15,16,30,31), function(x) subset(z_rank$Pts,z_rank$Pos==x))
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
