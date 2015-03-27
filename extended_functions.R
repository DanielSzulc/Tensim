library(dplyr)
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
        l1<-"```{r}"
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
        arrange(desc(CHANGE)) %>% print"
        l1_2 <-"## Ranking as of ```r max(db_ranking$Date)```"
        writeLines(c(l1,l1_2,l2,l2_1,l2_2,l2_3,l3),"rank_print.Rmd")
        knit2html("rank_print.Rmd", envir =  globalenv())
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