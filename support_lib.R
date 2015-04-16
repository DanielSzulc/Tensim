# support library
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)


select_results <-function(players = 1:100, 
                          tournament = unique(db_matches$Tournament), 
                          years = year(ymd(max(db_ranking$Date))), 
                          from = ymd(min(db_matches$Date)),
                          to = ymd(max(db_matches$Date)
                                                )){
        #input: players for whom you want data
        #       tournament names for which you want data c("Wimbledon","US Open)
        #       as default all torunamens
        #       years (current year as default) in form c(1996,1997, 2001:2002)
        # !! Achtung: You must conducting sorting and name matching in calling
        # function
        #output: raw list of players IDs (rows) with data in columns
        
        winners <- db_matches %>% select(-c(Pts_L)) %>% rename(ID_play = Win, 
                                                               Opponent = Los, 
                                                               Pts = Pts_W) %>%
                mutate(Result="Win")
        loser <- db_matches %>% select(-c(Pts_W)) %>% 
                rename(ID_play = Los, Opponent = Win, Pts = Pts_L) %>%
                mutate(Result = "Los") %>% group_by(rownames(db_matches)) %>%
                mutate(Score = reverse_score(Score)) %>% ungroup
                
        matches <- bind_rows(winners, loser)
        
        selected.matches <- matches %>% 
                filter(ID_play %in% players, 
                       Tournament %in% tournament,
                       year(ymd(Date)) %in% years,                       
                       ymd(Date)>=ymd(from) & ymd(Date)<=ymd(to))
        outcome <- selected.matches %>% group_by(rownames(selected.matches)) %>%
                mutate(Parsed=parse_score(Score)) %>%
                separate(col = Parsed, into = c("Sets.F","Sets.A","Games.F",
                                                "Games.A")) %>% 
                mutate(W = ifelse(Result=="Win",1,0),
                       L = ifelse(Result=="Los",1,0)) %>%
                mutate(Sets.F = as.numeric(Sets.F), Sets.A = as.numeric(Sets.A), 
                       Games.F = as.numeric(Games.F), Games.A = as.numeric(Games.A)) %>%
                ungroup %>% select(-TMN,-contains("rownames"))
        outcome
                
}
reverse_score <- function(score){
        result <-paste(sapply(strsplit(score," ")[[1]], 
                              function(v) paste(rev(strsplit(v,NULL)[[1]]),
                                                collapse="")), collapse=" ")
        result

}
parse_score <-function(score){
        extracted.sets <- strsplit(score, split = " ",)[[1]]
        
        extracted.sets<-extracted.sets[extracted.sets!="0-0"]
        
        length(extracted.sets)
        sets.f <-0
        sets.a <-0
        games.f <-0
        games.a <-0
        
        for (i in seq_along(extracted.sets)) {
                result <- parse_set(extracted.sets[i])
                sets.f <- sets.f + result[1]
                sets.a <- sets.a + result[2]
                games.f <-games.f + result[3]
                games.a <-games.a + result[4]
        }
        outcome <- paste(sets.f,sets.a,games.f,games.a,sep=";")
        outcome
}
parse_set <- function(set_result){
        parsed.set <- strsplit(set_result,"-")[[1]]
        games.f <- as.numeric(parsed.set[1]) 
        games.a <- as.numeric(parsed.set[2])
        set.f<-0
        set.a<-0
        
        if (parsed.set[1]==7) {
                set.f<-1
        } else if (parsed.set[2]==7){
                set.a<-1
        } else if (parsed.set[1]== 6) {
                set.f<-1
        } else {
                set.a<-1
        }
        result<-c(set.f,set.a,games.f,games.a)
        
        result
        
}

DropPoints <- function(date) {
        x_t <- select_results(years = year(max(db_ranking$Date))-1)
        next.week <- filter(x_t, Drop_date<=ymd(date)+days(7) & Drop_date>ymd(date)) %>%
                group_by(ID_play, Tournament) %>% summarise(points= sum(Pts))
        two.weeks <- filter(x_t, Drop_date<=ymd(date)+days(14) & Drop_date>ymd(date)+days(7)) %>%
        group_by(ID_play, Tournament) %>% summarise(points= sum(Pts))
        
        next.week <- plyr::join(next.week, players,by = "ID_play") %>% 
                select(Surname, Tournament, points) %>% arrange(desc(points))
        two.weeks <- plyr::join(two.weeks, players,by = "ID_play") %>%
                select(Surname, Tournament, points) %>% arrange(desc(points))
        print(next.week)
        print(two.weeks)
}
BreakRankingPoints <-function(player) {
        selected.matches <-(select_results(players = player, 
                                           years = c(year(max(db_ranking$Date)), 
                                                     year(max(db_ranking$Date))-1)))
        rank.breakdown <- selected.matches %>% 
                filter(Drop_date > max(db_ranking$Date)) %>% 
                group_by(Tournament) %>% 
                summarize(Points = sum(Pts), Round = min(Rnd), 
                          WL = min(Result),
                          Drop = max(Drop_date)) %>% 
                arrange(desc(Points))
        print(rank.breakdown)
        total.pts.info <- paste("Total points:", 
                                sum(rank.breakdown$Points),sep=" ")
        
        print(total.pts.info)
        rm("selected.matches")
        rm("rank.breakdown")


}
CompareRanking <-function(player1,player2) {
        pl1.surname<-players[which(players$ID_play==player1),"Surname"]
        pl2.surname<-players[which(players$ID_play==player2),"Surname"]
        pl1.rank <- filter(db_ranking, Id_pl==player1)                
        pl2.rank <- filter(db_ranking, Id_pl==player2)
        toplot.rank <-bind_rows(pl1.rank, pl2.rank)
        head(toplot.rank)
        #ggplot(toplot.rank, aes(Date,Pos),color=Surname) + geom_line()
        #qplot(Date,Pos, data=toplot.rank,colour=Surname,)
        pos.range <-range(toplot.rank$Pos)
        date.range<-as.Date(range(toplot.rank$Date))
        with(subset(toplot.rank, Id_pl==player1),
             plot(as.Date(Date), Pos, type="l", col="red", 
                               ylim=rev(pos.range), xlim=date.range))
        
        with(subset(toplot.rank, Id_pl==player2),
             lines(as.Date(Date), Pos, type="l", col="blue"))
        legend("bottomleft",legend = c(pl1.surname, pl2.surname), 
               lty=1, col=c("red","blue"), cex=0.8)
                
        
        
}