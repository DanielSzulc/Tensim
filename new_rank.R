## new rank
NewMakeRanking <-function(date) {
        
        kFactor<-0.2 #exponential average factor
        
        #years
        year<-year(ymd(curr_date))
        y <-integer(0)
        for (i in 1995:year){
                y<-c(y,i)
        }
        date <-as.character(date)
        tmp <- select_results(years = y) %>% filter(Drop_date>=date, ID_play!=0) %>% 
                group_by(ID_play) %>% summarise(Curr.Pts = sum(Pts, na.rm = TRUE))
        tmp
        r.tmp <- filter(db_ranking, Pos>0, Date==max(Date)) %>% 
                rename(ID_play = Id_pl)
        new.tab <- plyr::join(tmp,r.tmp, by="ID_play")
        new.tab <- new.tab %>% group_by(ID_play) %>% 
                mutate(new.pts = sum(kFactor*Curr.Pts, (1-kFactor)*Pts, na.rm = TRUE)) %>%
                mutate(Pts=round(new.pts,0)) %>% filter(Pts>0) %>%
                ungroup %>% mutate(Pos = min_rank(desc(new.pts)), Date = date)
        new.tab <-rename(new.tab, Id_pl=ID_play) %>% 
                select(Pos, Id_pl, Name, Surname, Country, Pts, Date) %>% 
                arrange(Pos) %>% print(n=75)
        
        db_r<-bind_rows(db_ranking,new.tab)
        
        write.csv(db_r,file="ranking.txt",row.names=FALSE)
        db_r
        
}