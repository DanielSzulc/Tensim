# seas_stat.R
MakeSeasonStats <- function(){
        year<-year(max(db_ranking$Date))
        tmp<-select_results() %>% select(ID_play, Rnd, Surface, Result, Tournament)
        
        winners<-filter(tmp, Rnd==1, Result=="Win") %>% group_by(ID_play) %>%
                summarise(Winners =n()) %>%ungroup %>% arrange(desc(Winners))
        winners <-plyr::join(winners,players,by = "ID_play") %>% 
                select(Surname, Winners)
        print(" ")
        print(paste("Tournament winners",year, sep=" "))
        print(winners)
        
        runners.up <- filter(tmp, Rnd==1, Result=="Los") %>% group_by(ID_play) %>%
                summarise(Finals =n()) %>%ungroup %>% arrange(desc(Finals))
        
        runners.up <-plyr::join(runners.up,players,by = "ID_play") %>% 
                select(Surname, Finals)
        print(" ")
        print(paste("Runners up",year, sep=" "))
        print(runners.up)
        
        starts <- tmp %>% group_by(ID_play) %>% 
                summarise(Starts=n_distinct(Tournament)) %>%ungroup %>% 
                arrange(desc(Starts))
        starts <-plyr::join(starts,players,by = "ID_play") %>% 
                select(Surname, Starts) %>% slice(1:15)
        print(" ")
        print(paste("The most appearances",year, sep=" "))
        print(starts)
        
        
        match.records <- tmp %>% group_by(ID_play, Result) %>% 
                summarise(No = n()) %>% spread(Result, No) %>% 
                group_by(ID_play) %>% 
                mutate(Matches=sum(Win,Los, na.rm = TRUE), 
                       perc=round(Win/Matches,3)) %>% ungroup %>% 
                arrange(desc(perc), desc(Matches))
        match.records <-plyr::join(match.records,players,by = "ID_play") %>% 
                select(Surname, Matches, Win, Los, perc) %>% slice(1:15)
        print(" ")
        print(paste("Best match records",year, sep=" "))
        print(match.records)
        
        surface.records <-tmp %>% group_by(ID_play, Result, Surface) %>% 
                summarise(No = n()) %>% spread(Result, No) %>% 
                group_by(ID_play, Surface) %>% 
                mutate(Matches=sum(Win,Los, na.rm = TRUE), 
                       perc=round(Win/Matches,3)) %>% ungroup %>% 
                arrange(desc(perc), desc(Matches))
        surface.records<-plyr::join(surface.records,players,by = "ID_play") %>%
                select(Surname, Matches, Win, Los, perc, Surface) %>% 
                group_by(Surface) %>% top_n(15,perc)
        by.surface <- split(surface.records, surface.records$Surface)  
        
        print(" ")
        print(paste("Match records by surface",year, sep=" "))
              
        print(by.surface)
        print("Players not classified: ", quote=FALSE)
        x<-filter(db_ranking,Date==max(Date))
        x<-setdiff(players$ID_play,x$Id_pl)
        print(x)
                                                    
}
ShowSeasonStats <-function(){
        options<-"```{r set-options, echo=FALSE, cache=FALSE}
        options(width=130)
        ```"
        open<-"```{r echo=FALSE, fig.width=11}"
        close<-"```"
        make.preview<-"MakeSeasonStats()"
        number.one<-"ShowNumberOne(TRUE); ShowNumberOne(FALSE); GraphLeaderCharts()"
        comment<-"## Ranking leaders and top 10"
        title <-paste("## ","Season stats as at",ymd(max(db_ranking$Date)), sep=" ")
        writeLines(c(title,options,open,make.preview,close,comment,open,
                     number.one,close),"ss.Rmd")
        knit2html("ss.Rmd",quiet = TRUE)
        browseURL("ss.html")
        
}