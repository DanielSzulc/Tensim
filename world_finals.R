##atp finals script
library(lubridate)
library(dplyr)
library(tidyr)

### Road to finals
show_road_to_finals <-function(){
        
        #liczymy obecna sytuacje rankingowa
        date <- max(db_ranking$Date)
        rank.date <-ymd(date)
        our.year <- year(rank.date)
        print(our.year) # test: usunac
        print(date); print(rank.date) # test: usunac
        
        working_W <- db_matches %>% mutate(Date=ymd(Date)) %>%
                filter(year(Date) == our.year) %>%
                select(Win,Pts_W) %>% rename(ID_play = Win, Pts = Pts_W)
        working_L <- db_matches %>% mutate(Date=ymd(Date)) %>%
                filter(year(Date) == our.year) %>%
                select(Los,Pts_L) %>% rename(ID_play = Los, Pts = Pts_L)
        working <-bind_rows(working_W, working_L) %>% group_by(ID_play) %>%
                summarise(Points = sum(Pts)) %>% arrange(desc(Points)) %>% print
        points.8 <- working[[8,"Points"]]
        points.9 <- working[[9,"Points"]]
        print(points.8)
        print(points.9)
                
                
        #liczymy ile jeszcze jest do zdobycia punktow
        working_T <- db_tournaments %>% mutate(Start_Date = ymd(Start_Date)) %>%
                filter(Start_Date > rank.date) %>% 
                select(-c(Draw,First_Rnd,Surface)) %>% 
                filter(Name_of_Tournament != "ATP World Finals") %>% 
                mutate(week = week(Start_Date)) %>% 
                group_by(week) %>% 
                summarise(high.cat = min(Category),pts = max_points(high.cat)) %>%
                print
        points.available <- sum(working_T$pts)
        print(points.available)
               
                                                            
                
        #liczymy kto jeszcze ma szanse byc w osemce
        working <- working %>% mutate(ahead.9 = Points - points.9, 
                                      behind.8 = points.8 - Points) %>%
                mutate(qualified = ifelse(ahead.9> points.available,"Q","F"), 
                       chance = ifelse(behind.8 < points.available, "CH","N")) %>%
                group_by(ID_play) %>%
                mutate(standing = asses_situation(qualified, chance)) %>%
                select(ID_play, Points, standing) %>%
                print(n=60)
        #tworzymy tabele
        result <- plyr::join(players, working, by="ID_play")
        road_to_finals<<- select(result, Surname, ID_play,Points, standing) %>% 
                arrange(desc(Points)) %>% print
        #printujemy tabele
        writeLines(c("## Road to finals",
                     "```{r echo=FALSE}",
                     "print(road_to_finals)",
                     "```")
                   ,"road_finals.Rmd")
        knit2html("road_finals.Rmd",quiet = TRUE)
        browseURL("road_finals.html")
}

###Finals handling

max_points <- function(categ){
        points <- data.frame(cat=1:5, pts=c(1007,487,325,227,182))
        result <- points[which(points$cat==categ), "pts"]
        result

}
asses_situation <- function(qualify, chance){
        if(qualify=="Q") result<-"Qualified"
        else if (chance=="CH") result <-"Competing" else result <-"No Chance"
        result
}