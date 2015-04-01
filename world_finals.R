##atp finals script
library(lubridate)
library(dplyr)
library(tidyr)
source("support_lib.R")

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

setup_finals <- function(){
        # qualified players
        qualification <- filter(db_ranking, Date==max(db_ranking$Date))
        qualification<-qualification[1:8,"Id_pl"]
        print(qualification)
        # first two are seeded - the others six are drawed
        draw <- sample(qualification[3:8])
        #each group has no 1 or no 2 and three other drawed players
        red.group <<- c(qualification[1],draw[1:3])
        blue.group <<- c(qualification[2],draw[4:6])
        print(red.group)
        print(blue.group)
        kORDER.OF.PLAY<- c(1,6,2,5,3,4)
        red.order<-combn(red.group,2)[,kORDER.OF.PLAY]
        blue.order<-combn(blue.group,2)[,kORDER.OF.PLAY]
        print(red.order)
        print(blue.order)
        match.order<<-cbind(red.order[,1:2],blue.order[,1:2],
                           red.order[,3:4],blue.order[,3:4],
                           red.order[,5:6],blue.order[,5:6])
        print(match.order)
                
        prepare_tournament("ATP World Finals")
        sets<<-3
        dump(list = c("red.group","blue.group","match.order"),"finals.dumped")
        db_matches<<-m_simulator(match.order[1,1],match.order[2,1], 
                                sets,tournament,rnd,surface,curr_date)
        #
}
play_finals <- function(){
        #if there was no match ATP Finals in given year - launch setup
        if (determine_tournament_stage("ATP World Finals")==0) {
                print("Setting up ATP World Finals...")
                setup_finals()
        } else {
                source("finals.dumped")
                prepare_tournament("ATP World Finals")
                tournament.stage <- determine_tournament_stage("ATP World Finals")
                start.date <- subset(db_tournaments$Start_Date, 
                                     db_tournaments$Name_of_Tournament == "ATP World Finals")
                start.date <- as.Date(start.date)                
                curr_date <<- start.date +(tournament.stage %/% 2)
                if (tournament.stage >= 12) { 
                        finals_semi()
                } else {
                        next.match <- tournament.stage + 1
                        next.play1 <- match.order[1,next.match]
                        next.play2 <- match.order[2,next.match]
                        
                #uruchom mecz
                        db_matches<<-m_simulator(next.play1, next.play2, 
                                        sets,tournament,rnd,surface,curr_date)
                #wyswietl wyniki
                        final_groups_results()
                }
                
                
        }
        #if there were matches , count theme and determina at which stage
        # we should resume ATP Finals
}


### Support functions
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
determine_tournament_stage <- function(tour){
        date <- max(db_ranking$Date)
        rank.date <-ymd(date)
        our.year <- year(rank.date)
        result <- filter(db_matches, year(ymd(Date))==our.year, 
                         Tournament==tour) %>%nrow
        result
}
final_groups_results <-function(){
        print("Tu beda wyswietlane wyniki")
        source("finals.dumped")
        print("Red Group")
        
        red.g <- select_results(players = red.group, tournament = "ATP World Finals")
        
        
        red.g <- red.g %>% group_by(ID_play) %>% 
                summarise(Wins = sum(W), Loss = sum(L), Sets.F=sum(Sets.F),
                          Sets.A=sum(Sets.A), Games.F=sum(Games.F), 
                          Games.A = sum(Games.A)) %>% 
                mutate(Sets = Sets.F-Sets.A, Games = Games.F - Games.A)
                
        red.g <-plyr::join(red.g,players[,c("ID_play","Surname")],
                           by="ID_play", type = "left")  %>%                      
                arrange(desc(Wins),desc(Sets),desc(Games)) %>%
                select(Surname, ID_play, Wins,Loss,Sets.F, Sets.A, Games.F, Games.A) %>% print
        
        print("Blue Group") 
        blue.g <- select_results(players = blue.group, tournament = "ATP World Finals")
        
        
        blue.g <- blue.g %>% group_by(ID_play) %>% 
                summarise(Wins = sum(W), Loss = sum(L), Sets.F=sum(Sets.F),
                          Sets.A=sum(Sets.A), Games.F=sum(Games.F), 
                          Games.A = sum(Games.A)) %>% 
                mutate(Sets = Sets.F-Sets.A, Games = Games.F - Games.A)
        
       blue.g <-plyr::join(blue.g,players[,c("ID_play","Surname")],
                           by="ID_play", type = "left")  %>%                      
                arrange(desc(Wins),desc(Sets),desc(Games)) %>%
                select(Surname, ID_play, Wins,Loss,Sets.F, Sets.A, Games.F, Games.A) %>% print
        dump(c("blue.g","red.g"), "finals_gr.dumped")
       
}
finals_semi <- function(){
        print("Semi-finals. Round robin completed.")
        source("finals_gr.dumped")
        stage <- determine_tournament_stage("ATP World Finals")
        rnd<<-2
        sets<<-3
        if (stage == 12) {
                p1 = blue.g[1,"ID_play"]
                p2 = red.g[2,"ID_play"]
        } else if (stage == 13) {
                p1 = blue.g[2,"ID_play"]
                p2 = red.g[1,"ID_play"]
        } else if (stage == 14){
                rnd<<-1
                sets<<-5
                finalists <- select_results(tournament = "ATP World Finals") %>%
                        filter(Rnd==2, Result=="Win")
                
                finalists <- finalists[,"ID_play"][[1]]
                p1 = finalists[1]
                p2 = finalists[2]
                
                        
        } else {
                print("Torunament finished"); break;
        }
        db_matches<<-m_simulator(p1,p2, 
                                 sets,tournament,rnd,surface,curr_date)
        
}