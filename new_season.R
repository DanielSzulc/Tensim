## new season calendar
library(lubridate)
library(dplyr)
MakeNewSeason <- function(){
        # calculate the year of last season1
        last.year <-year(max(ymd(db_tournaments$Start_Date)))
        # adding one to obtain the year of the next season
        next.year <- last.year+1
        base.date <-ymd(paste(next.year,01,01,sep="-"))
        # reading dates of last season tournaments and obtaining weeks
        calendar <- db_tournaments %>% mutate(week=week(Start_Date)) %>% 
                mutate(final_date = GetDateSunday(week, base = base.date)) %>%
                mutate(s.date = final_date - days(log2(First_Rnd))) %>%
                mutate(Start_Date = s.date) %>% arrange(Start_Date)              
        file.name <-paste("archive/",next.year,"preview",".txt",sep="")
        # export to a file tournaments.txt
        write.csv(calendar,file=file.name,row.names=FALSE)
        tournaments <- select(calendar, Name_of_Tournament:Surface)
        write.csv(tournaments,file="tournaments.txt",row.names=FALSE)
        
}

GetDateSunday <- function(week, base) {
        first.day <- wday(base)
        to.sunday <- 8 - first.day
        if (to.sunday < 3) {
                to.sunday <- to.sunday+7
        } else {
                to.sunday <- to.sunday
        }
        first.sunday <- ymd(base) + days(to.sunday)
        given.sunday <- first.sunday+days((week-1)*7)
        given.sunday
}