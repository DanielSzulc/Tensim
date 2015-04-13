setwd("C:/Users/Krokodylek/Documents/Rprogramming/tennis_sim")
#curr_date<-as.Date("1996-01-18")
source("extended_functions.R")
source("world_finals.R")
source("new_season.R")
players<-read.csv("players.txt",stringsAsFactors=FALSE)
db_matches<-read.csv("matches.txt",stringsAsFactors=FALSE)
db_ranking<-read.csv("ranking.txt",stringsAsFactors=FALSE)
db_tournaments<-read.csv("tournaments.txt",stringsAsFactors=FALSE)
tournament<-"N"
curr_date<-as.Date("1996-01-01")
rnd<-48
surface<-"N"
sets<-3
#moze sie przydac
#db_ranking[which(db_ranking[,"Date"]==max(db_ranking[,"Date"])&db_ranking[,"Pos"]<16),"Id_pl"]
#
#1. ustawiasz zmienne globalne tournament, curr_date i surface i rnd i sets (best of 3, best of 5)
#2 wcztujesz bazy db_matches, db_tournaments, i players
#players<-read.csv("players.txt",stringsAsFactors=FALSE)
#db_matches<-read.csv("matches.txt",stringsAsFactors=FALSE)
#db_tournaments<-read.csv("tournaments.txt")
#db_ranking<-mk_ranking(players,curr_date)

# db_matches<-m_simulator(1,2,sets,tournament,rnd,surface,curr_date)

#3  do zmiennej wynik zapisujesz wynik meczu
#4. zmienna wynik podajesz funkcji save mecz
#5. zwrocona linie laczysz z baza db_matches i zapisujesz do pliku matches.txt
m_simulator<-function(p1,p2,sets,tournament,rnd,surface,date)
{
        wynik<-match(p1,p2,max_sets=sets,FALSE,surf=surface,date)
        linia<-save_match(wynik,tournament,date,rnd,surface)
        temp_db<-rbind(db_matches,linia)
        write.csv(temp_db,file="matches.txt",row.names=FALSE)
        temp_db
}

match<-function(p1,p2,max_sets=3,no_tie_break=FALSE,surf="Hard",date)
{
        #inicjalizacja meczu
        par(mfrow=c(3,1))
        h2h_graph(history_graph(p1),p2)
        history_graph(p2)
        stats_player(p1,p2)
        readline(prompt="Press [enter] to continue")
        print(best_achieve(p1))
        readline(prompt="Press [enter] to continue")
        print(best_achieve(p2))
        match_preview(p1,p2,tournament)
        Sys.sleep(3)
        
        readline(prompt="Press [enter] to continue")
        sety<-c(0,0)
        in_games<-matrix(rep(0,2*max_sets),2,max_sets)
        rownames(in_games)<-c(as.character(players[p1,"Surname"]),as.character(players[p2,"Surname"]))
        current_set<-0
        set_counter<-1
        if(max_sets==5)
                {set_to_win<-3}
                else
                {set_to_win<-2}
        #fitness na poczatku meczu
        fit_p1<-start_fit(p1,date)
        fit_p2<-start_fit(p2,date)
        print(in_games)
        
        #pierwszy serwujacy
        serving<-sample(c(p1,p2),1)
        
        while ((sety[1]<set_to_win)&&(sety[2]<set_to_win))
        {
               while ((sum(current_set==p1)!=6)&&(sum(current_set==p2)!=6))
                {
                       current_set<-c(current_set,game(p1,p2,serving,surf,fit_p1,fit_p2))
                       serving<-if(serving==p1) p2 else p1
                       fit_p1<-game_fit(p1,fit_p1)
                       fit_p2<-game_fit(p2,fit_p2)
                       print(sum(current_set==p1))
                       print(sum(current_set==p2))
                       
                       
                }
               if ((sum(current_set==p1)-sum(current_set==p2))>=2)
               {        
                       sety[1]=sety[1]+1
               }
               else if    
                       ((sum(current_set==p2)-sum(current_set==p1)>=2))
                       {sety[2]=sety[2]+1}
               else 
                       {
                               current_set<-c(current_set,game(p1,p2,serving,surf,fit_p1,fit_p2))
               
                               if (sum(current_set==p1)-sum(current_set==p2)>=2) {sety[1]<-sety[1]+1}
                               else if ((sum(current_set==p2)-sum(current_set==p1))>=2) {sety[2]<-sety[2]+1}
                               else 
                                {
                                        current_set<-c(current_set,game(p1,p2,serving=0,surf,fit_p1,fit_p2))
                                        if(sum(current_set==p1)==7) {sety[1]<-sety[1]+1} else {sety[2]<-sety[2]+1}
                                }
                       }
               #zakonczenie seta
               in_games[1,set_counter]<-sum(current_set==p1)
               in_games[2,set_counter]<-sum(current_set==p2)
               set_counter<-set_counter+1
               print(in_games)
               print(sety)
               current_set<-numeric(0)
               
        }
        winner<- if(sety[1]==set_to_win) p1 else p2
        winner
        in_games
        gracze<-c(p1,p2)
        result<-cbind(gracze,sety,in_games)
        
        #zaktualizowanie danych o zawodnikach
        print(fit_p1)
        print(fit_p2)
        players[p1,"Fitness"]<<-round(fit_p1,4)
        players[p2,"Fitness"]<<-round(fit_p2,4)
        players[p1,"Last_match"]<<-as.character(date)
        players[p2,"Last_match"]<<-as.character(date)
        #zapis do pliku
        
        write.csv(players,file="players.txt",row.names=FALSE)
        #zapis meczu do bazy odbywa sie juz w innej funkcji (wywolujacej)
        result
}
game<-function(p1,p2,serving,surf,fit_p1,fit_p2)
{
        #surface występuje tylko dla playera 1
        #jeśli player_1 serwuje to zwieksza mu o 10% jesli p2 serwuje to p1 ma zmniejszone o 10%
        p1_chance<-players[p1,"Skills"]*surface_mod(p1,surf)*fit_p1*is_serving(p1,serving)
        
        p2_chance<-players[p2,"Skills"]*surface_mod(p2,surf)*fit_p2*is_serving(p2,serving)
        #print(p1_chance)
        #print(p2_chance)
        total<-c(p1_chance,p2_chance)
        chance<-total/sum(total)
        ##print(chance)
        Sys.sleep(1)
        rand<-sample(seq(0,1,0.005),1)
       ##print(rand)
      #print("Serwuje") print(serving) print("Wygrywa gema:")
        if (rand<chance[1]) p1 else  p2
        
}

is_serving <-function(player,serving)
{
        r<-if (player==serving) 1.2
        else 1
        r
}

#brala pod uwage nawierzchnie
surface_mod <- function(player,surface)
{
        r<-1
        if(players[player,"Surface_B"]==surface) {r<-1.05}
        else if ((players[player,"Surface_W"]==surface)) {r<-0.88}
        
        r
}

#okreslala startowy fitness
start_fit<-function(player,date)
{
        days<-as.Date(date)-as.Date(players[player,"Last_match"])
        #print(days)
        #print(as.numeric(days))
        
        fit<-players[player,"Fitness"]*(1+(as.numeric(days)/200))
        if (fit>1) fit<-1
        fit
}

#modyfikowala fitness w trakcie gry
game_fit<-function(player,current_fit)
{
        current_fit*0.998
}
#funkcja inicalizujaca srodowisko symulacyjne
sim_init<-function()
{
        players<-read.csv("players.txt",stringsAsFactors=FALSE)
        db_matches<-read.csv("matches.txt",stringsAsFactors=FALSE)
        db_tournaments<-read.csv("tournaments.txt")
}
#zapisywanie meczu do bazy
save_match<-function(wynik,tournament,date,rnd,surf)
{
        n<-ncol(wynik)
        tcat<-db_tournaments[which(db_tournaments[,"Name_of_Tournament"]==tournament),"Category"]
        if(wynik[1,2]>wynik[2,2])
        {
                Win<-wynik[1,1]
                Los<-wynik[2,1]
                Score<-convert_wynik(wynik,1,n)
        } else
        {
                Win<-wynik[2,1]
                Los<-wynik[1,1]
                Score<-convert_wynik(wynik,2,n)
        }
        Pts_W<-round((520/tcat/rnd))
        Pts_L<-round(((6-tcat)*(8/rnd)*min(wynik[1,2],wynik[2,2])))
        Drop_date<-as.Date(date)+365
        li<-data.frame(Date=as.character(date),Tournament=tournament,TMN=0,Rnd=rnd,Win,Los,Score,Pts_W,Pts_L,Drop_date=as.character(Drop_date),Surface=surf)
        li
        
        
}
#konwersja wyniku w setach na 6-2 2-6 6-4 gdzie pierwszy jest zwyciezca
convert_wynik<-function(wynik,Win,n)
{
        paste(wynik[Win,3:n],wynik[(3-Win),3:n],collapse=" ",sep="-")
}
#rankingator
mk_ranking<-function(players,date)
{
        sub<-db_matches[which(db_matches[,"Drop_date"]>date),]
        lo<-by(sub$Pts_L,sub$Los,sum)
        wi<-by(sub$Pts_W,sub$Win,sum)
        wm<-matrix(c(names(wi),wi),ncol=2)
        lom<-matrix(c(names(lo),lo),ncol=2)
        
        temp_rank<-data.frame(Pos=0,Id_pl=0,Name="A",Surname="B",Country="c",Pts=0,Date="1971-01-01")
        tmp<-matrix(c(0,0),1,2)
        for (i in 1:nrow(players))
        {
                pw<-if (length(wm[which(wm[,1]==i),2])!=0)
                {
                        wm[[which(wm[,1]==i),2]]
                } else
                {
                        0
                }
               
                pl<-if(length(lom[which(lom[,1]==i),2])>0)
                {
                        lom[[which(lom[,1]==i),2]] 
                } else
                {
                        0
                }
                pts<-as.numeric(pw)+as.numeric(pl)
                wiersz<-c(i,pts)
               
                tmp<-rbind(tmp,wiersz)
        }
       
        
        tmp<-tmp[which(tmp[,2]>0),]
        print(tmp)
        #ustawianie w kolejnosci
        
        ordering<-order(tmp[,2],decreasing=TRUE)
        print(ordering)
        k<-1
        m<-2
        for (i in ordering)
        {
                
                Pos<-k
                Id_pl<-tmp[[i,1]]
                Name=players[Id_pl,"Name"]
                Surname<-players[Id_pl,"Surname"]
                Country<-players[Id_pl,"Country"]
                Pts<-tmp[[i,2]]
                
                if(k>1)
                {
                        #rint(c("m:",m))
                        #rint("poprzedni")
                        #rint(temp_rank[m-1,"Pts"])
                        #rint("Aktualny")
                        #rint(Pts)
                        if (temp_rank[m-1,"Pts"]==Pts) Pos<-temp_rank[m-1,"Pos"]
                }
                Date=as.character(date)
                
                k<-k+1
                m<-m+1
                
                #print(c(Pos,Id_pl,Name,Surname,Pts))
                linia<-data.frame(Pos=Pos,Id_pl=Id_pl,Name=Name,Surname=Surname,Country=Country,Pts=Pts,Date=Date)
                temp_rank<-rbind(temp_rank,linia)
                #print(temp_rank)
                
                
        }
        db_r<-rbind(db_ranking,temp_rank)
       
        write.csv(db_r,file="ranking.txt",row.names=FALSE)
        
        db_r
}
stats_player<-function(p1,p2)
{
        p1W<-nrow(db_matches[which(db_matches[,"Win"]==p1),])
        p1L<-nrow(db_matches[which(db_matches[,"Los"]==p1),])
        p1R<-get_ranking(p1)
        p2W<-nrow(db_matches[which(db_matches[,"Win"]==p2),])
        p2L<-nrow(db_matches[which(db_matches[,"Los"]==p2),])
        p2R<-get_ranking(p2)
        p1v2W<-nrow(db_matches[which(db_matches[,"Win"]==p1&db_matches[,"Los"]==p2),])
        p2v1W<-nrow(db_matches[which(db_matches[,"Win"]==p2&db_matches[,"Los"]==p1),])
        n1<-players[p1,"Surname"]
        n2<-players[p2,"Surname"]
        p1print<-paste(n1,"Rank:",p1R,"W-L:",p1W,"-",p1L,sep=" ")
        p2print<-paste(n2,"Rank:",p2R,"W-L:",p2W,"-",p2L,sep=" ")
        h2hprint<-paste(n1,p1v2W,"-",p2v1W,n2,sep=" ")
        print(p1print)
        print(p2print)
        print(h2hprint)
}
get_ranking<-function(player)
{
        r<- if (length(db_ranking[which(db_ranking[,"Id_pl"]==player&db_ranking$Date==max(db_ranking$Date)),"Pos"])>0)
        {
                db_ranking[which(db_ranking[,"Id_pl"]==player&db_ranking$Date==max(db_ranking$Date)),"Pos"]
        } else
        {
                "NA"
        }
        r
}
prepare_tournament<-function(name)
{
        if (length(as.character(db_tournaments[which(db_tournaments[,"Name_of_Tournament"]==name),"Surface"]))>0)
        {
                result<-"Correct OK"
                surface<<-as.character(db_tournaments[which(db_tournaments[,"Name_of_Tournament"]==name),"Surface"])
                curr_date<<-as.Date(db_tournaments[which(db_tournaments[,"Name_of_Tournament"]==name),"Start_Date"])
                rnd<<-as.numeric(db_tournaments[which(db_tournaments[,"Name_of_Tournament"]==name),"First_Rnd"])
                tournament<<-as.character(db_tournaments[which(db_tournaments[,"Name_of_Tournament"]==name),"Name_of_Tournament"])
        } else 
        {
                result<-"Blad"
        }
        
        result
}
nxt_rnd<-function()
{
        curr_date<<-(curr_date+1)
        rnd<<-(rnd/2)
        print(paste("Poczatek rundy:",rnd,as.character(curr_date),sep=" "))
}
winners<-function()
{
        z_col<-c("Tournament","Win","Los","Score","Date")
        z_f<-db_matches[which(db_matches[,"Rnd"]==1),z_col]
        for (i in 1: nrow(z_f)){
                     z_f[i,"Win"]<-players[which(players[,"ID_play"]==z_f[i,"Win"]),"Surname"]
                     z_f[i,"Los"]<-players[which(players[,"ID_play"]==z_f[i,"Los"]),"Surname"]
         }
         z_f
}
best_achieve<-function(player)
{
        z2col<-c("Los","Date","Rnd","Tournament","Pts_W","Surface","Pts_L")
        r2<-db_matches[which(db_matches[,"Los"]==player),z2col]
        if(nrow(r2)>0)
        {
                for (i in 1:nrow(r2))
                {
                        r2[i,"Pts_W"]<-db_tournaments[which(db_tournaments[,"Name_of_Tournament"]==r2[i,"Tournament"]),"Category"]
                        r2[i,"Pts_L"]<-(1/(r2[i,"Rnd"]*r2[i,"Pts_W"]))
                }
                orde<-order(r2$Pts_L,decreasing=TRUE)
                or<-r2[orde[1:4],c("Tournament","Rnd","Date")]
                pl_inf<-players[which(players[,"ID_play"]==player),c("Name","Surname","Country")]
                wins<-db_matches[which(db_matches[,"Rnd"]==1&db_matches[,"Win"]==player),c("Tournament","Date")]
                wynik<-list(player=pl_inf,winner=wins,other=or)
        } else {wynik<-"No matches played"}
        wynik
}
history_graph<-function(player)
{
        pl_z<-db_matches[which(db_matches[,"Win"]==player),c("Surface","Date","Los")]
        pl_p<-db_matches[which(db_matches[,"Los"]==player),c("Surface","Date","Win")]
        if (nrow(pl_z)>0) 
        {
                pl_z<-cbind(pl_z,1,0)
                colnames(pl_z)<-c("Surface","Date","Opponent","Value","Total")
        }
        if (nrow(pl_p)>0)
        {
                pl_p<-cbind(pl_p,-1,0)
                colnames(pl_p)<-c("Surface","Date","Opponent","Value","Total")
               
        }
        if ((nrow(pl_z)>0)&&nrow(pl_p)>0)
        {
                pl_t<-rbind(pl_z,pl_p)
        } else if (nrow(pl_z)>0)
        {
                pl_t<-pl_z      
        } else if (nrow(pl_p)>0)
        {
                pl_t<-pl_p 
        }else
        {
                return(0)
        }
        
        pl_t<-pl_t[order(pl_t$Date),]
        for (i in 1:nrow(pl_t))
        {
                if (i>1)
                {
                        pl_t[i,"Total"]=pl_t[i-1,"Total"]+pl_t[i,"Value"]     
                }
                else
                {
                        pl_t[i,"Total"]=pl_t[i,"Value"]
                }
        }
        
        
        plot(pl_t$Total~as.Date(pl_t$Date),type='b',xlab="Date",ylab="Performance",main=players[player,"Surname"])
        abline(h=0,lty=3)
        pl_t
}
h2h_graph<-function(tablica,opponent)
{
        pl_t<-tablica[which(tablica[,"Opponent"]==opponent),]
        if (nrow(pl_t)==0) return(0)
        pl_t<-pl_t[order(pl_t$Date),]
        for (i in 1:nrow(pl_t))
        {
                if (i>1)
                {
                        pl_t[i,"Total"]=pl_t[i-1,"Total"]+pl_t[i,"Value"]     
                }
                else
                {
                        pl_t[i,"Total"]=pl_t[i,"Value"]
                }
        }
        
        
        plot(pl_t$Total~as.Date(pl_t$Date),type='b',xlab="Date",ylab="Performance",main=c("vs. ",players[opponent,"Surname"]))
        abline(h=0,lty=3)
}