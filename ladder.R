## ladder
ContinueTournament <-function() {
        ##wczytaj parametry turnieju
        no.players <-subset(db_tournaments$Draw, 
                            db_tournaments$Name_of_Tournament==tournament)
        
        ##sprawdz na jakim etapie jest turniej
        stage <- nrow(filter(db_matches, Tournament==tournament, 
                             year(Date)==year(max(db_ranking$Date))))
        name<-paste("./tour_archive/",tournament,"_",year(max(db_ranking$Date)),
                    ".txt", sep="")
        
                
        if(stage==(no.players-1)) {
                print(paste("Tournament",tournament,"done", sep=" "),quote=FALSE)
        } else if((stage==0)&&(!file.exists(name)) ) {
                print("Setting up tournament", quote=FALSE)
                ladder <- SetupTournament(no.players = no.players)
                SaveTournament(ladder)
                
        }else {
                ladder<- ReadTournament(tournament=tournament, 
                                        year(max(db_ranking$Date)))
                i<-1
                print(ladder)
                while(as.numeric(ladder[i,"Pass"])!=0) {
                        i<-i+1
                        print("Dziala")
                }
                while(rnd!=ladder[i,"Rnd"]) {
                        nxt_rnd()
                }
                print(i)
                
                cu.tmmn <-ladder[i,1]
                if (IsBye(cu.tmmn)==TRUE) {
                        ladder[i,"Pass"]=ladder[i,"p1"]
                        ladder[which(ladder$TMMN==ladder[i,"Nxt"]),
                               PassPlayer(ladder[i,"Nxt"],ladder)]=ladder[i,"Pass"]
                        
                } else {
                        db_matches<<-m_simulator(ladder[i,"p1"],ladder[i,"p2"], 
                                                sets,tournament,rnd, 
                                                surface,curr_date)
                        winner <- tail(db_matches,1)[1,"Win"]
                        ladder[i,"Pass"]=winner
                        ladder[which(ladder$TMMN==ladder[i,"Nxt"]),
                               PassPlayer(ladder[i,"Nxt"],ladder)]=ladder[i,"Pass"]
                }
                
                
        }
                      
        ##jesli wszystkie mecze rozegrane to wyswietl info
        ## jesli sie nie zaczal to go zesetupuj
        ## jesli sie zaczal to wczytaj baze
        SaveTournament(ladder)
        readline(prompt="Press [enter] to continue")
}
SaveTournament <-function(ladder){
        if(!file.exists("./tour_archive")) {
                dir.create("./tour_archive")
        }
        name<-paste("./tour_archive/",tournament,"_",year(max(db_ranking$Date)),
                    ".txt", sep="")
        dump("ladder", file = name)
}
SetupTournament <-function(tournament, year, no.players){
        theory.players <- 2^ceiling(log2(no.players))
        seeded<-theory.players %/% 4
        print(theory.players)
        print(seeded)
        playing.players <- qualify(app_players = g.a.p, wildcards = g.wc, 
                                   seeded =  seeded,
                                   total_draw = no.players)
        ladder <-data.frame(TMMN = (theory.players-1):1, p1=0, p2=0, score="", 
                            Rnd=0, Nxt=0, Pass=0)
        ladder<-mutate(ladder,Rnd=2^floor(log2(TMMN)), Nxt=TMMN%/%2)
        #obsadzanie rozstawionych
        for (i in seq_along(playing.players$seeded)){
                place <- PlaceSeeded(theory.players, seeded,i)
                ladder[which(ladder$TMMN==place),"p1"]=playing.players$seeded[i]
        }
        m<-theory.players-2  # wskaznik - najpierw parzyste
        k<-ceiling(mean((theory.players-1):(theory.players/2)))
        j<-1
        s<-c(-1,1,-3,3,-5,5,-7,7,-9,9,-11,11)  #moze kiedts trzeba zwiekszyc
        # jak beda wieksze turnieje
        for (i in seq_along(playing.players$drawed)) {
                
                if(m>=(theory.players/2)) {
                        
                        ladder[which(ladder$TMMN==m),(2+i%%2)] = 
                                playing.players$drawed[i]
                        
                } else {
                        match<-k+s[j]
                        j=j+1
                        ladder[which(ladder$TMMN==match),"p2"] = 
                                playing.players$drawed[i]
                }
                if (i%%2==0) {m<-m-2}
               
        }
        
        ladder
}

ReadTournament <-function(tournament, year){
        name<-paste("./tour_archive/",tournament,"_",year(max(db_ranking$Date)),
                    ".txt", sep="")
        source(name)
        as.data.frame(ladder)
}
PlaceSeeded <- function(th.players, seeded, pos.seed) {
        mean.seed <- mean(1:seeded)
        mean.TMMN <- mean((th.players-1):(th.players-(th.players/2)))
        #print(mean.TMMN)
        #print(mean.seed)
        if(pos.seed==1) {
                pos.seed=seeded
        } else {
                pos.seed=pos.seed-1
        }
        
        diff <- 2*(pos.seed-mean.seed)
        #print(diff)
        position.TMMN <- ceiling(mean.TMMN+diff)
        position.TMMN
}
IsBye <- function(tmmn){
        result<-if(ladder[which(ladder$TMMN==tmmn),"p2"]==0) {
                TRUE
        } else {
                FALSE
        }
        result
}
PassPlayer <- function(tmmn, ladder) {
        if (ladder[which(ladder$TMMN==tmmn), "p1"]==0){
                result <-"p1"
        } else {
                result <-"p2"
        }
        result
}