
route_trim<-function(r) as.numeric(strsplit(r,"-")[[1]][1])

ground<-function(x, d) round(x/d)*d

busUpdate<-function(oldgreen,key){

     gtfs<-tryCatch(
       GET('https://api.at.govt.nz/v3/public/realtime/tripupdates',
                  accept_json(),
                  add_headers('Ocp-Apim-Subscription-Key' = key)),
        error=function(e) NULL)

    if (is.null(gtfs)) return(NULL)
    if (status_code(gtfs)!=200) {
        print(status_code(gtfs))
        return(NULL)
    }
    sysnow<-Sys.time()
    now<-as.numeric(sysnow)
    
    buses<-lapply(content(gtfs)[[2]][[2]],function(x) x$trip_update$stop_time_update)
    arr<-do.call(rbind,lapply(buses, function(x) unlist(x$arrival)))
    dep<-do.call(rbind,lapply(buses, function(x) unlist(x$departure)))

    now <-as.numeric(Sys.time())
    arr<-arr[(arr[,"time"] > now- 10*60) & (arr[,"time"] < now+5*60), ]
    dep<-dep[(dep[,"time"] > now- 10*60) & (dep[,"time"] < now+5*60), ]

    update.times<-as.POSIXct(c(arr[,"time"],dep[,"time"]),origin="1970-01-01")
    
    arr.delay<-arr[,"delay"]
    dep.delay<-dep[,"delay"]
    bus.delay<-c(arr.delay,dep.delay)
    is.arrival<-rep(c(TRUE,FALSE),c(length(arr.delay),length(dep.delay)))
    

        ##beeswarm
        
        delaycat<-cut(bus.delay,c(-Inf,-180,-60,0,300,600,Inf))
        
        delaycols<-data.frame(
            delay=c("(-Inf,-180]", "(-180,-60]","(-60,0]", "(0,300]", "(300,600]", "(600, Inf]"),
            col=c("red","orange","#00EEB0","#00EEB0","orange","red"),
            pch=c(1,1,1,19,19,19), stringsAsFactors=FALSE
	)
        
        i<-match(delaycat,delaycols$delay)
        
        i[is.arrival & bus.delay<0]<-3  ## early arrival is ok
        
        newgreen<-mean(i %in% c(3,4))
        
        bus.delay<-pmin(50*60,pmax(-30*60,bus.delay))
        
        png("bus-summary.png",height=200,width=500)
        par(mar=c(4.1,1,1,1))
        
        beeswarm(bus.delay/60~is.arrival,pwcol=delaycols$col[i],pwpch=delaycols$pch[i],cex=0.5,horiz=TRUE,xlab="Minutes late",corral="gutter",method="swarm",xlim=c(-30,50))
        
        usr<-par("usr")
        text(usr[1]+1,2.3,"Arrivals",adj=0)
        text(usr[1]+1,0.6,"Departures",adj=0)
        dev.off()



    
    
    l0<-strftime(median(update.times),"%b %d %H:%M")
    l1<-paste0(round(newgreen*100),"% on time")
    
    if (is.na(oldgreen)) l1<-paste(l1,"\n")
    else if (newgreen>=oldgreen+.05) l1<-paste0(l1, ": getting better\n")
    else if (newgreen<=oldgreen-.05) l1<-paste0(l1, ": getting worse\n")
    else l1<-paste(l1,"\n")

    lateness<-list(l1,l0,newgreen,sum(!is.na(bus.delay)))

    buses<-lapply(content(gtfs)[[2]][[2]],function(x) c(route=route_trim(x$trip_update$trip$route_id),where=x$trip_update$stop_time_update$stop_sequence,x$trip_update$stop_time_update))
    arr<-do.call(rbind,lapply(buses, function(x) if("arrival" %in% names(x)) c(route=x$route,where=x$where,unlist(x$arrival))))
    dep<-do.call(rbind,lapply(buses, function(x) if("departure" %in% names(x)) c(route=x$route,where=x$where,unlist(x$departure))))

    arrbadtime<- (arr[,"time"]> now +3*60) | (arr[,"time"]< now-10*60)
    depbadtime<- (dep[,"time"]> now +3*60) | (dep[,"time"]< now-15*60)

    alltimes<-as.data.frame(rbind(arr[!arrbadtime,], dep[!depbadtime,]))
    alltimes<-alltimes[order(alltimes[,"route"],alltimes[,"where"]),]
    
    diffs <- (unlist(by(alltimes, alltimes$route, function(d) if(nrow(d)<=3) NULL else mean(abs(diff(d$delay))))))

    nbus<-unlist(by(alltimes, alltimes$route, function(d) nrow(d)*(nrow(d)>3)))

    headway<-list(sysnow, sum(nbus), length(diffs), ground(quantile(diffs/60, c(0.5,.75,.90)),.5))
    

    

    
    return( c(lateness, headway=list(headway)))
}

## startup
library(twitteR)
library(jsonlite) #need it
library(beeswarm)
library(maptools)
library(httr) #need it

load("auckland-thinned.rda")

## DEBUG
##tweet<-print


load("/home/tlum005/.ssh/twitter-bot-secrets.rda")
with(secrets,setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret))

oldgreen<-NA

## run
mihi<-""
#mihi<-"Kia ora."

repeat({
    cat("a",system.time({
        r<-busUpdate(oldgreen,key=secrets$newapikey)
    }))
    if (is.null(r)) {
        cat(Sys.time(),"No data: waiting 5 minutes\n")
	try(tweet("I see no buses. I'll just go have a nap until the internet clears up.\n Hei kōnā mai."))
	Sys.sleep(60*5)
	repeat({
            r<-busUpdate(oldgreen,key=secrets$newapikey)
            if(!is.null(r)) break;
            cat(Sys.time(),"No data: waiting 30 minutes\n")
            Sys.sleep(60*30)
            #mihi<-"Kia ora."
        })
    }
    oldgreen<-r[[3]]
    
    
    repeat({
        worked<-tryCatch(
            tweet(paste(mihi,"At", r[[2]],"I can see",r[[4]],"buses with",r[[1]],"\n\nTypical headway slippage is ",r$headway[[4]][1],
                        " minutes (gusting to ",r$headway[[4]][3],") based on ",r$headway[[3]]," routes.") , mediaPath="bus-summary.png",bypassCharLimit=TRUE),
            error=function(e) {print(conditionMessage(e)); NULL}
        )
        
        if (!is.null(worked)) break
        cat(Sys.time(),"tweet failed, waiting 15\n")
        Sys.sleep(15*60)
    })
    
    mihi<-""
    
    cat(Sys.time(),"Tweeted: waiting 15 minutes\n")
    Sys.sleep(15*60)
    
    if( as.POSIXlt(Sys.time())$hour>21) {
	night<-TRUE
	tweet("It's late. The buses will soon be asleep. Pō marie.")
	cat(Sys.time(),"Night: waiting 8 hours\n")
        
	Sys.sleep(60*8*60)
	mihi<-sample(c("Ata marie.", "Mōrena."),1)
    }
    
})	

#every bus delay time, location, and trip id
#also need bus's previous information. 看bus是否变化(和自己之前的比). 比如以前
#delay 5分钟，现在也是5分钟，没有变坏。

