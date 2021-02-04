library(rmutil)
library(ggplot2)
library(dplyr)



######### funkcja symulacji ##########
symulacja<- function(dystrybucja,
                     okres,
                     zmiennosc,
                     skutecznosc,
                     kapital,
                     prowizja){
  
if(dystrybucja == 1) {
  rndm <- rnorm(okres,0, zmiennosc)
} else if(dystrybucja == 2) {
  rndm <- rlaplace(okres,0, zmiennosc)}
  
rndm <- as.data.frame(rndm+1)
rndm <- cbind(c(1:nrow(rndm)), 
              rndm, 
              c(rep(0,nrow(rndm))),
              c(rep(0,nrow(rndm))))
names(rndm)[4] <- "inwestycja"
names(rndm)[3] <- "kierunek"
names(rndm)[1] <- "index"
names(rndm)[2] <- "zmiana"
rndm$kierunek[rndm$zmiana > 1] <- "1"
rndm$kierunek[rndm$zmiana < 1] <- "-1"
usun <- rndm$index[rndm$kierunek == "-1"]
usun <- usun[-sample(1:length(usun), size = (length(usun)*skutecznosc))]
rndm <- rndm[-c(usun),]
table(rndm$kierunek)[2]/sum(table(rndm$kierunek)) 
# skutecznosc 75% dla 0.3255, 71% dla 0.406281, 69% dla 0.4343 dla hanlowego
rndm$inwestycja <- kapital
rndm$inwestycja <- rndm$inwestycja*
  cumprod(rndm$zmiana)*
  (cumprod(rep(1-(2*prowizja),nrow(rndm))))
return(rndm$inwestycja)
}

sim1<- symulacja(
          dystrybucja = 2, # 1 dla normalnej, 2 dla laplace
          okres = 200, # liczony w 5 dniach sesji (i.e. tydzien), 
          # podana wartoœæ w okresie jest zmniejszana œrednio o 36,5% nie wiem czemu
          zmiennosc = 0.02,
          skutecznosc = 0.3255, # 75% dla 0.3255, jest dziwnie u³ozone
          kapital = 100,
          prowizja = 0.003)# 0.003 to 3% prowizji, liczby w obie strony 
length(sim1)
rlaplace(10, m=0, 1)
(sim1[length(sim1)]/100)-1 # stopa zwrotu

plot(sim1,type = "l")

?plot
######## monte carlo #########

mc.okres <- 106*1 # ostateczny bedzie uciêty o 50 %
ilosc <- 10000
mc.kapital <- 1000

mc.okres <- mc.okres
t <- 0:mc.okres
x <- matrix(0,ilosc, mc.okres+1)
for (i in 1:ilosc) { x[i,1:length(sim2)] <- sim2 <- symulacja( 
  dystrybucja = 2,
  okres = mc.okres,
  zmiennosc = 0.039, # 0.0173 dla handlowego
  skutecznosc = 0.43,
  kapital = mc.kapital,
  prowizja = 0.004
)

}

roi <- (x[,(ncol(x)*0.5)]/mc.kapital)-1
mean(roi)
median(roi)
# wykres wszystkich symulacji
x <- cbind(matrix(mc.kapital,ilosc,1),x)
plot(t[1:(mc.okres*0.50)], 
     x[1,1:(mc.okres*0.50)], 
     type = "l", 
     ylim = c(min(x),max(x[,1:(mc.okres*0.5)])))
for (i in 1:ilosc) {lines(t[1:(mc.okres*0.50)],x[i,1:(mc.okres*0.50)], col = i)}
axis(side = 2, at = c(1000,2000,3000,4000,5000,6000))
axis(side = 1, at = c(5,10,15,20,25,30,35,40,45,52))
abline(h= c(0,500,1000,1500,2000,3000,4000,5000), 
       v= c(0,5,10,15,20,25,30,35,40,45,50,52), col="gray", lty=3)


# histogram stóp zwrotu
kolumny <- 40

x.sz <- as.data.frame(x[,1:(mc.okres*0.5)])
stopy.zwrotu <- as.data.frame(c(x.sz[,(mc.okres*0.5)]/mc.kapital))

  abline(v = mean(stopy.zwrotu[,1]))
  
  

ggplot(data = stopy.zwrotu, aes(stopy.zwrotu[,1]))+
  geom_histogram(bins = kolumny, colour="black", fill="lightsteelblue2")+
  geom_vline(aes(xintercept=mean(stopy.zwrotu[,1])),
             color="coral", linetype="dashed", size=1)+
  scale_y_continuous(breaks=seq(0, 
                                nrow(stopy.zwrotu)*0.5, 
                                nrow(stopy.zwrotu)*0.02))+# od 0 do 1400 co 50
  scale_x_continuous(breaks = c(0, 0.5, 1, 1.5,2, 3, 4, 5, 6, 7) )+
  theme(axis.title.x=element_blank())



hist(stopy.zwrotu[,1], breaks = kolumny)
  
summary(stopy.zwrotu)

hist(stopy.zwrotu[,1], breaks = kolumny, col = "lightblue")
abline(v = mean(stopy.zwrotu[,1]), col="red", lwd=3, lty=2)
abline(v = mean(stopy.zwrotu[,1])-sd(stopy.zwrotu[,1]), col="green", lwd=3, lty=2)
abline(v = mean(stopy.zwrotu[,1])+sd(stopy.zwrotu[,1]), col="green", lwd=3, lty=2)
legend(x = "topright", 
       c( "œrednia", "odchylenie standardowe"),
       col = c("red", "green"),
       lwd = c(2, 2, 2))
axis(side = 1,at = c(0,1,2,3,4))


# boxploty dla symulacji

x.m <- x[,1:(ncol(x)*0.50)]
IIIpudelka <- x.m[,c((ncol(x.m)*0.33),
                     (ncol(x.m)*0.66),
                     (ncol(x.m)))]
IVpudelka <- x.m[,c((ncol(x.m)*0.25),
                    (ncol(x.m)*0.5),
                    (ncol(x.m)*0.75),
                    ncol(x.m))]
Xpudelka <- x.m[,c(seq(from = 1, to = ncol(x)*0.5, by = 2))]
pudelka.df <- as.data.frame(Xpudelka)

boxplot(pudelka.df, outline = FALSE)

####### orygina³ ##########
rndm <- rnorm(1000000,0,0.02)
rndm <- as.data.frame(rndm+1)
rndm <- cbind(c(1:nrow(rndm)), 
              rndm, 
              c(rep(0,nrow(rndm))),
              c(rep(0,nrow(rndm))))
names(rndm)[4] <- "inwestycja"
names(rndm)[3] <- "kierunek"
names(rndm)[1] <- "index"
names(rndm)[2] <- "zmiana"
rndm$kierunek[rndm$zmiana > 1] <- "1"
rndm$kierunek[rndm$zmiana < 1] <- "-1"
usun <- rndm$index[rndm$kierunek == "-1"]
usun <- usun[-sample(1:length(usun), size = (length(usun)*0.4343))]
rndm <- rndm[-c(usun),]
table(rndm$kierunek)[2]/sum(table(rndm$kierunek)) # skutecznosc 75% dla 0.3255
rndm$inwestycja <- 100
rndm$inwestycja <- rndm$inwestycja*
  cumprod(rndm$zmiana)*
  (cumprod(rep(0.996,nrow(rndm))))
length(rndm$inwestycja)
plot(rndm$inwestycja, type = "l")

