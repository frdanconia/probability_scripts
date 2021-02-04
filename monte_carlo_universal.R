####### metoda monte carlo #########

#najpierw funkcja generuj¹ca proces stochastyczny
monte_carlo <- function(
  dystrybucja,
  okres,
  zmiennosc,
  kapita³,
  dryf,
  nsymulacji){
  symulacja.prosta <- function(
    dystrybucja1,
    okres1,
    zmiennosc1,
    kapita³1,
    dryf1){   #dryf 1 dla neutralnego, a nie 0 !
    if(dystrybucja1 == 1){
      rndm <- rnorm(okres1, dryf1, zmiennosc1)
    } else if(dystrybucja1 == 2){
      rndm <- rlaplace(okres1, dryf1, zmiennosc1)
    }
    inwestycja <- cumprod(rndm)*kapita³1
    return(inwestycja)
  }
  
  t <- 0:length(okres)
  x <- matrix(0, nsymulacji, ncol = okres)
  for (i in 1:nsymulacji) {x[i,1:length(sim)] <- sim <- symulacja.prosta(
    dystrybucja1 = dystrybucja,
    okres1 = okres,
    zmiennosc1 = zmiennosc,
    kapita³1 = kapita³,
    dryf1 = dryf)
  
  }
  x <- cbind(matrix(kapita³, nsymulacji, ncol = 1),x)
  
  return(x)
}


MC <- monte_carlo(dystrybucja = 1,
                  okres = 50,
                  zmiennosc = 0.0172,
                  kapita³ = 378,
                  dryf = 1.000,
                  nsymulacji = 10000) # wyglada legitnie na pierwszy rzut oka

######### wykres symulacji ##########
osx <- round(seq(0, ncol(MC), ncol(MC)/10), -1)
osy <- round(seq(min(MC), max(MC), (max(MC)-min(MC))/10),-2)

tytu³ <- paste("Symulacja Monte Carlo, n = ", nrow(MC))

plot(MC[1,], type = "l", ylim =c(min(MC), max(MC)), axes = FALSE, 
     main = tytu³,
     xlab = "okres",
     ylab = "kurs symulacji")
abline(h = osy,v = osx, col="gray", lty=3)
for (i in 2:nrow(MC)) {lines(MC[i,], 
                             col = i, lwd = 1)}
axis(side = 1, at = osx) # do ilu zaokr¹gliæ -1 oznacza do 10 a 2 oznacza do 0.1
axis(side = 2, at = osy)


###### teraz histogram ##########
stopy.zwrotu <- as.data.frame((MC[,ncol(MC)]/MC[1,1])-1)
tytu³.histogram <- paste("rozk³ad stóp zwrotu",nrow(MC),"symulacji")


ggplot(data = stopy.zwrotu, aes(stopy.zwrotu[,1]))+ # musi byæ stopy.zwrotu[,1] bo inaczje zwracasz sie do ca³ego data.frame'u
  geom_histogram(bins = 40, colour = "black", fill = "lightsteelblue2")+
  geom_vline(aes(xintercept = mean(stopy.zwrotu[,1])),
             colour = "coral", linetype = "dashed", size = 1)+
  geom_vline(aes(xintercept = mean(stopy.zwrotu[,1]+sd(stopy.zwrotu[,1]))),
             colour = "springgreen3", linetype = "dashed", size =1)+
  geom_vline(aes(xintercept = mean(stopy.zwrotu[,1]-sd(stopy.zwrotu[,1]))),
             colour = "springgreen3", linetype = "dashed", size =1)+
  scale_y_continuous(breaks=seq(0, 
                                nrow(stopy.zwrotu)*0.5, 
                                nrow(stopy.zwrotu)*0.02))+# od 0 do 1400 co 50
  scale_x_continuous(breaks = c(seq(from = round(min(stopy.zwrotu),2), # drugi argument round to do ilu zaogr¹gliæ po przecinku
                                    to = round(max(stopy.zwrotu),2), 
                                    by = round((max(stopy.zwrotu)-min(stopy.zwrotu))/12,2))))+
  theme(axis.title.x=element_blank())+
  labs(title = tytu³.histogram)