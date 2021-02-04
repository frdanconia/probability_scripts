library(rmutil)
dlaplace(10, 2, 1)
plaplace(10, 2, 1)
qlaplace(10, 2, 1)
rlaplace(10, 2, 1)

rndm <- r(D)(100)
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
usun <- usun[-sample(1:length(usun), size = (length(usun)*0.3255))]
rndm <- rndm[-c(usun),]
table(rndm$kierunek)[2]/sum(table(rndm$kierunek)) # skutecznosc 75% dla 0.3255
rndm$inwestycja <- 100
rndm$inwestycja <- rndm$inwestycja*
  cumprod(rndm$zmiana)*
  (cumprod(rep(0.996,nrow(rndm))))
length(rndm$inwestycja)
plot(rndm$inwestycja, type = "l")