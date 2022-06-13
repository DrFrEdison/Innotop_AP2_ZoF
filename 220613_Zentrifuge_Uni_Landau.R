library(devtools); suppressMessages(install_github("DrFrEdison/r4dt", dependencies = T) ); library(r4dt); dt <- list()

setwd(dt$wd <- wd$Innotop$AP2)
setwd("./220613_Zentrifuge_Uni_Landau")
setwd("./spc")

bev <- list()
bev$raw <- fread("220613_au.csv", sep = ";", dec = ",")

head(bev$raw$filenamep)

bev$raw$cell <- substr(bev$raw$filenamep
                       , lapply( gregexpr( "_" , bev$raw$filenamep), function( x ) x[[3]] + 1)
                       , lapply( gregexpr( "_" , bev$raw$filenamep), function( x ) x[[4]] - 1))

bev$raw$beer <- substr(bev$raw$filenamep
                       , lapply( gregexpr( "_" , bev$raw$filenamep), function( x ) x[[4]] + 1)
                       , lapply( gregexpr( "_" , bev$raw$filenamep), function( x ) x[[5]] - 1))

bev$raw$rcf <- substr(bev$raw$filenamep
                       , lapply( gregexpr( "_" , bev$raw$filenamep), function( x ) x[[5]] + 1)
                       , lapply( gregexpr( "_" , bev$raw$filenamep), function( x ) x[[6]] - 1))

bev$raw$duration <- substr(bev$raw$filenamep
                      , lapply( gregexpr( "_" , bev$raw$filenamep), function( x ) x[[6]] + 1)
                      , lapply( gregexpr( "_" , bev$raw$filenamep), function( x ) x[[7]] - 1))

unique( bev$raw$cell )
unique( bev$raw$beer )
unique( bev$raw$rcf )
unique( bev$raw$duration )

bev$raw$cell <- factor(bev$raw$cell)
bev$raw$beer <- factor(bev$raw$beer)
bev$raw$duration <- factor( formatC( as.numeric(gsub( "s", "", bev$raw$duration) ), width = 4, format = "d", flag = "0"))
bev$raw$rcf <- factor( formatC( as.numeric(gsub( "rcf", "", bev$raw$rcf ) ), width = 4, format = "d", flag = "0"))

names(bev$raw)
for(i in 190:598)
  bev$raw <- bev$raw[ , moveme(names(bev$raw), paste(i, "last")), with = F]


bev$trsnum <- transfer_csv.num.col( bev$raw )

colfunc <- colorRampPalette(c("red", "blue", "darkgreen", "orange"))

# DT0041 Goecklinger 800 rcf ####
bev$sub$DT0041_Goecklinger_800rcf <- bev$raw[ bev$raw$cell == "DT0041" & bev$raw$beer == "Goecklinger" & bev$raw$rcf == "0800" | bev$raw$rcf == "0000"]
bev$sub$DT0041_Goecklinger_800rcf <- bev$sub$DT0041_Goecklinger_800rcf[ bev$sub$DT0041_Goecklinger_800rcf$beer == "Goecklinger" ]
bev$sub$DT0041_Goecklinger_800rcf <- bev$sub$DT0041_Goecklinger_800rcf[order(bev$sub$DT0041_Goecklinger_800rcf$rcf)]
bev$sub$DT0041_Goecklinger_800rcf <- droplevels(bev$sub$DT0041_Goecklinger_800rcf)

bev$par$DT0041_Goecklinger_800rcf$colp <- colfunc(length( levels(bev$sub$DT0041_Goecklinger_800rcf$duration) ))
bev$par$DT0041_Goecklinger_800rcf$colp2 <- bev$par$DT0041_Goecklinger_800rcf$colp[ bev$sub$DT0041_Goecklinger_800rcf$duration ]
bev$par$DT0041_Goecklinger_800rcf$legendt <- as.character(paste0(as.character( levels(bev$sub$DT0041_Goecklinger_800rcf$duration) ), " Sekunden bei 800 rcf"))

bev$sub$DT0041_Goecklinger_800rcf <- transfer_csv(bev$sub$DT0041_Goecklinger_800rcf)

par(mfrow = c(1,1))
matplot(bev$trsnum$wl
        , t(bev$sub$DT0041_Goecklinger_800rcf$spc)
        , lty = 1, type = "l"
        , xlab = lambda, ylab = "AU", main = "Goecklinger Bier bei 800 rcf"
        , col = bev$par$DT0041_Goecklinger_800rcf$colp2)

legend("topright", legend = bev$par$DT0041_Goecklinger_800rcf$legendt
       , col = bev$par$DT0041_Goecklinger_800rcf$colp, lty = 1)

boxplot(bev$sub$DT0041_Goecklinger_800rcf$spc$X210 ~ bev$sub$DT0041_Goecklinger_800rcf$data$duration
        , col = bev$par$DT0041_Goecklinger_800rcf$colp, pch = 20
        , xlab = "Zentrifugationsdauer in Sekunden", ylab = "AU bei 210 nm")
boxplot(bev$sub$DT0041_Goecklinger_800rcf$spc$X450 ~ bev$sub$DT0041_Goecklinger_800rcf$data$duration
        , col = bev$par$DT0041_Goecklinger_800rcf$colp, pch = 20, log = "y"
        , xlab = "Zentrifugationsdauer in Sekunden", ylab = "AU bei 450 nm")
boxplot(bev$sub$DT0041_Goecklinger_800rcf$spc1st$X210 ~ bev$sub$DT0041_Goecklinger_800rcf$data$duration
        , col = bev$par$DT0041_Goecklinger_800rcf$colp, pch = 20
        , xlab = "Zentrifugationsdauer in Sekunden", ylab = expression(paste(Delta, " AU / ", Delta, lambda, 210)))


# DT0041 Goecklinger Dauer 600 ####
bev$sub$DT0041_Goecklinger_600s <- bev$raw[ bev$raw$cell == "DT0041" & bev$raw$beer == "Goecklinger" & bev$raw$duration == "0600" | bev$raw$duration == "0000"]
bev$sub$DT0041_Goecklinger_600s <- bev$sub$DT0041_Goecklinger_600s[ bev$sub$DT0041_Goecklinger_600s$beer == "Goecklinger" ]
bev$sub$DT0041_Goecklinger_600s <- bev$sub$DT0041_Goecklinger_600s[order(bev$sub$DT0041_Goecklinger_600s$rcf)]
bev$sub$DT0041_Goecklinger_600s <- droplevels(bev$sub$DT0041_Goecklinger_600s)

bev$par$DT0041_Goecklinger_600s$colp <- colfunc(length( levels(bev$sub$DT0041_Goecklinger_600s$rcf) ))
bev$par$DT0041_Goecklinger_600s$colp2 <- bev$par$DT0041_Goecklinger_600s$colp[ bev$sub$DT0041_Goecklinger_600s$rcf ]
bev$par$DT0041_Goecklinger_600s$legendt <- as.character(paste0(as.character( levels(bev$sub$DT0041_Goecklinger_600s$rcf) ), " rcf bei 600 Sekunden"))

bev$sub$DT0041_Goecklinger_600s <- transfer_csv(bev$sub$DT0041_Goecklinger_600s)

matplot(bev$trsnum$wl
        , t(bev$sub$DT0041_Goecklinger_600s$spc)
        , lty = 1, type = "l"
        , xlab = lambda, ylab = "AU", main = "Goecklinger Bier bei 800 rcf"
        , col = bev$par$DT0041_Goecklinger_600s$colp2)

legend("topright", legend = bev$par$DT0041_Goecklinger_600s$legendt
       , col = bev$par$DT0041_Goecklinger_600s$colp, lty = 1)

boxplot(bev$sub$DT0041_Goecklinger_600s$spc$X210 ~ bev$sub$DT0041_Goecklinger_600s$data$rcf
        , col = bev$par$DT0041_Goecklinger_600s$colp, pch = 20
        , xlab = "Relative Zentrifugalkraft", ylab = "AU bei 210 nm")
boxplot(bev$sub$DT0041_Goecklinger_600s$spc$X450 ~ bev$sub$DT0041_Goecklinger_600s$data$rcf
        , col = bev$par$DT0041_Goecklinger_600s$colp, pch = 20, log = "y"
        , xlab = "Relative Zentrifugalkraft", ylab = "AU bei 450 nm")
boxplot(bev$sub$DT0041_Goecklinger_600s$spc1st$X210 ~ bev$sub$DT0041_Goecklinger_600s$data$rcf
        , col = bev$par$DT0041_Goecklinger_600s$colp, pch = 20
        , xlab = "Relative Zentrifugalkraft", ylab = expression(paste(Delta, " AU / ", Delta, lambda, 210)))

# DT0001 Goecklinger 800 rcf ####
bev$sub$DT0001_Goecklinger_800rcf <- bev$raw[ bev$raw$cell == "DT0001" & bev$raw$beer == "Goecklinger" & bev$raw$rcf == "0800" | bev$raw$rcf == "0000"]
bev$sub$DT0001_Goecklinger_800rcf <- bev$sub$DT0001_Goecklinger_800rcf[ bev$sub$DT0001_Goecklinger_800rcf$beer == "Goecklinger" ]
bev$sub$DT0001_Goecklinger_800rcf <- bev$sub$DT0001_Goecklinger_800rcf[order(bev$sub$DT0001_Goecklinger_800rcf$rcf)]
bev$sub$DT0001_Goecklinger_800rcf <- droplevels(bev$sub$DT0001_Goecklinger_800rcf)

bev$par$DT0001_Goecklinger_800rcf$colp <- colfunc(length( levels(bev$sub$DT0001_Goecklinger_800rcf$duration) ))
bev$par$DT0001_Goecklinger_800rcf$colp2 <- bev$par$DT0001_Goecklinger_800rcf$colp[ bev$sub$DT0001_Goecklinger_800rcf$duration ]
bev$par$DT0001_Goecklinger_800rcf$legendt <- as.character(paste0(as.character( levels(bev$sub$DT0001_Goecklinger_800rcf$duration) ), " Sekunden bei 800 rcf"))

bev$sub$DT0001_Goecklinger_800rcf <- transfer_csv(bev$sub$DT0001_Goecklinger_800rcf)

matplot(bev$trsnum$wl
        , t(bev$sub$DT0001_Goecklinger_800rcf$spc)
        , lty = 1, type = "l"
        , xlab = lambda, ylab = "AU", main = "Goecklinger Bier bei 800 rcf"
        , col = bev$par$DT0001_Goecklinger_800rcf$colp2)

legend("topright", legend = bev$par$DT0001_Goecklinger_800rcf$legendt
       , col = bev$par$DT0001_Goecklinger_800rcf$colp, lty = 1)

boxplot(bev$sub$DT0001_Goecklinger_800rcf$spc$X210 ~ bev$sub$DT0001_Goecklinger_800rcf$data$duration
        , col = bev$par$DT0001_Goecklinger_800rcf$colp, pch = 20
        , xlab = "Zentrifugationsdauer in Sekunden", ylab = "AU bei 210 nm")
boxplot(bev$sub$DT0001_Goecklinger_800rcf$spc$X450 ~ bev$sub$DT0001_Goecklinger_800rcf$data$duration
        , col = bev$par$DT0001_Goecklinger_800rcf$colp, pch = 20, log = "y"
        , xlab = "Zentrifugationsdauer in Sekunden", ylab = "AU bei 450 nm")
boxplot(bev$sub$DT0001_Goecklinger_800rcf$spc1st$X210 ~ bev$sub$DT0001_Goecklinger_800rcf$data$duration
        , col = bev$par$DT0001_Goecklinger_800rcf$colp, pch = 20
        , xlab = "Zentrifugationsdauer in Sekunden", ylab = expression(paste(Delta, " AU / ", Delta, lambda, 210)))


# DT0001 Goecklinger Dauer 600 ####
bev$sub$DT0001_Goecklinger_600s <- bev$raw[ bev$raw$cell == "DT0001" & bev$raw$beer == "Goecklinger" & bev$raw$duration == "0600" | bev$raw$duration == "0000"]
bev$sub$DT0001_Goecklinger_600s <- bev$sub$DT0001_Goecklinger_600s[ bev$sub$DT0001_Goecklinger_600s$beer == "Goecklinger" ]
bev$sub$DT0001_Goecklinger_600s <- bev$sub$DT0001_Goecklinger_600s[order(bev$sub$DT0001_Goecklinger_600s$rcf)]
bev$sub$DT0001_Goecklinger_600s <- droplevels(bev$sub$DT0001_Goecklinger_600s)

bev$par$DT0001_Goecklinger_600s$colp <- colfunc(length( levels(bev$sub$DT0001_Goecklinger_600s$rcf) ))
bev$par$DT0001_Goecklinger_600s$colp2 <- bev$par$DT0001_Goecklinger_600s$colp[ bev$sub$DT0001_Goecklinger_600s$rcf ]
bev$par$DT0001_Goecklinger_600s$legendt <- as.character(paste0(as.character( levels(bev$sub$DT0001_Goecklinger_600s$rcf) ), " rcf bei 600 Sekunden"))

bev$sub$DT0001_Goecklinger_600s <- transfer_csv(bev$sub$DT0001_Goecklinger_600s)

matplot(bev$trsnum$wl
        , t(bev$sub$DT0001_Goecklinger_600s$spc)
        , lty = 1, type = "l"
        , xlab = lambda, ylab = "AU", main = "Goecklinger Bier bei 800 rcf"
        , col = bev$par$DT0001_Goecklinger_600s$colp2)

legend("topright", legend = bev$par$DT0001_Goecklinger_600s$legendt
       , col = bev$par$DT0001_Goecklinger_600s$colp, lty = 1)

boxplot(bev$sub$DT0001_Goecklinger_600s$spc$X210 ~ bev$sub$DT0001_Goecklinger_600s$data$rcf
        , col = bev$par$DT0001_Goecklinger_600s$colp, pch = 20
        , xlab = "Relative Zentrifugalkraft", ylab = "AU bei 210 nm")
boxplot(bev$sub$DT0001_Goecklinger_600s$spc$X450 ~ bev$sub$DT0001_Goecklinger_600s$data$rcf
        , col = bev$par$DT0001_Goecklinger_600s$colp, pch = 20, log = "y"
        , xlab = "Relative Zentrifugalkraft", ylab = "AU bei 450 nm")
boxplot(bev$sub$DT0001_Goecklinger_600s$spc1st$X210 ~ bev$sub$DT0001_Goecklinger_600s$data$rcf
        , col = bev$par$DT0001_Goecklinger_600s$colp, pch = 20
        , xlab = "Relative Zentrifugalkraft", ylab = expression(paste(Delta, " AU / ", Delta, lambda, 210)))


# DT0041 Alle Biersorten Vergleich ####
bev$sub$DT0041_000rcf_800rcf_240s <- bev$raw[ bev$raw$cell == "DT0041" & bev$raw$duration == "0240" | bev$raw$duration == "0000"]
bev$sub$DT0041_000rcf_800rcf_240s <- bev$sub$DT0041_000rcf_800rcf_240s[bev$sub$DT0041_000rcf_800rcf_240s$cell == "DT0041"]
bev$sub$DT0041_000rcf_800rcf_240s <- bev$sub$DT0041_000rcf_800rcf_240s[bev$sub$DT0041_000rcf_800rcf_240s$beer != "Hefe1zu10"]
bev$sub$DT0041_000rcf_800rcf_240s <- bev$sub$DT0041_000rcf_800rcf_240s[order(bev$sub$DT0041_000rcf_800rcf_240s$beer, bev$sub$DT0041_000rcf_800rcf_240s$rcf)]
bev$sub$DT0041_000rcf_800rcf_240s <- droplevels(bev$sub$DT0041_000rcf_800rcf_240s)


# bev$par$DT0041_000rcf_800rcf_240s$colp <- colfunc(length( levels(bev$sub$DT0041_000rcf_800rcf_240s$beer) ))
# bev$par$DT0041_000rcf_800rcf_240s$colp2 <- bev$par$DT0041_000rcf_800rcf_240s$colp[ bev$sub$DT0041_000rcf_800rcf_240s$beer ]
bev$par$DT0041_000rcf_800rcf_240s$colp <- colfunc(length( levels(interaction(bev$sub$DT0041_000rcf_800rcf_240s$rcf, bev$sub$DT0041_000rcf_800rcf_240s$beer, drop = T))))
bev$par$DT0041_000rcf_800rcf_240s$colp2 <- bev$par$DT0041_000rcf_800rcf_240s$colp[ interaction(bev$sub$DT0041_000rcf_800rcf_240s$rcf, bev$sub$DT0041_000rcf_800rcf_240s$beer, drop = T) ]

bev$par$DT0041_000rcf_800rcf_240s$lty <- c(3,1)
bev$par$DT0041_000rcf_800rcf_240s$lty2 <- bev$par$DT0041_000rcf_800rcf_240s$lty[ bev$sub$DT0041_000rcf_800rcf_240s$rcf ]

bev$par$DT0041_000rcf_800rcf_240s$lwd <- c(1,1)
bev$par$DT0041_000rcf_800rcf_240s$lwd2 <- bev$par$DT0041_000rcf_800rcf_240s$lwd2[ bev$sub$DT0041_000rcf_800rcf_240s$duration ]

bev$par$DT0041_000rcf_800rcf_240s$legendt <- paste( rep(as.character(paste0(as.character( levels(bev$sub$DT0041_000rcf_800rcf_240s$beer) ))), each = 2), c("pur", "zentrifugiert"))

bev$sub$DT0041_000rcf_800rcf_240s <- transfer_csv(bev$sub$DT0041_000rcf_800rcf_240s)

bev$sub$DT0041_000rcf_800rcf_240s$data

par(mfrow = c(2,2))
for(i in 1:4){
  xx <- bev$sub$DT0041_000rcf_800rcf_240s$data$beer == levels(bev$sub$DT0041_000rcf_800rcf_240s$data$beer)[ i ]
  matplot(bev$trsnum$wl
          , t(bev$sub$DT0041_000rcf_800rcf_240s$spc)[ , xx ]
          , lty = bev$par$DT0041_000rcf_800rcf_240s$lty2 [ xx ], type = "l"
          , xlab = lambda, ylab = "AU", main = paste(levels(bev$sub$DT0041_000rcf_800rcf_240s$data$beer)[ i ], " bei 800 rcf und 240 Sekunden")
          , col = bev$par$DT0041_000rcf_800rcf_240s$colp2[ xx ]
          , lwd = bev$par$DT0041_000rcf_800rcf_240s$pch2[ xx ]
          , ylim = c(0, 5))
}

# PCA ####
# DT0041 ####
bev$sub$DT0041 <- bev$raw[ bev$raw$cell == "DT0041"]
bev$sub$DT0041 <- bev$sub$DT0041[ bev$sub$DT0041$beer == "Goecklinger"]
# bev$sub$DT0041 <- bev$sub$DT0041[ bev$sub$DT0041$beer != "Hefe"]
# bev$sub$DT0041 <- bev$sub$DT0041[ bev$sub$DT0041$rcf != "0000"]

# bev$sub$DT0041 <- bev$sub$DT0041[ bev$sub$DT0041$beer == "Goecklinger" ]
# bev$sub$DT0041 <- bev$sub$DT0041[order(bev$sub$DT0041$rcf)]
bev$sub$DT0041 <- droplevels(bev$sub$DT0041)

bev$par$DT0041$colp <- colfunc(length( levels(bev$sub$DT0041$rcf) ))
bev$par$DT0041$colp2 <- bev$par$DT0041$colp[ bev$sub$DT0041$rcf ]

bev$par$DT0041$pch <- c(15,17,19,2)
bev$par$DT0041$pch2 <- bev$par$DT0041$pch[ bev$sub$DT0041$beer ]

bev$par$DT0041$legendt <- as.character( levels( bev$sub$DT0041$beer ) )
bev$par$DT0041$legendt2 <- as.character( levels( bev$sub$DT0041$rcf ) )

bev$sub$DT0041 <- transfer_csv(bev$sub$DT0041)
bev$sub$DT0041$data

bev$sub$DT0041$pca300450 <- pca(bev$sub$DT0041$spc [ , which(bev$sub$DT0041$wl %in% 300) : which(bev$sub$DT0041$wl %in% 450)])


par(mfrow = c(1,1))
PCx = 1
PCy = 3
plot(bev$sub$DT0041$pca300450$calres$scores[ , PCx], bev$sub$DT0041$pca300450$calres$scores[ , PCy]
     , xlab = paste("PC", PCx, "Explained Variance =", round( bev$sub$DT0041$pca300450$calres$expvar[ PCx ] ,1), "%")
     , ylab = paste("PC", PCy, "Explained Variance =", round( bev$sub$DT0041$pca300450$calres$expvar[ PCy ] ,1), "%")
     , col = bev$par$DT0041$colp2
     , pch = bev$par$DT0041$pch2
     , cex = 2)

legend("topleft", bev$par$DT0041$legendt, pch = bev$par$DT0041$pch)
legend("topright", bev$par$DT0041$legendt2, pch = 20, col = bev$par$DT0041$colp)



