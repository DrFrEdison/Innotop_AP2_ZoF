# Package update and initialization ####
library(devtools)
suppressMessages(install_github("DrFrEdison/r4dt", dependencies = T, upgrade = "always", quiet = T) )
suppressPackageStartupMessages(library(r4dt))

# Beer zentrifugation experiment in Landau ####
bev <- list()

# read spc from csv
setwd(bev$wd <- wd$Innotop$AP2)
setwd("./220613_Zentrifuge_Uni_Landau")
setwd("./spc")

bev$raw <- fread("220617_au.csv", sep = ";", dec = ",")

# get parameter for cell, beer, rcf and centrifugation duration
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

# order data.table
for(i in 190:598)
  bev$raw <- bev$raw[ , moveme(names(bev$raw), paste(i, "last")), with = F]

# read xlsx with EBC (European Brewery Convention) values
setwd(bev$wd)
setwd("./220613_Zentrifuge_Uni_Landau")

bev$EBCraw <- read.xlsx("220610_Versuchsreihe_Goecklinger_Oettinger_Hefe_Uni_LD.xlsx")
names(bev$EBCraw)[2] <- "beer"
names(bev$EBCraw)[3] <- "rcf"
names(bev$EBCraw)[4] <- "duration"
names(bev$EBCraw)[5] <- "Turbidity"

bev$EBCraw$beer <- factor(bev$EBCraw$beer)
bev$EBCraw$duration <- factor( formatC( as.numeric(gsub( "s", "", bev$EBCraw$duration) ), width = 4, format = "d", flag = "0"))
bev$EBCraw$rcf <- factor( formatC( as.numeric(gsub( "rcf", "", bev$EBCraw$rcf ) ), width = 4, format = "d", flag = "0"))

# calculate mean and sd
bev$EBCraw <- data.table(bev$EBCraw)
bev$EBC <- ddply(bev$EBCraw, .(beer, rcf, duration), summarize
                 , mean(Turbidity)
                 , sd(Turbidity))

bev$merge <- merge.data.table(bev$raw, bev$EBC, by = c("beer", "duration", "rcf"), all = T)
for(i in 190:598)
  bev$merge <- bev$merge[ , moveme(names(bev$merge), paste(i, "last")), with = F]

nrow(bev$raw)
nrow(bev$EBC)
nrow(bev$merge)

# trs
bev$trsnum <- transfer_csv.num.col( bev$raw )

# colfunction ####
colfunc <- colorRampPalette(c("red", "blue", "darkgreen", "orange"))

# DT0041 Goecklinger 800 rcf ####
bev$sub$DT0041_Goecklinger_800rcf <- bev$merge[ bev$merge$cell == "DT0041" & bev$merge$beer == "Goecklinger" & bev$merge$rcf == "0800" | bev$merge$rcf == "0000"]
bev$sub$DT0041_Goecklinger_800rcf <- bev$sub$DT0041_Goecklinger_800rcf[ bev$sub$DT0041_Goecklinger_800rcf$beer == "Goecklinger" ]
bev$sub$DT0041_Goecklinger_800rcf <- bev$sub$DT0041_Goecklinger_800rcf[order(bev$sub$DT0041_Goecklinger_800rcf$rcf)]
bev$sub$DT0041_Goecklinger_800rcf <- bev$sub$DT0041_Goecklinger_800rcf[ !is.na(bev$sub$DT0041_Goecklinger_800rcf$`500`) , ]
bev$sub$DT0041_Goecklinger_800rcf <- droplevels(bev$sub$DT0041_Goecklinger_800rcf)

bev$par$DT0041_Goecklinger_800rcf$colp <- colfunc(length( levels(bev$sub$DT0041_Goecklinger_800rcf$duration) ))
bev$par$DT0041_Goecklinger_800rcf$colp2 <- bev$par$DT0041_Goecklinger_800rcf$colp[ bev$sub$DT0041_Goecklinger_800rcf$duration ]

bev$par$DT0041_Goecklinger_800rcf$pchp <- c(15:19, 0:2, 5)
bev$par$DT0041_Goecklinger_800rcf$pchp2 <- bev$par$DT0041_Goecklinger_800rcf$pchp[ bev$sub$DT0041_Goecklinger_800rcf$duration ]

bev$par$DT0041_Goecklinger_800rcf$legendt <- as.character(paste0(as.character( as.numeric( levels(bev$sub$DT0041_Goecklinger_800rcf$duration))), " s ~ 800 rcf"))

bev$sub$DT0041_Goecklinger_800rcf <- transfer_csv(bev$sub$DT0041_Goecklinger_800rcf)

png(paste0(date(),"_DT0041_Goecklinger_800rcf_spc.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
par(mfrow = c(1,1), mar = c(4,4,3,1))
matplot(bev$trsnum$wl
        , t(bev$sub$DT0041_Goecklinger_800rcf$spc)
        , lty = 1, type = "l"
        , xlab = lambda, ylab = "AU", main = "Spektren - Goecklinger Hausbräu bei 800 rcf, 2 mm Pfadlaenge"
        , col = bev$par$DT0041_Goecklinger_800rcf$colp2)

legend("topright", legend = bev$par$DT0041_Goecklinger_800rcf$legendt
       , col = bev$par$DT0041_Goecklinger_800rcf$colp, lty = 1)
dev.off()

bev$sub$DT0041_Goecklinger_800rcf$pca300500 <- pca(bev$sub$DT0041_Goecklinger_800rcf$spc[ , which( bev$sub$DT0041_Goecklinger_800rcf$wl %in% 300 ) : which( bev$sub$DT0041_Goecklinger_800rcf$wl %in% 500 )], ncomp = 4)

png(paste0(date(),"_DT0041_Goecklinger_800rcf_pca.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")

layout(matrix(c(1,2,3,4,5,5), byrow = T, ncol = 2), heights = c(1, 1, .3))

par(mar = c(4,4,3,1))
PCx = 1
PCy = 2
plot(bev$sub$DT0041_Goecklinger_800rcf$pca300500$calres$scores[ , PCx], bev$sub$DT0041_Goecklinger_800rcf$pca300500$calres$scores[ , PCy]
     , xlab = paste("PC", PCx, "Explained Variance =", round( bev$sub$DT0041_Goecklinger_800rcf$pca300500$calres$expvar[ PCx ] ,1), "%")
     , ylab = paste("PC", PCy, "Explained Variance =", round( bev$sub$DT0041_Goecklinger_800rcf$pca300500$calres$expvar[ PCy ] ,1), "%")
     , col = bev$par$DT0041_Goecklinger_800rcf$colp2
     , pch = bev$par$DT0041_Goecklinger_800rcf$pchp2
     , cex = 2
     , main = "PCA - Goecklinger Hausbräu bei 800 rcf, 2 mm Pfadlaenge")

boxplot(bev$sub$DT0041_Goecklinger_800rcf$spc$X450 ~ bev$sub$DT0041_Goecklinger_800rcf$data$duration
        , col = bev$par$DT0041_Goecklinger_800rcf$colp, pch = 20, log = "y"
        , xlab = "Zentrifugationsdauer in Sekunden", ylab = "AU bei 450 nm"
        , main = "AU_450nm - Goecklinger Hausbräu bei 800 rcf, 2 mm Pfadlaenge")

boxplot(bev$sub$DT0041_Goecklinger_800rcf$pca$calres$scores[ , 1] ~ bev$sub$DT0041_Goecklinger_800rcf$data$duration
        , col = bev$par$DT0041_Goecklinger_800rcf$colp, pch = 20
        , xlab = "Zentrifugationsdauer in Sekunden", ylab = "PC 1"
        , main = "PC1 - Goecklinger Hausbräu bei 800 rcf, 2 mm Pfadlaenge")

boxplot(bev$sub$DT0041_Goecklinger_800rcf$pca$calres$scores[ , 2] ~ bev$sub$DT0041_Goecklinger_800rcf$data$duration
        , col = bev$par$DT0041_Goecklinger_800rcf$colp, pch = 20
        , xlab = "Zentrifugationsdauer in Sekunden", ylab = "PC 2"
        , main = "PC2 - Goecklinger Hausbräu bei 800 rcf, 2 mm Pfadlaenge")

par(mar = c(0,0,0,0))
plot(1,1, type = "n", axes = F, xlab = "", ylab = "")
legend("center", legend = bev$par$DT0041_Goecklinger_800rcf$legendt
       , col = bev$par$DT0041_Goecklinger_800rcf$colp
       , pch = bev$par$DT0041_Goecklinger_800rcf$pchp
       , xpd = T, ncol = 5, bty = "n", cex = 1.5, pt.cex = 2, pt.lwd = 2)

dev.off()



bev$sub$DT0041_Goecklinger_800rcf$pca300500_sub <- pca(bev$sub$DT0041_Goecklinger_800rcf$spc[ bev$sub$DT0041_Goecklinger_800rcf$data$rcf > 0 , which( bev$sub$DT0041_Goecklinger_800rcf$wl %in% 300 ) : which( bev$sub$DT0041_Goecklinger_800rcf$wl %in% 500 )], ncomp = 4)

png(paste0(date(),"_DT0041_Goecklinger_800rcf_pca_sub.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")

layout(matrix(c(1,2,3,4,5,5), byrow = T, ncol = 2), heights = c(1, 1, .3))

par(mar = c(4,4,3,1))
PCx = 1
PCy = 2
plot(bev$sub$DT0041_Goecklinger_800rcf$pca300500_sub$calres$scores[ , PCx], bev$sub$DT0041_Goecklinger_800rcf$pca300500_sub$calres$scores[ , PCy]
     , xlab = paste("PC", PCx, "Explained Variance =", round( bev$sub$DT0041_Goecklinger_800rcf$pca300500_sub$calres$expvar[ PCx ] ,1), "%")
     , ylab = paste("PC", PCy, "Explained Variance =", round( bev$sub$DT0041_Goecklinger_800rcf$pca300500_sub$calres$expvar[ PCy ] ,1), "%")
     , col = bev$par$DT0041_Goecklinger_800rcf$colp2[ bev$sub$DT0041_Goecklinger_800rcf$data$rcf > 0 ]
     , pch = bev$par$DT0041_Goecklinger_800rcf$pchp2[ bev$sub$DT0041_Goecklinger_800rcf$data$rcf > 0 ]
     , cex = 2
     , main = "PCA - Goecklinger Hausbräu bei 800 rcf, 2 mm Pfadlaenge")

boxplot(bev$sub$DT0041_Goecklinger_800rcf$spc$X450[ bev$sub$DT0041_Goecklinger_800rcf$data$rcf > 0 ] ~ bev$sub$DT0041_Goecklinger_800rcf$data$duration[ bev$sub$DT0041_Goecklinger_800rcf$data$rcf > 0 ]
        , col = bev$par$DT0041_Goecklinger_800rcf$colp[ -1 ], pch = 20, log = "y"
        , xlab = "Zentrifugationsdauer in Sekunden", ylab = "AU bei 450 nm"
        , main = "AU_450nm - Goecklinger Hausbräu bei 800 rcf, 2 mm Pfadlaenge")

boxplot(bev$sub$DT0041_Goecklinger_800rcf$pca300500_sub$calres$scores[ , 1] ~ bev$sub$DT0041_Goecklinger_800rcf$data$duration[ bev$sub$DT0041_Goecklinger_800rcf$data$rcf > 0 ]
        , col = bev$par$DT0041_Goecklinger_800rcf$colp[ -1 ], pch = 20
        , xlab = "Zentrifugationsdauer in Sekunden", ylab = "PC 1"
        , main = "PC1 - Goecklinger Hausbräu bei 800 rcf, 2 mm Pfadlaenge")

boxplot(bev$sub$DT0041_Goecklinger_800rcf$pca300500_sub$calres$scores[ , 2] ~ bev$sub$DT0041_Goecklinger_800rcf$data$duration[ bev$sub$DT0041_Goecklinger_800rcf$data$rcf > 0 ]
        , col = bev$par$DT0041_Goecklinger_800rcf$colp[ -1 ], pch = 20
        , xlab = "Zentrifugationsdauer in Sekunden", ylab = "PC 2"
        , main = "PC2 - Goecklinger Hausbräu bei 800 rcf, 2 mm Pfadlaenge")

par(mar = c(0,0,0,0))
plot(1,1, type = "n", axes = F, xlab = "", ylab = "")
legend("center", legend = bev$par$DT0041_Goecklinger_800rcf$legendt[ - 1]
       , col = bev$par$DT0041_Goecklinger_800rcf$colp[ - 1 ]
       , pch = bev$par$DT0041_Goecklinger_800rcf$pchp[ - 1 ]
       , xpd = T, ncol = 5, bty = "n", cex = 1.5, pt.cex = 2, pt.lwd = 2)

dev.off()

png(paste0(date(),"_DT0041_Goecklinger_800rcf_spc_zoom.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
par(mfrow = c(1,1), mar = c(4,4,3,1))
matplot(bev$trsnum$wl
        , t(bev$sub$DT0041_Goecklinger_800rcf$spc)
        , lty = 1, type = "l"
        , xlab = lambda, ylab = "AU", main = "Spektren - Goecklinger Hausbräu bei 800 rcf, 2 mm Pfadlaenge"
        , col = bev$par$DT0041_Goecklinger_800rcf$colp2
        , xlim = c(300, 500)
        , ylim = c(0, 2))

legend("bottomleft", legend = bev$par$DT0041_Goecklinger_800rcf$legendt
       , col = bev$par$DT0041_Goecklinger_800rcf$colp, lty = 1)
dev.off()

png(paste0(date(),"_DT0001_Goecklinger_800rcf_spc_zoom_UV.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
par(mfrow = c(1,1), mar = c(4,4,3,1))
matplot(bev$trsnum$wl
        , t(bev$sub$DT0001_Goecklinger_800rcf$spc)
        , lty = 1, type = "l"
        , xlab = lambda, ylab = "AU", main = "Spektren - Goecklinger Hausbräu bei 800 rcf, 2 mm Pfadlaenge"
        , col = bev$par$DT0001_Goecklinger_800rcf$colp2
        , xlim = c(190, 300)
        , ylim = c(.5, 4.5))

legend("topright", legend = bev$par$DT0001_Goecklinger_800rcf$legendt
       , col = bev$par$DT0001_Goecklinger_800rcf$colp, lty = 1, ncol = 2)
dev.off()

# DT0041 Goecklinger Dauer 600 ####
bev$sub$DT0041_Goecklinger_600s <- bev$merge[ bev$merge$cell == "DT0041" & bev$merge$beer == "Goecklinger" & bev$merge$duration == "0600" | bev$merge$duration == "0000"]
bev$sub$DT0041_Goecklinger_600s <- bev$sub$DT0041_Goecklinger_600s[ bev$sub$DT0041_Goecklinger_600s$beer == "Goecklinger" ]
bev$sub$DT0041_Goecklinger_600s <- bev$sub$DT0041_Goecklinger_600s[order(bev$sub$DT0041_Goecklinger_600s$rcf)]
bev$sub$DT0041_Goecklinger_600s <- droplevels(bev$sub$DT0041_Goecklinger_600s)

bev$par$DT0041_Goecklinger_600s$colp <- colfunc(length( levels(bev$sub$DT0041_Goecklinger_600s$rcf) ))
bev$par$DT0041_Goecklinger_600s$colp2 <- bev$par$DT0041_Goecklinger_600s$colp[ bev$sub$DT0041_Goecklinger_600s$rcf ]
bev$par$DT0041_Goecklinger_600s$legendt <- as.character(paste0(as.character( as.numeric(levels(bev$sub$DT0041_Goecklinger_600s$rcf)) ), " rcf ~ 600 s"))

bev$par$DT0041_Goecklinger_600s$pchp <- c(15:19, 0:2, 5)
bev$par$DT0041_Goecklinger_600s$pchp2 <- bev$par$DT0041_Goecklinger_600s$pchp[ bev$sub$DT0041_Goecklinger_600s$rcf ]

bev$sub$DT0041_Goecklinger_600s <- transfer_csv(bev$sub$DT0041_Goecklinger_600s)

bev$sub$DT0041_Goecklinger_600s$pca300500 <- pca(bev$sub$DT0041_Goecklinger_600s$spc[ , which( bev$sub$DT0041_Goecklinger_600s$wl %in% 300 ) : which( bev$sub$DT0041_Goecklinger_600s$wl %in% 500 )], ncomp = 4)

png(paste0(date(),"_DT0041_Goecklinger_600s_spc.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
par(mfrow = c(1,1), mar = c(4,4,3,1))
matplot(bev$trsnum$wl
        , t(bev$sub$DT0041_Goecklinger_600s$spc)
        , lty = 1, type = "l"
        , xlab = lambda, ylab = "AU", main = "Spektren - Goecklinger Hausbräu, 600 Sekunden, 2 mm Pfadlaenge"
        , col = bev$par$DT0041_Goecklinger_600s$colp2)

legend("topright", legend = bev$par$DT0041_Goecklinger_600s$legendt
       , col = bev$par$DT0041_Goecklinger_600s$colp, lty = 1)
dev.off()

png(paste0(date(),"_DT0041_Goecklinger_600s_spc_zoom.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
par(mfrow = c(1,1), mar = c(4,4,3,1))
matplot(bev$trsnum$wl
        , t(bev$sub$DT0041_Goecklinger_600s$spc)
        , lty = 1, type = "l"
        , xlab = lambda, ylab = "AU", main = "Spektren - Goecklinger Hausbräu, 600 Sekunden, 2 mm Pfadlaenge"
        , col = bev$par$DT0041_Goecklinger_600s$colp2
        , xlim = c(300, 500)
        , ylim = c(0, 2)
)

legend("bottomleft", legend = bev$par$DT0041_Goecklinger_600s$legendt
       , col = bev$par$DT0041_Goecklinger_600s$colp, lty = 1)
dev.off()

png(paste0(date(),"_DT0001_Goecklinger_600s_spc_zoom_UV.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
par(mfrow = c(1,1), mar = c(4,4,3,1))
matplot(bev$trsnum$wl
        , t(bev$sub$DT0001_Goecklinger_600s$spc)
        , lty = 1, type = "l"
        , xlab = lambda, ylab = "AU", main = "Spektren - Goecklinger Hausbräu, 600 Sekunden, 0,31 mm Pfadlaenge"
        , col = bev$par$DT0001_Goecklinger_600s$colp2
        , xlim = c(190, 300)
        , ylim = c(.5, 3)
)

legend("bottomleft", legend = bev$par$DT0001_Goecklinger_600s$legendt
       , col = bev$par$DT0001_Goecklinger_600s$colp, lty = 1)
dev.off()

png(paste0(date(),"_DT0041_Goecklinger_600s_pca.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")

layout(matrix(c(1,2,3,4,5,5), byrow = T, ncol = 2), heights = c(1, 1, .3))

par(mar = c(4,4,3,1))
PCx = 1
PCy = 2
plot(bev$sub$DT0041_Goecklinger_600s$pca300500$calres$scores[ , PCx], bev$sub$DT0041_Goecklinger_600s$pca300500$calres$scores[ , PCy]
     , xlab = paste("PC", PCx, "Explained Variance =", round( bev$sub$DT0041_Goecklinger_600s$pca300500$calres$expvar[ PCx ] ,1), "%")
     , ylab = paste("PC", PCy, "Explained Variance =", round( bev$sub$DT0041_Goecklinger_600s$pca300500$calres$expvar[ PCy ] ,1), "%")
     , col = bev$par$DT0041_Goecklinger_600s$colp2
     , pch = bev$par$DT0041_Goecklinger_600s$pchp2
     , cex = 2
     , main = "PCA - Goecklinger Hausbräu, 600 Sekunden, 2 mm Pfadlaenge")

boxplot(bev$sub$DT0041_Goecklinger_600s$spc$X450 ~ bev$sub$DT0041_Goecklinger_600s$data$rcf
        , col = bev$par$DT0041_Goecklinger_600s$colp, pch = 20, log = "y"
        , xlab = "Relative Zentrifugalkraft", ylab = "AU bei 450 nm"
        , main = "AU_450nm - Goecklinger Hausbräu 600 Sekunden, 2 mm Pfadlaenge")

boxplot(bev$sub$DT0041_Goecklinger_600s$pca300500$calres$scores[ , 1] ~ bev$sub$DT0041_Goecklinger_600s$data$rcf
        , col = bev$par$DT0041_Goecklinger_600s$colp, pch = 20
        , xlab = "Relative Zentrifugalkraft", ylab = "PC 1"
        , main = "PC1 - Goecklinger Hausbräu 600 Sekunden, 2 mm Pfadlaenge")

boxplot(bev$sub$DT0041_Goecklinger_600s$pca300500$calres$scores[ , 2] ~ bev$sub$DT0041_Goecklinger_600s$data$rcf
        , col = bev$par$DT0041_Goecklinger_600s$colp, pch = 20
        , xlab = "Relative Zentrifugalkraft", ylab = "PC 2"
        , main = "PC2 - Goecklinger Hausbräu 600 Sekunden, 2 mm Pfadlaenge")

par(mar = c(0,0,0,0))
plot(1,1, type = "n", axes = F, xlab = "", ylab = "")
legend("center", legend = bev$par$DT0041_Goecklinger_600s$legendt
       , col = bev$par$DT0041_Goecklinger_600s$colp
       , pch = bev$par$DT0041_Goecklinger_600s$pchp
       , xpd = T, ncol = 5, bty = "n", cex = 1.5, pt.cex = 2, pt.lwd = 2)

dev.off()

bev$sub$DT0041_Goecklinger_600s$pca300500_sub <- pca(bev$sub$DT0041_Goecklinger_600s$spc[ bev$sub$DT0041_Goecklinger_600s$data$rcf > 0 & bev$sub$DT0041_Goecklinger_600s$spc$X450 < .2 , which( bev$sub$DT0041_Goecklinger_600s$wl %in% 300 ) : which( bev$sub$DT0041_Goecklinger_600s$wl %in% 500 )], ncomp = 4)

png(paste0(date(),"_DT0041_Goecklinger_600s_pca_sub.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")

layout(matrix(c(1,2,3,4,5,5), byrow = T, ncol = 2), heights = c(1, 1, .3))

par(mar = c(4,4,3,1))
PCx = 1
PCy = 2
plot(bev$sub$DT0041_Goecklinger_600s$pca300500_sub$calres$scores[ , PCx], bev$sub$DT0041_Goecklinger_600s$pca300500_sub$calres$scores[ , PCy]
     , xlab = paste("PC", PCx, "Explained Variance =", round( bev$sub$DT0041_Goecklinger_600s$pca300500_sub$calres$expvar[ PCx ] ,1), "%")
     , ylab = paste("PC", PCy, "Explained Variance =", round( bev$sub$DT0041_Goecklinger_600s$pca300500_sub$calres$expvar[ PCy ] ,1), "%")
     , col = bev$par$DT0041_Goecklinger_600s$colp2[ bev$sub$DT0041_Goecklinger_600s$data$rcf > 0 & bev$sub$DT0041_Goecklinger_600s$spc$X450 < .2 ]
     , pch = bev$par$DT0041_Goecklinger_600s$pchp2[ bev$sub$DT0041_Goecklinger_600s$data$rcf > 0 & bev$sub$DT0041_Goecklinger_600s$spc$X450 < .2 ]
     , cex = 2
     , main = "PCA - Goecklinger Hausbräu, 600 Sekunden, 2 mm Pfadlaenge"
     # , xlim = c(-.5, .75)
     # , ylim = c(-.5, 1)
)

boxplot(bev$sub$DT0041_Goecklinger_600s$spc$X450[ bev$sub$DT0041_Goecklinger_600s$data$rcf > 0 & bev$sub$DT0041_Goecklinger_600s$spc$X450 < .2 ] ~ bev$sub$DT0041_Goecklinger_600s$data$rcf[ bev$sub$DT0041_Goecklinger_600s$data$rcf > 0 & bev$sub$DT0041_Goecklinger_600s$spc$X450 < .2 ]
        , col = bev$par$DT0041_Goecklinger_600s$colp[ - 1], pch = 20, log = "y"
        , xlab = "Relative Zentrifugalkraft", ylab = "AU bei 450 nm"
        , main = "AU_450nm - Goecklinger Hausbräu 600 Sekunden, 2 mm Pfadlaenge")

boxplot(bev$sub$DT0041_Goecklinger_600s$pca300500_sub$calres$scores[, 1] ~ bev$sub$DT0041_Goecklinger_600s$data$rcf[ bev$sub$DT0041_Goecklinger_600s$data$rcf > 0 & bev$sub$DT0041_Goecklinger_600s$spc$X450 < .2 ]
        , col = bev$par$DT0041_Goecklinger_600s$colp[ - 1], pch = 20
        , xlab = "Relative Zentrifugalkraft", ylab = "PC 1"
        , main = "PC1 - Goecklinger Hausbräu 600 Sekunden, 2 mm Pfadlaenge")

boxplot(bev$sub$DT0041_Goecklinger_600s$pca300500_sub$calres$scores[ , 2] ~ bev$sub$DT0041_Goecklinger_600s$data$rcf[ bev$sub$DT0041_Goecklinger_600s$data$rcf > 0 & bev$sub$DT0041_Goecklinger_600s$spc$X450 < .2 ]
        , col = bev$par$DT0041_Goecklinger_600s$colp[ - 1], pch = 20
        , xlab = "Relative Zentrifugalkraft", ylab = "PC 2"
        , main = "PC2 - Goecklinger Hausbräu 600 Sekunden, 2 mm Pfadlaenge")

par(mar = c(0,0,0,0))
plot(1,1, type = "n", axes = F, xlab = "", ylab = "")
legend("center", legend = bev$par$DT0041_Goecklinger_600s$legendt[ -1 ]
       , col = bev$par$DT0041_Goecklinger_600s$colp[ -1 ]
       , pch = bev$par$DT0041_Goecklinger_600s$pchp[ -1 ]
       , xpd = T, ncol = 5, bty = "n", cex = 1.5, pt.cex = 2, pt.lwd = 2)

dev.off()


# DT0001 Goecklinger 800 rcf ####
bev$sub$DT0001_Goecklinger_800rcf <- bev$merge[ bev$merge$cell == "DT0001" & bev$merge$beer == "Goecklinger" & bev$merge$rcf == "0800" | bev$merge$rcf == "0000"]
bev$sub$DT0001_Goecklinger_800rcf <- bev$sub$DT0001_Goecklinger_800rcf[ bev$sub$DT0001_Goecklinger_800rcf$beer == "Goecklinger" ]
bev$sub$DT0001_Goecklinger_800rcf <- bev$sub$DT0001_Goecklinger_800rcf[order(bev$sub$DT0001_Goecklinger_800rcf$rcf)]
bev$sub$DT0001_Goecklinger_800rcf <- bev$sub$DT0001_Goecklinger_800rcf[ !is.na(bev$sub$DT0001_Goecklinger_800rcf$`500`) , ]
bev$sub$DT0001_Goecklinger_800rcf <- droplevels(bev$sub$DT0001_Goecklinger_800rcf)

bev$par$DT0001_Goecklinger_800rcf$colp <- colfunc(length( levels(bev$sub$DT0001_Goecklinger_800rcf$duration) ))
bev$par$DT0001_Goecklinger_800rcf$colp2 <- bev$par$DT0001_Goecklinger_800rcf$colp[ bev$sub$DT0001_Goecklinger_800rcf$duration ]

bev$par$DT0001_Goecklinger_800rcf$pchp <- c(15:19, 0:2, 5)
bev$par$DT0001_Goecklinger_800rcf$pchp2 <- bev$par$DT0001_Goecklinger_800rcf$pchp[ bev$sub$DT0001_Goecklinger_800rcf$duration ]

bev$par$DT0001_Goecklinger_800rcf$legendt <- as.character(paste0(as.character( as.numeric( levels(bev$sub$DT0001_Goecklinger_800rcf$duration))), " s ~ 800 rcf"))

bev$sub$DT0001_Goecklinger_800rcf <- transfer_csv(bev$sub$DT0001_Goecklinger_800rcf)

png(paste0(date(),"_DT0001_Goecklinger_800rcf_spc.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
par(mfrow = c(1,1), mar = c(4,4,3,1))
matplot(bev$trsnum$wl
        , t(bev$sub$DT0001_Goecklinger_800rcf$spc)
        , lty = 1, type = "l"
        , xlab = lambda, ylab = "AU", main = "Spektren - Goecklinger Hausbräu bei 800 rcf, 0,3 mm Pfadlaenge"
        , col = bev$par$DT0001_Goecklinger_800rcf$colp2)

legend("topright", legend = bev$par$DT0001_Goecklinger_800rcf$legendt
       , col = bev$par$DT0001_Goecklinger_800rcf$colp, lty = 1)
dev.off()


png(paste0(date(),"_DT0001_Goecklinger_800rcf_spc_zoom.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
par(mfrow = c(1,1), mar = c(4,4,3,1))
matplot(bev$trsnum$wl
        , t(bev$sub$DT0001_Goecklinger_800rcf$spc)[   ,  which(unlist(lapply(apply(bev$sub$DT0001_Goecklinger_800rcf$spc, 1, function(x) which(x  > 3.1)), length)) == 0)]
        , lty = 1, type = "l"
        , xlab = lambda, ylab = "AU", main = "Spektren - Goecklinger Hausbräu bei 800 rcf, 0,3 mm Pfadlaenge"
        , col = bev$par$DT0001_Goecklinger_800rcf$colp2
        , xlim = c(190, 300))

legend("bottomleft", legend = bev$par$DT0001_Goecklinger_800rcf$legendt
       , col = bev$par$DT0001_Goecklinger_800rcf$colp, lty = 1)
dev.off()

bev$sub$DT0001_Goecklinger_800rcf$pca200300 <- pca(bev$sub$DT0001_Goecklinger_800rcf$spc[ , which( bev$sub$DT0001_Goecklinger_800rcf$wl %in% 200 ) : which( bev$sub$DT0001_Goecklinger_800rcf$wl %in% 300 )], ncomp = 4)

png(paste0(date(),"_DT0001_Goecklinger_800rcf_pca.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")

layout(matrix(c(1,2,3,4,5,5), byrow = T, ncol = 2), heights = c(1, 1, .3))

par(mar = c(4,4,3,1))
PCx = 1
PCy = 2
plot(bev$sub$DT0001_Goecklinger_800rcf$pca200300$calres$scores[ , PCx], bev$sub$DT0001_Goecklinger_800rcf$pca200300$calres$scores[ , PCy]
     , xlab = paste("PC", PCx, "Explained Variance =", round( bev$sub$DT0001_Goecklinger_800rcf$pca200300$calres$expvar[ PCx ] ,1), "%")
     , ylab = paste("PC", PCy, "Explained Variance =", round( bev$sub$DT0001_Goecklinger_800rcf$pca200300$calres$expvar[ PCy ] ,1), "%")
     , col = bev$par$DT0001_Goecklinger_800rcf$colp2
     , pch = bev$par$DT0001_Goecklinger_800rcf$pchp2
     , cex = 2
     , main = "PCA - Goecklinger Hausbräu bei 800 rcf, 0,3 mm Pfadlaenge")

boxplot(bev$sub$DT0001_Goecklinger_800rcf$spc$X450 ~ bev$sub$DT0001_Goecklinger_800rcf$data$duration
        , col = bev$par$DT0001_Goecklinger_800rcf$colp, pch = 20, log = "y"
        , xlab = "Zentrifugationsdauer in Sekunden", ylab = "AU bei 450 nm"
        , main = "AU_450nm - Goecklinger Hausbräu bei 800 rcf, 0,3 mm Pfadlaenge")

boxplot(bev$sub$DT0001_Goecklinger_800rcf$pca200300$calres$scores[ , 1] ~ bev$sub$DT0001_Goecklinger_800rcf$data$duration
        , col = bev$par$DT0001_Goecklinger_800rcf$colp, pch = 20
        , xlab = "Zentrifugationsdauer in Sekunden", ylab = "PC 1"
        , main = "PC1 - Goecklinger Hausbräu bei 800 rcf, 0,3 mm Pfadlaenge")

boxplot(bev$sub$DT0001_Goecklinger_800rcf$pca200300$calres$scores[ , 2] ~ bev$sub$DT0001_Goecklinger_800rcf$data$duration
        , col = bev$par$DT0001_Goecklinger_800rcf$colp, pch = 20
        , xlab = "Zentrifugationsdauer in Sekunden", ylab = "PC 2"
        , main = "PC2 - Goecklinger Hausbräu bei 800 rcf, 0,3 mm Pfadlaenge")

par(mar = c(0,0,0,0))
plot(1,1, type = "n", axes = F, xlab = "", ylab = "")
legend("center", legend = bev$par$DT0001_Goecklinger_800rcf$legendt
       , col = bev$par$DT0001_Goecklinger_800rcf$colp
       , pch = bev$par$DT0001_Goecklinger_800rcf$pchp
       , xpd = T, ncol = 5, bty = "n", cex = 1.5, pt.cex = 2, pt.lwd = 2)

dev.off()


# DT0001 Goecklinger Dauer 600 ####
bev$sub$DT0001_Goecklinger_600s <- bev$merge[ bev$merge$cell == "DT0001" & bev$merge$beer == "Goecklinger" & bev$merge$duration == "0600" | bev$merge$duration == "0000"]
bev$sub$DT0001_Goecklinger_600s <- bev$sub$DT0001_Goecklinger_600s[ bev$sub$DT0001_Goecklinger_600s$beer == "Goecklinger" ]
bev$sub$DT0001_Goecklinger_600s <- bev$sub$DT0001_Goecklinger_600s[order(bev$sub$DT0001_Goecklinger_600s$rcf)]
bev$sub$DT0001_Goecklinger_600s <- droplevels(bev$sub$DT0001_Goecklinger_600s)

bev$par$DT0001_Goecklinger_600s$colp <- colfunc(length( levels(bev$sub$DT0001_Goecklinger_600s$rcf) ))
bev$par$DT0001_Goecklinger_600s$colp2 <- bev$par$DT0001_Goecklinger_600s$colp[ bev$sub$DT0001_Goecklinger_600s$rcf ]
bev$par$DT0001_Goecklinger_600s$legendt <- as.character(paste0(as.character( as.numeric(levels(bev$sub$DT0001_Goecklinger_600s$rcf)) ), " rcf ~ 600 s"))

bev$par$DT0001_Goecklinger_600s$pchp <- c(15:19, 0:2, 5)
bev$par$DT0001_Goecklinger_600s$pchp2 <- bev$par$DT0001_Goecklinger_600s$pchp[ bev$sub$DT0001_Goecklinger_600s$rcf ]

bev$sub$DT0001_Goecklinger_600s <- transfer_csv(bev$sub$DT0001_Goecklinger_600s)

bev$sub$DT0001_Goecklinger_600s$pca200300 <- pca(bev$sub$DT0001_Goecklinger_600s$spc[ , which( bev$sub$DT0001_Goecklinger_600s$wl %in% 200 ) : which( bev$sub$DT0001_Goecklinger_600s$wl %in% 300 )], ncomp = 4)

png(paste0(date(),"_DT0001_Goecklinger_600s_spc.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
par(mfrow = c(1,1), mar = c(4,4,3,1))
matplot(bev$trsnum$wl
        , t(bev$sub$DT0001_Goecklinger_600s$spc)
        , lty = 1, type = "l"
        , xlab = lambda, ylab = "AU", main = "Spektren - Goecklinger Hausbräu, 600 Sekunden, 0,31 Pfadlaenge"
        , col = bev$par$DT0001_Goecklinger_600s$colp2)

legend("topright", legend = bev$par$DT0001_Goecklinger_600s$legendt
       , col = bev$par$DT0001_Goecklinger_600s$colp, lty = 1)
dev.off()

png(paste0(date(),"_DT0001_Goecklinger_600s_pca.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")

layout(matrix(c(1,2,3,4,5,5), byrow = T, ncol = 2), heights = c(1, 1, .3))

par(mar = c(4,4,3,1))
PCx = 1
PCy = 2
plot(bev$sub$DT0001_Goecklinger_600s$pca200300$calres$scores[ , PCx], bev$sub$DT0001_Goecklinger_600s$pca200300$calres$scores[ , PCy]
     , xlab = paste("PC", PCx, "Explained Variance =", round( bev$sub$DT0001_Goecklinger_600s$pca200300$calres$expvar[ PCx ] ,1), "%")
     , ylab = paste("PC", PCy, "Explained Variance =", round( bev$sub$DT0001_Goecklinger_600s$pca200300$calres$expvar[ PCy ] ,1), "%")
     , col = bev$par$DT0001_Goecklinger_600s$colp2
     , pch = bev$par$DT0001_Goecklinger_600s$pchp2
     , cex = 2
     , main = "PCA - Goecklinger Hausbräu, 600 Sekunden, 2 mm Pfadlaenge")

boxplot(bev$sub$DT0001_Goecklinger_600s$spc$X450 ~ bev$sub$DT0001_Goecklinger_600s$data$rcf
        , col = bev$par$DT0001_Goecklinger_600s$colp, pch = 20, log = "y"
        , xlab = "Relative Zentrifugalkraft", ylab = "AU bei 450 nm"
        , main = "AU_450nm - Goecklinger Hausbräu 600 Sekunden, 0,31 mm Pfadlaenge")

boxplot(bev$sub$DT0001_Goecklinger_600s$pca200300$calres$scores[ , 1] ~ bev$sub$DT0001_Goecklinger_600s$data$rcf
        , col = bev$par$DT0001_Goecklinger_600s$colp, pch = 20
        , xlab = "Relative Zentrifugalkraft", ylab = "PC 1"
        , main = "PC1 - Goecklinger Hausbräu 600 Sekunden, 0,31 mm Pfadlaenge")

boxplot(bev$sub$DT0001_Goecklinger_600s$pca200300$calres$scores[ , 2] ~ bev$sub$DT0001_Goecklinger_600s$data$rcf
        , col = bev$par$DT0001_Goecklinger_600s$colp, pch = 20
        , xlab = "Relative Zentrifugalkraft", ylab = "PC 2"
        , main = "PC2 - Goecklinger Hausbräu 600 Sekunden, 0,31 mm Pfadlaenge")

par(mar = c(0,0,0,0))
plot(1,1, type = "n", axes = F, xlab = "", ylab = "")
legend("center", legend = bev$par$DT0001_Goecklinger_600s$legendt
       , col = bev$par$DT0001_Goecklinger_600s$colp
       , pch = bev$par$DT0001_Goecklinger_600s$pchp
       , xpd = T, ncol = 5, bty = "n", cex = 1.5, pt.cex = 2, pt.lwd = 2)

dev.off()

# DT0041 Alle Biersorten Vergleich ####
bev$sub$DT0041_000rcf_800rcf_240s <- bev$merge[ bev$merge$cell == "DT0041" & bev$merge$duration == "0240" | bev$merge$duration == "0000"]
bev$sub$DT0041_000rcf_800rcf_240s <- bev$sub$DT0041_000rcf_800rcf_240s[bev$sub$DT0041_000rcf_800rcf_240s$cell == "DT0041"]
bev$sub$DT0041_000rcf_800rcf_240s <- bev$sub$DT0041_000rcf_800rcf_240s[bev$sub$DT0041_000rcf_800rcf_240s$beer != "Hefe1zu10"]
bev$sub$DT0041_000rcf_800rcf_240s <- bev$sub$DT0041_000rcf_800rcf_240s[order(bev$sub$DT0041_000rcf_800rcf_240s$beer, bev$sub$DT0041_000rcf_800rcf_240s$rcf)]
bev$sub$DT0041_000rcf_800rcf_240s <- droplevels(bev$sub$DT0041_000rcf_800rcf_240s)

bev$par$DT0041_000rcf_800rcf_240s$colp <- colfunc(length( levels(interaction(bev$sub$DT0041_000rcf_800rcf_240s$rcf, bev$sub$DT0041_000rcf_800rcf_240s$beer, drop = T))))
bev$par$DT0041_000rcf_800rcf_240s$colp2 <- bev$par$DT0041_000rcf_800rcf_240s$colp[ interaction(bev$sub$DT0041_000rcf_800rcf_240s$rcf, bev$sub$DT0041_000rcf_800rcf_240s$beer, drop = T) ]

bev$par$DT0041_000rcf_800rcf_240s$lty <- c(3,1)
bev$par$DT0041_000rcf_800rcf_240s$lty2 <- bev$par$DT0041_000rcf_800rcf_240s$lty[ bev$sub$DT0041_000rcf_800rcf_240s$rcf ]

bev$par$DT0041_000rcf_800rcf_240s$pch <- c(1,17)
bev$par$DT0041_000rcf_800rcf_240s$pch2 <- bev$par$DT0041_000rcf_800rcf_240s$pch[ bev$sub$DT0041_000rcf_800rcf_240s$rcf ]

bev$par$DT0041_000rcf_800rcf_240s$legendt <- paste( rep(as.character(paste0(as.character( levels(bev$sub$DT0041_000rcf_800rcf_240s$beer) ))), each = 2), c("pur", "zentrifugiert"))

bev$sub$DT0041_000rcf_800rcf_240s <- transfer_csv(bev$sub$DT0041_000rcf_800rcf_240s)

bev$sub$DT0041_000rcf_800rcf_240s$data

png(paste0(date(),"_DT0041_Biersorten_Vergleich_800rcfs_spc.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
par(mfrow = c(2,2), mar = c(4,4,3,1))
for(i in 1:4){
  xx <- bev$sub$DT0041_000rcf_800rcf_240s$data$beer == levels(bev$sub$DT0041_000rcf_800rcf_240s$data$beer)[ i ]
  matplot(bev$trsnum$wl
          , t(bev$sub$DT0041_000rcf_800rcf_240s$spc)[ , xx ]
          , lty = bev$par$DT0041_000rcf_800rcf_240s$lty2 [ xx ], type = "l"
          , xlab = lambda, ylab = "AU", main = paste(levels(bev$sub$DT0041_000rcf_800rcf_240s$data$beer)[ i ], " bei 800 rcf und 240 Sekunden")
          , col = bev$par$DT0041_000rcf_800rcf_240s$colp2[ xx ]
          , lwd = 1
          , ylim = c(0, 5))

  legend("topright"
         , paste(c(0, 800), "rcf ~ 240 s")
         , lty = bev$par$DT0041_000rcf_800rcf_240s$lty
         , lwd = 1
         , col = bev$par$DT0041_000rcf_800rcf_240s$colp[ (i * 2 - 1) : (i*2)])
}
dev.off()





