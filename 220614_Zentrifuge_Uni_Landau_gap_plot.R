# read Budweiser/Jupiler data ####
bev$Budweiser <- read.xlsx("D:/OneDrive - Dausch Technologies GmbH/FE_Hardware/P191380_ABInBev_BG/03_Projektdokumentation/06_DDM/GC-IMS/Validierung/03_ABInBev_GC-IMS_LD_Prozess/210705_Validation_GC-IMS_final_comparison.xlsx"
                           , sheet = "raw")
bev$Budweiser <- bev$Budweiser[, -c(grep("NTU", bev$Budweiser[1,]), grep("Daily", names(bev$Budweiser)))]
bev$Budweiser <-  bev$Budweiser[ - c(1:3) , ]
head(bev$Budweiser)
head10(bev$Budweiser)

# manipulate data #
bev$Budweiser$Diacetyl <- as.numeric(bev$Budweiser$Diacetyl)
bev$Budweiser$Pentanedione <- as.numeric(bev$Budweiser$Pentanedione)
bev$Budweiser$Diacetyl_ABI <- as.numeric(bev$Budweiser$Diacetyl_ABI)
bev$Budweiser$Pentanedione_ABI <- as.numeric(bev$Budweiser$Pentanedione_ABI)
bev$Budweiser$Turbidity.filtered <- as.numeric(bev$Budweiser$Turbidity.filtered)
bev$Budweiser$Turbidity.raw <- as.numeric(bev$Budweiser$Turbidity.raw)
bev$Budweiser$Turbidity.filtered_ABI <- as.numeric(bev$Budweiser$Turbidity.filtered_ABI)

bev$Budweiser$Date <- as.POSIXct(as.character(bev$Budweiser$Date), format = "%y%m%d")
bev$Budweiser$Days <- as.numeric((bev$Budweiser$Date - bev$Budweiser$Date[1]) / (60*60*24))
bev$Budweiser <- bev$Budweiser[ , moveme(names(bev$Budweiser), "ID Date Days first")]

# Turbidity_800rcf_240s ####
names(bev$EBC)[4] <- "mean"
names(bev$EBC)[5] <- "sd"

bev$sub$Turbidity_800rcf_240s <- bev$EBC[ bev$EBC$rcf == "0800" | bev$EBC$rcf == "0000" , ]
bev$sub$Turbidity_800rcf_240s <- bev$sub$Turbidity_800rcf_240s[ bev$sub$Turbidity_800rcf_240s$duration   == "0240" | bev$sub$Turbidity_800rcf_240s$duration == "0000" , ]

bev$sub$Turbidity_800rcf_240s <- bev$sub$Turbidity_800rcf_240s[ order(bev$sub$Turbidity_800rcf_240s$beer) , ]
bev$sub$Turbidity_800rcf_240s <- bev$sub$Turbidity_800rcf_240s[bev$sub$Turbidity_800rcf_240s$mean < 10000 , ]

bev$sub$Turbidity_800rcf_240s <- droplevels(bev$sub$Turbidity_800rcf_240s)

bev$sub$Turbidity_800rcf_240s$beer[ which(bev$sub$Turbidity_800rcf_240s$beer == "Hefe1zu10") ] <- "Hefe"
bev$sub$Turbidity_800rcf_240s$beer <- factor(bev$sub$Turbidity_800rcf_240s$beer)

bev$sub$Turbidity_800rcf_240s[3:4,] <- bev$sub$Turbidity_800rcf_240s[4:3,]

bev$barplot$y <- c(bev$sub$Turbidity_800rcf_240s$mean
                   , tapply(bev$Budweiser$Turbidity.raw, bev$Budweiser$Beer, mean)
                   , tapply(bev$Budweiser$Turbidity.filtered_ABI, bev$Budweiser$Beer, function(x) mean(x, na.rm = T))
                   , tapply(bev$Budweiser$Turbidity.filtered, bev$Budweiser$Beer, mean))
bev$barplot$sd <- c(bev$sub$Turbidity_800rcf_240s$sd
                    , tapply(bev$Budweiser$Turbidity.raw, bev$Budweiser$Beer, sd)
                    , tapply(bev$Budweiser$Turbidity.filtered_ABI, bev$Budweiser$Beer, function(x) sd(x, na.rm = T))
                    , tapply(bev$Budweiser$Turbidity.filtered, bev$Budweiser$Beer, sd))
bev$barplot$y <- bev$barplot$y[ c(1:8, 9, 11, 13, 10, 12, 14) ]
bev$barplot$sd <- bev$barplot$sd[ c(1:8, 9, 11, 13, 10, 12, 14) ]



bev$barplot$xx <- .3
require(plotrix)

sort(bev$barplot$y + bev$barplot$sd)
bev$barplot$xgab <- matrix(c(0,40,55,75,130,170,220,250), nrow = 2)

png(paste0(date(),"_Truebung_Zentrifuge_Filter.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")

bev$barplot$heights <- print(rev(c(.2, .2, apply(bev$barplot$xgab, 2, diff) / sum(c( apply(bev$barplot$xgab, 2, diff) )), .1)))
bev$barplot$density <- c( rep(c(0,1000), 4), rep(c(0,25, 60),2) )
bev$barplot$colp <- c(rep(colfunc(length( levels( bev$sub$Turbidity_800rcf_240s$beer ))), each = 2)
                      , rep("pink", 3)
                      , rep("brown", 3))

layout(matrix(rev(c(2,3,4,5,6,7,1,1,1,1,1,1)), byrow = F, ncol = 2), heights = bev$barplot$heights, widths = c(.05, .9))

par(mar = c(0,0,0,0))
plot(1,1,type = "n", xlab = "", ylab = "", axes = F
     , ylim = c(0,1)
     , xlim = c(.5,length(bev$barplot$y)+.5))
text(mean(c(.5,length(bev$barplot$y)+.5))
     , .5, "Trübung in EBC", srt = 90, cex = 2, xpd = T)


par(mar = c(0,4,0,1))
plot(1,1,type = "n", xlab = "", ylab = "", axes = F
     , ylim = c(0,1)
     , xlim = c(.5,length(bev$barplot$y)+.5))

bev$barplot$xlab <- c("Goecklinger", "Hefe", "Paulaner", "Zischke"
                      , "Budweiser"
                      , "Jupiler")

text(c(1.5, 3.5, 5.5, 7.5, 10, 13) - .35 + .5
     , .75
     , bev$barplot$xlab
     , srt = 45
     , xpd = T
     , adj = 1
     , xpd = T)

segments( x <- c(0,2,4,6,8,11,14) + .5
          , par("usr")[4]
          , x
          , .75
          , xpd = T)

par(mar = c(0,4,0,1))
plot( 1, 1, type="n", xlab="", ylab="", axes=F
      , xlim = c(.5,length(bev$barplot$y)+.5)
      , ylim = c(bev$barplot$xgab[1,1],bev$barplot$xgab[2,1]))
rect(xleft = 1:length(bev$barplot$y) - bev$barplot$xx
     , ybottom = 0
     , xright = 1:length(bev$barplot$y) + bev$barplot$xx
     , ytop = bev$barplot$y
     , density = bev$barplot$density
     , col = bev$barplot$colp)
segments(x0 = 1:length(bev$barplot$y)
         , y0 = bev$barplot$y
         , x1 = 1:length(bev$barplot$y)
         , y1 = bev$barplot$y + bev$barplot$sd
         , col = bev$barplot$colp)

segments(par("usr")[1], 0, par("usr")[2], 0, xpd = T)
segments( x
          , par("usr")[3]
          , x
          , 0
          , xpd = T)


segments(par("usr")[1],0,par("usr")[1],1000, xpd = T)
rect(-1,bev$barplot$xgab[2,1],100,bev$barplot$xgab[2,1]+3, xpd = F, border = "grey", col = "white")
axis(2, las = 2, at = c(0,10,20,30))

segments(par("usr")[1], 0, par("usr")[1], 1000, xpd = T)

par(mar = c(0,4,0,1))
plot( 1, 1, type="n", xlab="", ylab="", axes=F
      , xlim = c(.5,length(bev$barplot$y)+.5)
      , ylim = c(bev$barplot$xgab[1,2],bev$barplot$xgab[2,2]))
rect(xleft = 1:length(bev$barplot$y) - bev$barplot$xx
     , ybottom = 0
     , xright = 1:length(bev$barplot$y) + bev$barplot$xx
     , ytop = bev$barplot$y
     , density = bev$barplot$density
     , col = bev$barplot$colp)
segments(x0 = 1:length(bev$barplot$y)
         , y0 = bev$barplot$y
         , x1 = 1:length(bev$barplot$y)
         , y1 = bev$barplot$y + bev$barplot$sd
         , col = bev$barplot$colp)
segments(par("usr")[1],-1000,par("usr")[1],1000, xpd = T)
rect(-1,bev$barplot$xgab[1,2],100,bev$barplot$xgab[1,2]-3, xpd = F, border = "grey", col = "white")
rect(-1,bev$barplot$xgab[2,2],100,bev$barplot$xgab[2,2]+3, xpd = F, border = "grey", col = "white")
axis(2, las = 2, at = c(60,70))

segments(par("usr")[1], 0, par("usr")[1], 1000, xpd = T)

par(mar = c(0,4,0,1))
plot( 1, 1, type="n", xlab="", ylab="", axes=F
      , xlim = c(.5,length(bev$barplot$y)+.5)
      , ylim = c(bev$barplot$xgab[1,3],bev$barplot$xgab[2,3]))
rect(xleft = 1:length(bev$barplot$y) - bev$barplot$xx
     , ybottom = 0
     , xright = 1:length(bev$barplot$y) + bev$barplot$xx
     , ytop = bev$barplot$y
     , density = bev$barplot$density
     , col = bev$barplot$colp)
segments(x0 = 1:length(bev$barplot$y)
         , y0 = bev$barplot$y
         , x1 = 1:length(bev$barplot$y)
         , y1 = bev$barplot$y + bev$barplot$sd
         , col = bev$barplot$colp)
segments(par("usr")[1],-1000,par("usr")[1],1000, xpd = T)
rect(-1,bev$barplot$xgab[1,3],100,bev$barplot$xgab[1,3]-3, xpd = F, border = "grey", col = "white")
rect(-1,bev$barplot$xgab[2,3],100,bev$barplot$xgab[2,3]+3, xpd = F, border = "grey", col = "white")
axis(2, las = 2, at = c(140, 150, 160))
segments(par("usr")[1], 0, par("usr")[1], 1000, xpd = T)

par(mar = c(0,4,0,1))
plot( 1, 1, type="n", xlab="", ylab="", axes=F
      , xlim = c(.5,length(bev$barplot$y)+.5)
      , ylim = c(bev$barplot$xgab[1,4],bev$barplot$xgab[2,4]))
rect(xleft = 1:length(bev$barplot$y) - bev$barplot$xx
     , ybottom = 0
     , xright = 1:length(bev$barplot$y) + bev$barplot$xx
     , ytop = bev$barplot$y
     , density = bev$barplot$density
     , col = bev$barplot$colp)
segments(x0 = 1:length(bev$barplot$y)
         , y0 = bev$barplot$y
         , x1 = 1:length(bev$barplot$y)
         , y1 = bev$barplot$y + bev$barplot$sd
         , col = bev$barplot$colp)
segments(par("usr")[1],-1000,par("usr")[1],1000, xpd = F)
rect(-1,bev$barplot$xgab[1,4],100,bev$barplot$xgab[1,4]-3, xpd = F, border = "grey", col = "white")
axis(2, las = 2, at = c(230, 240, 250))
segments(par("usr")[1], 0, par("usr")[1], 1000, xpd = T)

legend("topright"
       , c("Unfiltriert", "800 rcf ~ 240 s ", "Papierfilter von ABInBev", "Prototyp Bypass-Filter")
       , density = c(0,1000,25,60), cex = 1.5, bty = "n")

par(mar = c(0,4,0,1))
plot(1,1,type = "n", xlab = "", ylab = "", axes = F
     , ylim = c(0,1)
     , xlim = c(.5,length(bev$barplot$y)+.5))
dev.off()

bev$barplot$y_rel <- c(100, NA, 100, NA, 100, NA, 100, NA, 100, NA, NA, 100, NA, NA)
bev$barplot$y_rel[ 2 ] <- bev$barplot$y[ 2 ] / bev$barplot$y[ 1 ] * 100
bev$barplot$y_rel[ 4 ] <- bev$barplot$y[ 4 ] / bev$barplot$y[ 3 ] * 100
bev$barplot$y_rel[ 6 ] <- bev$barplot$y[ 6 ] / bev$barplot$y[ 5 ] * 100
bev$barplot$y_rel[ 8 ] <- bev$barplot$y[ 8 ] / bev$barplot$y[ 7 ] * 100
bev$barplot$y_rel[ 10 ] <- bev$barplot$y[ 10 ] / bev$barplot$y[ 9 ] * 100
bev$barplot$y_rel[ 11 ] <- bev$barplot$y[ 11 ] / bev$barplot$y[ 9 ] * 100
bev$barplot$y_rel[ 13 ] <- bev$barplot$y[ 13 ] / bev$barplot$y[ 12 ] * 100
bev$barplot$y_rel[ 14 ] <- bev$barplot$y[ 14 ] / bev$barplot$y[ 12 ] * 100

bev$barplot$sd_rel <- bev$barplot$sd / bev$barplot$y * 100
bev$barplot$sd_rel <-  bev$barplot$sd_rel[ which( bev$barplot$y_rel != 100) ]

bev$barplot$y_rel <- bev$barplot$y_rel[ which( bev$barplot$y_rel != 100) ]

png(paste0(date(),"_Resttruebung_Zentrifuge_Filter_relativ.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")

bev$barplot$heights <- print(rev(c(.2, .2, apply(bev$barplot$xgab, 2, diff) / sum(c( apply(bev$barplot$xgab, 2, diff) )), .1)))
bev$barplot$density <- c( rep(c(1000), 4), rep(c(25, 60),2) )
bev$barplot$colp <- c(rep(colfunc(length( levels( bev$sub$Turbidity_800rcf_240s$beer ))), each = 1)
                      , rep("pink", 2)
                      , rep("brown", 2))


bev$barplot$xlab <- c("Goecklinger", "Hefe", "Paulaner", "Zischke"
                      , "Budweiser", "Budweiser"
                      , "Jupiler", "Jupiler")


par(mar = c(5,4,1,1))
plot( 1, 1, type="n", xlab="", ylab="Relative Resttrübung in %", axes=F
      , xlim = c(.5,length(bev$barplot$y_rel)+.5)
      , ylim = c(0, 80))
rect(xleft = 1:length(bev$barplot$y_rel) - bev$barplot$xx
     , ybottom = 0
     , xright = 1:length(bev$barplot$y_rel) + bev$barplot$xx
     , ytop = bev$barplot$y_rel
     , density = bev$barplot$density
     , col = bev$barplot$colp)
segments(x0 = 1:length(bev$barplot$y_rel)
         , y0 = bev$barplot$y_rel
         , x1 = 1:length(bev$barplot$y_rel)
         , y1 = bev$barplot$y_rel + bev$barplot$sd_rel
         , col = bev$barplot$colp)

axis(2, las = 2)
text(1:8# + .5
     , par("usr")[3] - diff(par("usr")[3:4] * 0.05)
     , bev$barplot$xlab
     , xpd = T
     , srt = 45
     , adj = 1)
segments(1:8, par("usr")[3], 1:8, par("usr")[3] - diff(par("usr")[3:4] * 0.025), xpd = T)
segments(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[3], xpd = T)
box()

legend("topright"
       , c("800 rcf ~ 240 s ", "Papierfilter von ABInBev", "Prototyp Bypass-Filter")
       , density = c(1000,25,60), cex = 1.5, bty = "n")

dev.off()

