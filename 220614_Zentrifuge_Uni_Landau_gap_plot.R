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



boxplot(bev$sub$Turbidity_800rcf_240s$Turbidity ~ bev$sub$Turbidity_800rcf_240s$rcf + bev$sub$Turbidity_800rcf_240s$beer
        , outline = F
        , xlab = "", ylab = "Trübung in EBC", main = "Trübung ~ Biertyp bei 800 rcf ~ 240 s", ylim = c(0, 40)
        , col = rep(colfunc(length( levels( bev$sub$Turbidity_800rcf_240s$beer ))), each = 2))

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

png(paste0(date(),"_Turbidity_merge.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
layout(matrix(c(4,3,2,1), byrow = T, ncol = 1), heights = c( 35/apply(bev$barplot$xgab, 2, diff) ))
par(mar = c(4,4,0,1))
plot( 1, 1, type="n", xlab="", ylab="", axes=F
     , xlim = c(.5,length(bev$barplot$y)+.5)
     , ylim = c(bev$barplot$xgab[1,1],bev$barplot$xgab[2,1]))
rect(xleft = 1:length(bev$barplot$y) - bev$barplot$xx
     , ybottom = 0
     , xright = 1:length(bev$barplot$y) + bev$barplot$xx
     , ytop = bev$barplot$y)
segments(x0 = 1:length(bev$barplot$y)
         , y0 = bev$barplot$y
         , x1 = 1:length(bev$barplot$y)
         , y1 = bev$barplot$y + bev$barplot$sd)
segments(par("usr")[1],0,par("usr")[1],1000, xpd = T)
axis(2, las = 2)
rect(-1,bev$barplot$xgab[2,1],100,bev$barplot$xgab[2,1]+3, xpd = F, border = "grey", col = "white")

par(mar = c(0,4,0,1))
plot( 1, 1, type="n", xlab="", ylab="", axes=F
      , xlim = c(.5,length(bev$barplot$y)+.5)
      , ylim = c(bev$barplot$xgab[1,2],bev$barplot$xgab[2,2]))
rect(xleft = 1:length(bev$barplot$y) - bev$barplot$xx
     , ybottom = 0
     , xright = 1:length(bev$barplot$y) + bev$barplot$xx
     , ytop = bev$barplot$y)
segments(x0 = 1:length(bev$barplot$y)
         , y0 = bev$barplot$y
         , x1 = 1:length(bev$barplot$y)
         , y1 = bev$barplot$y + bev$barplot$sd)
segments(par("usr")[1],-1000,par("usr")[1],1000, xpd = T)
axis(2, las = 2)
rect(-1,bev$barplot$xgab[1,2],100,bev$barplot$xgab[1,2]-3, xpd = F, border = "grey", col = "white")
rect(-1,bev$barplot$xgab[2,2],100,bev$barplot$xgab[2,2]+3, xpd = F, border = "grey", col = "white")

par(mar = c(0,4,0,1))
plot( 1, 1, type="n", xlab="", ylab="", axes=F
      , xlim = c(.5,length(bev$barplot$y)+.5)
      , ylim = c(bev$barplot$xgab[1,3],bev$barplot$xgab[2,3]))
rect(xleft = 1:length(bev$barplot$y) - bev$barplot$xx
     , ybottom = 0
     , xright = 1:length(bev$barplot$y) + bev$barplot$xx
     , ytop = bev$barplot$y)
segments(x0 = 1:length(bev$barplot$y)
         , y0 = bev$barplot$y
         , x1 = 1:length(bev$barplot$y)
         , y1 = bev$barplot$y + bev$barplot$sd)
segments(par("usr")[1],-1000,par("usr")[1],1000, xpd = T)
axis(2, las = 2)
rect(-1,bev$barplot$xgab[1,3],100,bev$barplot$xgab[1,3]-3, xpd = F, border = "grey", col = "white")
rect(-1,bev$barplot$xgab[2,3],100,bev$barplot$xgab[2,3]+3, xpd = F, border = "grey", col = "white")

par(mar = c(0,4,3,1))
plot( 1, 1, type="n", xlab="", ylab="", axes=F
      , xlim = c(.5,length(bev$barplot$y)+.5)
      , ylim = c(bev$barplot$xgab[1,4],bev$barplot$xgab[2,4]))
rect(xleft = 1:length(bev$barplot$y) - bev$barplot$xx
     , ybottom = 0
     , xright = 1:length(bev$barplot$y) + bev$barplot$xx
     , ytop = bev$barplot$y)
segments(x0 = 1:length(bev$barplot$y)
         , y0 = bev$barplot$y
         , x1 = 1:length(bev$barplot$y)
         , y1 = bev$barplot$y + bev$barplot$sd)
segments(par("usr")[1],-1000,par("usr")[1],1000, xpd = F)
axis(2, las = 2)
rect(-1,bev$barplot$xgab[1,4],100,bev$barplot$xgab[1,4]-3, xpd = F, border = "grey", col = "white")
dev.off()
