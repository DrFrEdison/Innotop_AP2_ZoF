png(paste0(date(),"_DT0041_Bierprobe_800rcfs_spc.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
par(mfrow = c(1,1), mar = c(4,5,3,1)
    , cex.axis = 1.5
    , cex.lab = 1.5)
for(i in 4){
  xx <- bev$sub$DT0041_000rcf_800rcf_240s$data$beer == levels(bev$sub$DT0041_000rcf_800rcf_240s$data$beer)[ i ]
  matplot(bev$trsnum$wl
          , t(bev$sub$DT0041_000rcf_800rcf_240s$spc)[ , xx ]
          , lty = bev$par$DT0041_000rcf_800rcf_240s$lty2 [ xx ], type = "l"
          , xlab = lambda, ylab = "AU", main = paste("Bierprobe, zentrifugiert bei 800 rcf für 240 Sekunden")
          , col = bev$par$DT0041_000rcf_800rcf_240s$colp2[ xx ]
          , lwd = 2
          , ylim = c(0, 5))

  legend("topright"
         , paste(c(0, 800), "rcf ~ 240 s")
         , lty = bev$par$DT0041_000rcf_800rcf_240s$lty
         , lwd = 1.5
         , cex = 1.5
         , col = bev$par$DT0041_000rcf_800rcf_240s$colp[ (i * 2 - 1) : (i*2)])
}
dev.off()
