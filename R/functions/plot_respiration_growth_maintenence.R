#--------------------------------------------------------------------------------------------------
#- growth vs. maintenance respiration, Amthor (2000) and McCree (1970)
#- read Adu-Bredu 2003 Ecological Research.
#- r/w = g x (1/w x dW/dT) +m ; r/w is the mean specific respiraiton rate.
# (1/w x dW/dT) is RGR
# g and m are growth and maintenence respiration rates
plot_respiration_growth_maintenence <- function(growth){
  gmr <- growth
  
  gmr$RtoW <- with(gmr,Ra/(totMass*0.47))
  gmr$logy <- log10(gmr$RtoW)
  gmr$logx <- log10(gmr$RGR/1000)
  
  model1 <- lme(RtoW~T_treatment+RGR+RGR:T_treatment,random=~1|chamber,data=gmr) # slopes not different
  model2 <- lme(RtoW~T_treatment,random=~1|chamber,data=gmr)                # intercepts are not different
  model3 <- lme(RtoW~RGR,data=gmr,random=~1|chamber)
  model4 <- lm(RtoW~RGR,data=gmr)
  
  int <- unname(round(coef(model4)[1],3))
  slope <- unname(round(coef(model4)[2],4))
  r2 <- round(summary(model4)$r.squared,2)
  
  
  
  
  #- add predictions for ambient and warmed treatments
  # Set up a regular sequence of numbers, for which respiration is to be predicted
  xval <- seq(0, max(gmr$RGR), length = 101)
  
  # Two separate dataframes, one for each treatment/
  amb_dfr <- data.frame(RGR = xval, T_treatment = "ambient")
  ele_dfr <- data.frame(RGR = xval, T_treatment = "elevated")
  
  # Predictions of the model using 'predict.lm'
  predamb <- as.data.frame(predict(model4, amb_dfr, interval = "confidence"))
  predele <- as.data.frame(predict(model4, ele_dfr, interval = "confidence"))
  
  # Plot. Set up the axis limits so that they start at 0, and go to the
  # maximum.
  
  #- on normal scale
  windows(20,20)
  par(mar=c(6,6,1,1),cex.lab=1.8,cex.axis=1.2,las=1)
  plotBy(RtoW~RGR|T_treatment,data=subset(gmr,Water_treatment=="control"),ylim=c(0,0.2),xlim=c(0,20),pch=16,
         legend=F,cex=1.2,
         ylab=expression(Respiration~per~unit~tree~C~(gC~gC^-1)),
         xlab=expression(Relative~growth~rate~(mgC~gC^-1~d^-1)))
  plotBy(RtoW~RGR|T_treatment,data=subset(gmr,Water_treatment=="drydown"),ylim=c(0,0.2),xlim=c(0,20),pch=1,
         legend=F,add=T,cex=1.2)
  legend("topleft",pch=c(16,16,1,1),col=palette()[1:2],
         legend=c("A-Wet","W-Wet","A-Dry","W-Dry"),bty="n",cex=1.2)
  
  # Add the lines; the fit and lower and upper confidence intervals.
  lines(xval, predamb$fit, col ="black", lwd = 2)
  lines(xval, predamb$lwr, col = "black", lwd = 1, lty = 3)
  lines(xval, predamb$upr, col = "black", lwd = 1, lty = 3)
  
  #lines(xval, predele$fit, col = palette()[2], lwd = 2)
  #lines(xval, predele$lwr, col = palette()[2], lwd = 1, lty = 3)
  #lines(xval, predele$upr, col = palette()[2], lwd = 1, lty = 3)
  
  legend("bottomright",legend=paste("y =",int,"+",slope,"x,","r2 =",r2,sep=" "),bty="n")
  
  dev.copy2pdf(file="output/R_growth_maintenance.pdf")
}
