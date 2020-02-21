#--------------------------------------------------------------------------------------------------
#- growth vs. maintenance respiration, Amthor (2000) and McCree (1970)
#- read Adu-Bredu 2003 Ecological Research.
#- r/w = g x (1/w x dW/dT) +m ; r/w is the mean specific respiraiton rate.
# (1/w x dW/dT) is RGR
# g and m are growth and maintenence respiration rates
plot_respiration_growth_maintenence <- function(growth){
  gmr <- growth
  
  gmr$RtoW <- with(gmr,Ra/14/(totMass*0.47))
  gmr$RtoG <- with(gmr,Ra/14/(dMass_c))
  
  gmr$logy <- log10(gmr$RtoW)
  gmr$logx <- log10(gmr$RGR/1000) # convert to g from mg
  
  model1 <- lme(RtoW~T_treatment+RGR+RGR:T_treatment,random=~1|chamber,data=gmr) # slopes not different
  model2 <- lme(RtoW~T_treatment,random=~1|chamber,data=gmr)                     # intercepts are not different
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
  dfr_both <- data.frame(RGR = xval)
  
  model_amb <- lm(RtoW~RGR,data=subset(gmr,T_treatment=="ambient"))
  model_ele <- lm(RtoW~RGR,data=subset(gmr,T_treatment=="elevated"))
  
  # Predictions of the model using 'predict.lm'
  predamb <- as.data.frame(predict(model_amb, amb_dfr, interval = "confidence"))
  predele <- as.data.frame(predict(model_ele, ele_dfr, interval = "confidence"))
  
  predboth <- as.data.frame(predict(model4, dfr_both, interval = "confidence"))
  
  
  # Plot. Set up the axis limits so that they start at 0, and go to the
  # maximum.
  
  
  #--- Resp vs. R G R
  #- on normal scale
  windows(20,20)
  par(mar=c(6,8,1,1),cex.lab=1.8,cex.axis=1.2,las=1)
  plotBy(RtoW~RGR|T_treatment,data=subset(gmr,Water_treatment=="control"),ylim=c(0,0.015),xlim=c(0,17),pch=16,
         legend=F,cex=1.2,
         ylab="",
         xlab=expression(Relative~growth~rate~(mgC~gC^-1~d^-1)))
  title(ylab=expression(Respiration~per~unit~tree~C~(gC~gC^-1~d^-1)),line=4)
  
  plotBy(RtoW~RGR|T_treatment,data=subset(gmr,Water_treatment=="drydown"),ylim=c(0,0.2),xlim=c(0,20),pch=1,
         legend=F,add=T,cex=1.2)
  legend("topleft",pch=c(16,16,1,1),col=palette()[1:2],
         legend=c("A-Con","W-Con","A-Dry","W-Dry"),bty="n",cex=1.2)
  
  # Add the lines; the fit and lower and upper confidence intervals.
  #lines(xval, predamb$fit, col = palette()[1], lwd = 2)
  #lines(xval, predamb$lwr, col = palette()[1], lwd = 1, lty = 3)
  #lines(xval, predamb$upr, col = palette()[1], lwd = 1, lty = 3)
  
  #lines(xval, predele$fit, col = palette()[2], lwd = 2)
  #lines(xval, predele$lwr, col = palette()[2], lwd = 1, lty = 3)
  #lines(xval, predele$upr, col = palette()[2], lwd = 1, lty = 3)
  
  lines(xval, predboth$fit, col = "black", lwd = 2)
  lines(xval, predboth$lwr, col = "black", lwd = 1, lty = 3)
  lines(xval, predboth$upr, col = "black", lwd = 1, lty = 3)
  #legend("bottomright",legend=paste("y =",int,"+",slope,"x,","r2 =",r2,sep=" "),bty="n")
  
  dev.copy2pdf(file="output/R_growth_maintenance_linear.pdf")
  
  
  
  
  #---- Resp vs. AGR
  #- on a LOG SCALE
  
  model1 <- lme(logy~T_treatment+logx+logx:T_treatment,random=~1|chamber,data=gmr) # slopes not different
  model2 <- lme(logy~T_treatment,random=~1|chamber,data=gmr)                     # intercepts are not different
  model3 <- lme(logy~logx,data=gmr,random=~1|chamber)
  model4 <- lm(logy~logx,data=gmr)
  
  int <- unname(round(coef(model4)[1],3))
  slope <- unname(round(coef(model4)[2],4))
  r2 <- round(summary(model4)$r.squared,2)
  
  
  #- add predictions for ambient and warmed treatments
  # Set up a regular sequence of numbers, for which respiration is to be predicted
  xval <- seq(min(gmr$logx), max(gmr$logx), length = 101)
  
  # Two separate dataframes, one for each treatment/
  amb_dfr <- data.frame(logx = xval, T_treatment = "ambient")
  ele_dfr <- data.frame(logx = xval, T_treatment = "elevated")
  
  model_amb <- lm(logy~logx,data=subset(gmr,T_treatment=="ambient"))
  model_ele <- lm(logy~logx,data=subset(gmr,T_treatment=="elevated"))
  
  # Predictions of the model using 'predict.lm'
  predamb <- as.data.frame(predict(model_amb, amb_dfr, interval = "confidence"))
  predele <- as.data.frame(predict(model_ele, ele_dfr, interval = "confidence"))
  
  # windows(20,20)
  # par(mar=c(6,6,1,1),cex.lab=1.8,cex.axis=1.2,las=1)
  # plotBy(logy~logx|T_treatment,data=subset(gmr,Water_treatment=="control"),pch=16,
  #        legend=F,cex=1.2,xlim=c(-3,-1.5),ylim=c(-1.8,-0.6),
  #        ylab=expression(log[10]~(Respiration~per~unit~tree~C~(gC~gC^-1~d^-1))),
  #        xlab=expression(log[10]~(Relative~growth~rate~(gC~gC^-1~d^-1))))
  # plotBy(logy~logx|T_treatment,data=subset(gmr,Water_treatment=="drydown"),pch=1,
  #         legend=F,add=T,cex=1.2)
  # legend("topleft",pch=c(16,16,1,1),col=palette()[1:2],
  #         legend=c("A-Con","W-Con","A-Dry","W-Dry"),bty="n",cex=1.2)
  # 
  # Add the lines; the fit and lower and upper confidence intervals.
  # lines(xval, predamb$fit, col = palette()[1], lwd = 2)
  # lines(xval, predamb$lwr, col = palette()[1], lwd = 1, lty = 3)
  # lines(xval, predamb$upr, col = palette()[1], lwd = 1, lty = 3)
  # 
  # lines(xval, predele$fit, col = palette()[2], lwd = 2)
  # lines(xval, predele$lwr, col = palette()[2], lwd = 1, lty = 3)
  # lines(xval, predele$upr, col = palette()[2], lwd = 1, lty = 3)
  # 
  # dev.copy2pdf(file="output/R_growth_maintenance_logscale.pdf")
  # 
  
  
  #-------------------------------------------------------------------
  #- alternatively solve eq. 2 in Amthor 2000
  #  respiration = Rg*Growth+Rm*Mass
  #Ra ~ g*dMass_C+m*totMass_c
  amthor1 <- lm(Ra~dMass_c+dMass_c:T_treatment+totMass_c,data=gmr)
  amthor2 <- lm(Ra~dMass_c+totMass_c,data=gmr)
  
  library(lmerTest)
  library(lme4)
  library(visreg)
  amthor3 <- lmer(Ra~dMass_c+dMass_c:T_treatment+totMass_c+totMass_c:T_treatment+(1|chamber),data=gmr)
  anova(amthor3)
  coef(amthor3)
  confint(amthor3)
  
  #- refit amthor model for treatments separately
  amthor3.amb <- lmer(Ra~dMass_c+totMass_c+(1|chamber),data=subset(gmr,T_treatment=="ambient"))
  confint(amthor3.amb)
  summary(amthor3.amb)
  amthor3.ele <- lmer(Ra~dMass_c+totMass_c+(1|chamber),data=subset(gmr,T_treatment=="elevated"))
  confint(amthor3.ele)
  summary(amthor3.ele)
  
  # this is the version used to produce the parameters in Table 1.
    amthor4 <- lmer(Ra~dMass_c+totMass_c+(1|chamber),data=gmr)
  coef(amthor4)
  visreg(amthor2,"totMass_c","dMass_c",overlay=T)
}
