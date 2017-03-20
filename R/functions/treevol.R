#-------------------------------------------------------------------------------------------------------------------
# estimate tree wood and bark volume and wood and bark mass from a series of measurements of diameter, and the height of that diameter measurement
treevol <- function(dat){
  dat2 <- subset(dat,diam>0)
  
  #-- get the bark depth estimates (along with wood and bark densities)
  density <- getBarkWoodDensity()
  density$barkDepth <- with(density,(diamoverbark-diamunderbark)/2)
  density$log_barkDepth <- log10(density$barkDepth)
  density$log_diamoverbark <- log10(density$diamoverbark)
  lm_bd <- lm(log_barkDepth~log_diamoverbark,data=density) # fit the harvest data
  dens.coefs <- unname(coef(lm_bd))
  dat2$bark_depth <- 10^(dens.coefs[1]+dens.coefs[2]*log10(dat2$diam)) # apply bark depths to estimate inner diameters
  dat2$diam_inner <- with(dat2,diam-bark_depth)  
  
  # estimate density from diameter
  bd.coef <- unname(coef(lm(bark_density~diamoverbark,data=density)))
  wd.coef <- unname(coef(lm(wooddensity~diamoverbark,data=density)))
  
  
  #loop over each stem segment and calculate volume as the fustrum of a cone. Assume the (unmeasured) bottom bit is a cylinder.
  vol <- 0
  vol_inner <- 0
  vol.cum <- 0
  vol.cum.inner <- 0
  mass_wood <- vol_wood <- 0
  mass_bark <- vol_bark <- 0
  for (i in 1:nrow(dat2)){
    # get the vertical segment length
    if(i==1){h1 <- 0}
    if(i > 1){h1 <- dat2[i-1,"height_n"]}
    h2 <- dat2[i,"height_n"]
    h <- h2-h1
    
    # get the radii in cm
    if(i==1){r1 <- dat2[i,"diam"]/20}
    if(i > 1){r1 <- dat2[i-1,"diam"]/20}
    r2 <- dat2[i,"diam"]/20
    
    #outer volume
    vol[i] <- pi*h/3* (r1^2+r1*r2+r2^2) # volume in cm3, fustrum of a cone. See http://jwilson.coe.uga.edu/emt725/Frustum/Frustum.cone.html
    
    
    # get the inner radii in cm
    
    if(i==1){r1.inner <- dat2[i,"diam_inner"]/20}
    if(i > 1){r1.inner <- dat2[i-1,"diam_inner"]/20}
    r2.inner <- dat2[i,"diam_inner"]/20
    
    vol_inner[i] <- pi*h/3* (r1.inner^2+r1.inner*r2.inner+r2.inner^2) # volume in cm3, fustrum of a cone. See http://jwilson.coe.uga.edu/emt725/Frustum/Frustum.cone.html
    
    # get wood volume and mass
    vol_wood[i] <- vol_inner[i]
    mass_wood[i] <- vol_wood[i]*(wd.coef[1]+wd.coef[2]*mean(r1.inner,r2.inner)) #wood mass is volume time density
    
    # get bark volume and mass
    vol_bark[i] <- vol[i] - vol_inner[i]
    mass_bark[i] <- vol_bark[i]*(bd.coef[1]+bd.coef[2]*mean(r1.inner,r2.inner)) #wood mass is volume time density
  }
  
  
  df.out <- dat2[1,c(1,2,3,4,5,6)]
  df.out$diam <- max(dat2$diam)
  df.out$height <- max(dat2$Plant_height)
  df.out$vol <- sum(vol,na.rm=T)
  df.out$vol_wood <- sum(vol_wood,na.rm=T)
  df.out$vol_bark <- sum(vol_bark,na.rm=T)
  df.out$mass_wood <- sum(mass_wood,na.rm=T)
  df.out$mass_bark <- sum(mass_bark,na.rm=T)
  
  return(df.out)
}
#-------------------------------------------------------------------------------------------------------------------