#
# the way I handled use.metric in here is as follows:
# I assume that the input is metric if use.metric=TRUE
# and English if use.metric=FALSE. Then since the functions are
# written in English, if metric input then convert to English,
# calculate volume as normal, then if metric convert back to a volume
# in metric units.       mwr    March 21 2018.
#

rzheight<-function(tpa, qmd){ # Ritchie and Zhang height function
  hp <- c(  276.49514, -124.88647, -0.0335065, 0.0452437,  1.1606388 )      #height parms for R&Z
  height <- 4.5+(hp[1]+hp[2]/sqrt(tpa))*(1-exp(qmd*(hp[3]+hp[4]/sqrt(tpa))))^hp[5]
  return(height)
}

#ls2005height<-function(tpa, qmd){ # Long and Shaw (2005) height function
#  qtmp<-qmd
#  qtmp[qtmp<2.1]<-NA
#    height<-( (qtmp-2.07) / (202-200*tpa^0.0011) )^(1/0.64)
#  return(height)
#}   # took this function out because it is bogus

ls2012height<-function(tpa, qmd){ # Long and Shaw (2012) height function
  qtmp<-qmd
  qtmp[qtmp<1.2]<-NA
  height<-((-1.143+qtmp)/(0.679*tpa^-0.254))^1.062
  return(height)
}

McC1986height<-function(tpa, qmd){ # McCarter and Long (1986)
  mess1 <- ((1/54.4)*((( qmd/(1-0.00759*tpa^0.446) )^(1/0.361))-5.14))
  mess2 <- ( 0.00396 + 0.000779*qmd^2.27 )

  height <- (1/1.27)*(mess1^(1/0.916)/mess2)^(1/1.09)
  return(height)
}
################################################################################
dmd.volume<-function(ineq  = 2,
                     max.sdi=NULL,
                     tpa=NULL,
                     qmd=NULL,
                     ba=NULL,
                     use.metric=FALSE){

  dfvol<-function(tpa, qmd, max.sdi){ # Drew and Flewelling (1979) volume equation
    dfrd <- tpa*((qmd/10)^1.6)/max.sdi
    vol<- (1/68.682)*(6.8084+(qmd*(1-0.32375*dfrd^0.44709)^-1)^(1/0.36716))
    vol<-vol*tpa
    return(vol)
  }
  df1979height<-function(tpa, qmd, dfvol){ # Drew and Flewelling (1979) height
    height<- ((dfvol/tpa)/(0.008695+0.0007764*qmd^2.1987))^(1/1.10319)
    return(height)
  }

  if(!(ineq %in% c(2,3,6,7,9))){
    message(paste( "Error in dmd.volume, invalid input value for ineq:", ineq))
    return(NULL)
  }
  # first check to see if two NULL values are present
  if(is.null(tpa) & is.null(ba)){
    message("Both tpa and ba are NULL, invalid input to dmd.volume")
    return(NULL)
  }
  if(is.null(ba) & is.null(qmd)){
    message("Both ba and qmd are NULL, invalid input to dmd.volume")
    return(NULL)
  }
  if(is.null(tpa) & is.null(qmd)){
    message("Both tpa and qmd are NULL, invalid input to dmd.volume")
    return(NULL)
  }

  if(is.null(tpa)){tpa<-NA}
  if(is.null(qmd)){qmd<-NA}
  if(is.null(ba)){ba<-NA}

  # second error check the input values, no negative etc.
  if( sum(is.na(tpa))==length(tpa) ){
    if(length(qmd) != length(ba)){
      message("Invalid input to dmd.volume: missing values")
      return(NULL)
    } else if(sum(qmd<0, na.rm=TRUE)>0 | sum(ba<0, na.rm=TRUE)>0 ){
      message("Invalid negative values input to dmd.volume")
      return(NULL)
    } else {
      if(!use.metric){
        tpa<-(ba/(qmd*qmd))*576/pi
      }else{
        tpa<-(ba/(qmd*qmd))*40000/pi
      }
    }
  } else if( sum(is.na(qmd))==length(qmd) ) {
    if(length(tpa) != length(ba) | sum(tpa==0)<1){
      message("Invalid input to dmd.volume: missing values")
      return(NA)
    } else if(sum(tpa<0, na.rm=TRUE)>0 || sum(ba<0, na.rm=TRUE)>0  ){
      message("Invalid negative values input to dmd.volume")
      return(NA)
    } else{
      if(!use.metric){
        qmd<-sqrt(ba*576/(tpa*pi))
      }else{
        qmd<-sqrt(ba*40000/(tpa*pi))
      }
    }

  } else if( sum(is.na(ba))==length(ba)) {
    if(length(tpa) != length(qmd)){
      message("Invalid input to dmd.volume missing values")
      return(NA)
    } else if (sum(tpa<0, na.rm=TRUE) >0 | sum(qmd<0, na.rm=TRUE)>0  ){
      message("Invalid negative values input to dmd.volume")
      return(NA)
    } else{
      if(!use.metric){
        ba<-round(tpa*qmd*qmd*pi/576, 4)
      }else{
        ba<-round(tpa*qmd*qmd*pi/40000, 4)
      }
    }
  }
# done with error checking, build the data frame
  if(!use.metric){
    tpa.ac   <- tpa
    qmd.in   <- qmd
    ba.ft2ac <- ba
    stands<-as.data.frame( cbind(tpa.ac, qmd.in, ba.ft2ac) )
  } else {
    tpa.ha  <- tpa
    qmd.cm  <- qmd
    ba.m2ha <- ba
    stands<-as.data.frame( cbind(tpa.ha, qmd.cm, ba.m2ha) )
  }
# set the temporary variables in English:
  if(!use.metric){ #then it is already English
    tpae <- tpa
    qmde <- qmd
    bae  <- ba
    max.sdie<- max.sdi
  }else{           #convert to English
    tpae <- tpa*0.404686
    qmde <- qmd/2.54
    bae  <- ba*4.356
    max.sdie<- max.sdi*0.404686
  }
# calculate volumes in cubic feet per acre, change to metric if directed by use.metric
  volume <- switch(ineq,
                          NA,
                          -152+0.017*tpae*qmde^2.8,
                          (tpae^0.9648)*(exp(-3.8220-1.3538/sqrt(tpae)))*(qmde^2.7863),
                          NA,
                          NA,
                          0.007*(tpae^1.146)*(qmde^2.808),
                          dfvol(tpae, qmde, max.sdie),
                          NA,
                          (tpae/54.4)*((( qmde/(1-0.00759*tpae^0.446) )^(1/0.361))-5.14) )

  volume <- (volume+abs(volume))/2     #get rid of neg values
  vole<-volume
  if(!use.metric){
    stands$volume.ft3ac <- volume
  } else {
    stands$volume.m3ha <- volume/14.2913
  }
# calculate Dominant Height in feet, change to metric if directed by use.metric

  height <- switch(ineq,
                     NA,                        #1. NULL
                     NA,                        #2. L&S (2005)
                     rzheight(tpa=tpae,
                              qmd=qmde),        #3. R&Z (2018)
                     NA,                        #4. CE  (1988)
                     NA,                        #5. PC  (1992)
                     ls2012height(tpa=tpae,
                                  qmd=qmde),    #6. L&S (2012)
                     df1979height(tpa=tpae,
                                  qmd=qmde,
                                  dfvol=vole),  #7. D&F
                     NA,                        #8. NULL
                     McC1986height(tpa=tpae,
                                   qmd=qmde))   #9. McC (1986)

  if(!use.metric){
    stands$height.ft <- height
  } else {
    stands$height.m  <- height*0.3048
  }
# calculate Biomass in tons per acre, change to metric if directed by use.metric
  biomass <- switch(ineq,
                          NA,
                          NA,
                          tpae*exp(-7.2510)*(qmde^2.4550),
                          NA,
                          NA,
                          NA,
                          NA,
                          NA,
                          NA )
  if(!use.metric){
    stands$biomass.Tonsac <- biomass
  } else {
    stands$biomass.Mgha   <- biomass*2.2417
  }

  # calculate Crown Cover %
  bae   <- (0.005454154*qmde*qmde*tpae)
  sdie  <- tpae*(qmde/10)^1.605
  ccpct <- switch(ineq,
                           NA,
                           NA,
                           100*(tpae^-0.037)*(1-exp(-002*(tpae*(qmde/10)^1.605)-0.021*bae^(0.114*log(tpae)))),
                           NA,
                           NA,
                           NA,
                           NA,
                           NA,
                           NA )
  stands$ccpct  <- ccpct

  return(stands)

}
