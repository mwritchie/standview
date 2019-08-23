# This file handles calculation of volume, biomass, crown cover, and height
#
# the way I handled use.metric in here is as follows:
# I assume that the input is metric if use.metric=TRUE
# and English if use.metric=FALSE. Then since the functions are
# written in English, if metric input then convert to English,
# calculate volume as normal, then if metric convert back to a volume
# in metric units.       mwr    March 21 2018.
#

# coefficients for Ritchie and Zhang pp functions
# These came from the fits done by Woong Song Jang August 22, 2019
# They were fit using fixed effects after mixed-effects models were found inferior on independent data


#				a0			a			b			c
#rz.beta.vol <-c( 1.035384, -4.741209,  3.060285, -2.319617 )

#					a0          a          b          c
#rz.beta.bio <-c(0.9936280, -7.2543293,  2.4742879, -0.2006977)

#	              	 b             c             d
#rz.beta.cc <-c(-0.0002236136,  1.2746535623,  0.9308677542)

#				              	a0           a1            b0            b1             c
#rz.cov.ht<-matrix(c(6050.3474793, 198.04600749,  4.807445e-01, -2.726396e-01, -5.6827222828,
#                    198.0460075,   17.14930305,  2.220025e-02, -3.010459e-02, -0.1812731844,
#                    0.4807445,      0.02220025,  4.312286e-05, -3.739720e-05, -0.0004778776,
#                   -0.2726396,     -0.03010459, -3.739720e-05,  6.419454e-05,  0.0003236961,
#                   -5.6827223,     -0.18127318, -4.778776e-04,  3.236961e-04,  0.0062340926), nrow=5, byrow=TRUE)


#						               a0            a             b           c
#rz.cov.vol <-matrix(c( 0.0010595783, -0.007960152,  0.0005359328,  0.01472568,
#                      -0.0079601516,  0.061858773, -0.0049967689, -0.10403963,
#                       0.0005359328, -0.004996769,  0.0007618920,  0.00354421,
#                       0.0147256780, -0.104039627,  0.0035442099,  0.24614517), nrow=4, byrow=TRUE)

#		                 				a0             a             b             c
#rz.cov.bio <-matrix(c(  2.861185e-05, -0.0002138692,  1.237792e-05,  4.393887e-04,
#                       -2.138692e-04,  0.0016656694, -1.246945e-04, -3.066672e-03,
#                        1.237792e-05, -0.0001246945,  2.189819e-05,  5.837887e-05,
#                        4.393887e-04, -0.0030666722,  5.837887e-05,  8.179116e-03), nrow=4, byrow=TRUE)


#			               b            c            d
#rz.cov.cc <-matrix(c( 3.512070e-10, 3.006206e-07, 1.632807e-07,
#                      3.006206e-07, 2.810850e-04, 1.285552e-04,
#                      1.632807e-07, 1.285552e-04, 8.169589e-05), nrow=3, byrow=TRUE)


####################################################################################################
rzheight<-function(tpa, qmd){ # Ritchie and Zhang height function
  rz.beta.ht <-c(314.29288264,  17.46283433,  -0.02719311,   0.02898155,  1.50607209) # from Woong Song Jang
  hp <- rz.beta.ht      #height parms for R&Z i know this is redundant but it makes fn easier to read

  height <- 4.5 + (hp[1]+hp[2]*sqrt(tpa))*(1-exp(qmd*(hp[3]+hp[4]/sqrt(tpa))))^hp[5]

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
