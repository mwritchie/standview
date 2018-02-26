
rzheight<-function(tpa, qmd){ # Ritchie and Zhang height function
  hp <- c(  276.49514, -124.88647, -0.0335065, 0.0452437,  1.1606388 )               #height parms for R&Z
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

dmd.volume<-function(ineq  = 1,
                     max.sdi=600,
                     tpa=NA,
                     qmd=NA,
                     ba=NA){

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


  message(switch(ineq,
                 "No volume and height for ineq=1",
                 NULL,
                 NULL,
                 "No volume and height for ineq=4",
                 "No volume and height for ineq=5",
                 NULL,
                 NULL,
                 "No volume and height for ineq=8",
                 NULL))


  # first error check the input values, no negative etc.
  if( sum(is.na(tpa))==length(tpa) ){
    if(length(qmd) != length(ba)){
      message("Invalid input to dmd.volume")
      return(NA)
    } else if(sum(qmd<0, na.rm=TRUE)>0 | sum(ba<0, na.rm=TRUE)>0 ){
      message("Invalid negative values input to dmd.volume")
      return(NA)
    } else {
      tpa<-(ba/(qmd*qmd))*576/pi
    }
  } else if( sum(is.na(qmd))==length(qmd) ) {
    if(length(tpa) != length(ba) | sum(tpa==0)<1){
      message("Invalid input to dmd.volume")
      return(NA)
    } else if(sum(tpa<0, na.rm=TRUE)>0 || sum(ba<0, na.rm=TRUE)>0  ){
      message("Invalid negative values input to dmd.volume")
      return(NA)
    } else{
      qmd<-sqrt(ba*576/(tpa*pi))
    }

  } else if( sum(is.na(ba))==length(ba)) {
    if(length(tpa) != length(qmd)){
      message("Invalid input to dmd.volume")
      return(NA)
    } else if (sum(tpa<0, na.rm=TRUE) >0 | sum(qmd<0, na.rm=TRUE)>0  ){
      message("Invalid negative values input to dmd.volume")
      return(NA)
    } else{
      ba<-round(tpa*qmd*qmd*pi/576, 3)
    }
  }
# done with error checking, build the data frame
  stands<-as.data.frame(cbind(tpa,qmd,ba))
# calculate volumes
  stands$volume <- switch(ineq,
                          NA,
                          -152+0.017*tpa*qmd^2.8,
                          (tpa^0.9648)*(exp(-3.8220-1.3538/sqrt(tpa)))*(qmd^2.7863),
                          NA,
                          NA,
                          0.007*(tpa^1.146)*(qmd^2.808),
                          dfvol(tpa, qmd, max.sdi),
                          NA,
                          (tpa/54.4)*((( qmd/(1-0.00759*tpa^0.446) )^(1/0.361))-5.14) )

  stands$volume <- (stands$volume+abs(stands$volume))/2                   #get rid of neg values
# calculate Dominant Height

  stands$height <- switch(ineq,
                          NA,                       #1. NULL
                          NA,                       #2. L&S (2005)
                          rzheight(tpa,qmd),        #3. R&Z (2018)
                          NA,                       #4. CE  (1988)
                          NA,                       #5. PC  (1992)
                          ls2012height(tpa,qmd),    #6. L&S (2012)
                          df1979height,             #7. D&F
                          NA,                       #8. NULL
                          McC1986height)            #9. NULL

  return(stands)

}
