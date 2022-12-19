# Code written by Martin Ritchie, USFS, Pacific Southwest Research Station, Redding Laboratory

dmd.view<-function(ineq         = 3,
                   inul         = TRUE,
                   insdi        = TRUE,
                   inply        = TRUE,
                   insdr        = FALSE,
                   insdl        = FALSE,
                   max.sdi      = NA,
                   dmd.title    = " ",
                   sdi.lines    = NA,
                   mgt.zone     = c(0.35,0.60),
                   reineke.term = 1.605,
                   bsi          = 90,
                   mzcol        = "grey",
                   sdicol       = "red",
                   invol        = FALSE,
                   vcol         = "blue",
                   use.metric   = FALSE){

ineq<-as.integer(ineq)
max.sdi<-as.numeric(max.sdi)
sdi.lines<-as.numeric(sdi.lines)
mgt.zone<-as.numeric(mgt.zone)
bsi<- as.numeric(bsi)
reineke.term<-as.numeric(reineke.term)
#mzcol="grey"

# standard conversions
ac.to.ha <- 2.471052
in.to.cm <- 2.54
feet.to.m <- 0.3048
ft2.to.m2 <- 0.092903
max.ineq  <- 11

# Error Checking Section

e.code <- 0 # error code 0 indicates successful process
# test for acceptable range of values
if(!(ineq %in% 1:max.ineq)) {
  message("Unacceptable value for argument ineq; Figure not Rendered")
  return()
}
# test for acceptable title, must be character string
if(!is.character(dmd.title)){
  message("Invalid argument dmd.title; must be a character. Figured not rendered by dmd.view")
  return()
}
# test for acceptable site index must be between 70 and 110
if(ineq==5){
  if(!use.metric){
    if(!(bsi>=70 && bsi<=110)){
      message("Invalid argument bsi; Barrett's SI must be between 70 and 110 feet")
      return()
    }
  } else{
    if(!(bsi>=21 && bsi<=34)){
      message("Invalid argument bsi; Barrett's SI must be between 21 and 34 m")
      return()
    }
  }
}

#if(sum(is.na(sdi.lines))){
#  message("Invalid sdi.lines array, contains NA or negative values")
#  return()
#}

#check values of max.sdi
if(ineq==1 || ineq==6 || ineq==10 || ineq==11 || ineq==12){
  if(is.na(max.sdi)){
    message("Error: User must specify max.sdi if ineq=1, 6, 10, 11, or 12. DMD not rendered")
    return()
  }
  if(!use.metric){
    if(ineq==1 && !((max.sdi>=200) && (max.sdi<=1000))){
      message("sdi upper limit for ineq=1 (200-1000 TPA) invalid: DMD not rendered")
      return()
    }
    if(ineq==6 && !((max.sdi>=450) && (max.sdi<=600))){
      message("sdi upper limit for ineq=6 (450-600 TPA) invalid: DMD not rendered")
      return()
    }
    if(ineq==10 && !((max.sdi>=580) && (max.sdi<=600))){
      message("sdi upper limit for ineq=10 (580-600 TPA) invalid: DMD not rendered")
      return()
    }
    if(ineq==11 && !((max.sdi>=700) && (max.sdi<=1000))){
      message("sdi upper limit for ineq=11 (700-1000 TPA) invalid: DMD not rendered")
      return()
    }
    if(ineq==12 && !((max.sdi>=550) && (max.sdi<=700))){
      message("sdi upper limit for ineq=12 (550-700 TPA) invalid: DMD not rendered")
      return()
    }


  } else{
    if(ineq==1 && !((max.sdi>=494) && (max.sdi<=2470))){
      message("sdi upper limit for ineq=1 (494-2470 THPA) invalid: DMD not rendered")
      return()
    }
    if(ineq==6 && !((max.sdi>=1112) && (max.sdi<=1483))){
      message("sdi upper limit for ineq=6 (1112-1483 TPHA) invalid: DMD not rendered")
      return()
    }
    if(ineq==10 && !((max.sdi>=1433) && (max.sdi<=1483))){
      message("sdi upper limit for ineq=10 (1433-1483 TPHA) invalid: DMD not rendered")
      return()
    }
    if(ineq==11 && !((max.sdi>=1605) && (max.sdi<=2223))){
      message("sdi upper limit for ineq=11 (1605-2223 TPHA) invalid: DMD not rendered")
      return()
    }
    if(ineq==12 && !((max.sdi>=1360) && (max.sdi<=1730))){
      message("sdi upper limit for ineq=12 (1360-1730 TPHA) invalid: DMD not rendered")
      return()
    }

  }
}

if(ineq==1){
  sdi.lines<-sort(sdi.lines)
  # check to see if the sdi.lines are less than max.sdi
  if(min(max.sdi-sdi.lines)<=0){
    message("Invalid sdi.lines are not all less than sdi maximum")
    return()
  }
}

# test for acceptable reineke term must be between 1.30 and 2.0
if(!(reineke.term>=1.30 & reineke.term<=2.00)){
  message("Invalid argument reineke.term; must be between 1.30 and 2.00")
  return()
}

# reference diameter for sdi: 10 inches, or 25.4 cm
sdi.index <- ifelse(!use.metric, 10, 25.4 )

#####Set Title #######################################################
main.t <- dmd.title
if(dmd.title==" "){
  main.t  <- switch(ineq,
                    dmd.title,                                           #1
                    "Ponderosa Pine (Long and Shaw 2005)",               #2
                    "Ponderosa Pine (Jang et al. 2021)",                 #3
                    "Ponderosa Pine (Edminster 1988)",                   #4
                    "Ponderosa Pine (Cochran 1992)",                     #5
                    "Mixed-Conifer (Long and Shaw 2012)",                #6
                    "Coastal Douglas-Fir (Long et al. 1988)",            #7
                    "White Fir (Zhang et al. 2007)",                     #8
                    "Lodgepole Pine (McCarter and Long 1986)",           #9
                    "Spruce/Fir (Woodall and Weiskittel 2021)",         #10
                    "Redwood/Douglas-fir (Ritchie and Berrill 2022)",   #11
                    "Douglas-fir/Redwood (Ritchie and Berrill 2022)" )  #12
}

# Set the Max SDI
if(!use.metric){ # English Units
max.sdi <- switch(ineq,
                  ifelse(max.sdi<=1000 & max.sdi>=300, max.sdi , 400),
                  450,
                  400,
                  410,
                  365,
                  ifelse(!(max.sdi<=600 & max.sdi>=450), 550, max.sdi),
                  600,
                  800,
                  700,
                  max.sdi,
                  max.sdi,
                  max.sdi)
} else{ # metric units
max.sdi <- switch(ineq,
                  ifelse(max.sdi<=2470 & max.sdi>=741, max.sdi , 988),
                  1112,
                  988,
                  1013,
                  902,
                  ifelse(!(max.sdi<=1482 & max.sdi>=1112), 1359, max.sdi),
                  1482,
                  1976,
                  1729,
                  max.sdi,
                  max.sdi,
                  max.sdi)

}

tcex    <- switch(ineq, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50,
                        0.50, 0.50)
acex    <- switch(ineq, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75,
                        0.75, 0.75)
# size of text for sdi annotation:
scex    <- switch(ineq, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.65,
                        0.65, 0.65)
sdilw   <- switch(ineq, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00,
                        1.00, 1.00)

gridcol <- switch(ineq, "grey","grey","grey","grey","grey","grey","grey","grey","grey", "grey",
                        "grey", "grey")
gridlw  <- switch(ineq, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2,
                        0.2, 0.2)

# min for x and y axis
if(!use.metric){
  min.x   <- switch(ineq, 40, 40, 15, 40, 40, 40, 50, 50, 80, 40, 40, 40)
  min.y   <- switch(ineq,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1)
} else {
  min.x   <- switch(ineq, 100, 100, 40, 100, 100, 100, 120, 120, 200, 100, 100, 100)
  min.y   <- switch(ineq,   3,   3,  3,   3,   3,   3,   3,   3,   3,   3,   3,   3)
}

# max for x and y axis
if(!use.metric){
  max.x   <- switch(ineq, 2000, 1000, 1000,  1200,  1000,  1000,  1000,  2000, 2000, 2000, 2000, 2000 )
  max.y   <- switch(ineq,   36,   36,   36,    30,    36,    36,    36,    36,   26,   16,   36,   36 )
} else {
  max.x   <- switch(ineq, 5000, 2500, 2500,  3000,  2500,  2500,  2500,  5000, 5000, 5000, 5000, 5000 )
  max.y   <- switch(ineq,   92,   92,   92,    92,    92,    92,    92,    92,   66,   40,   92,   92 )
}

# This is the slope term from Reineke Space
slp     <- switch(ineq,
                  reineke.term,
                  1.6000,
                  1.7721,
                  1.66113,
                  1.7721,
                  1.600,
                  1.605,
                  1.500,
                  1.600,
                  1.605,
                  1.600,
                  1.600)
islp    <- 1/slp              # inverse of the slope

# x-array for some plots
tx <- seq(from=min.x, to=max.x, by=1)

ylim.adj<- switch(ineq,
                  c( 0.90, 1.10),
                  c( 0.88, 1.10),
                  c( 0.88, 1.10),
                  c( 0.88, 1.10),
                  c( 0.88, 1.10),
                  c( 0.88, 1.10),
                  c( 0.88, 1.10),
                  c( 0.88, 1.10),
                  c( 0.88, 1.10),
                  c( 0.88, 1.10),
                  c( 0.88, 1.10),
                  c( 0.88, 1.10) )
# sets lower and upper limit of plotable stuff
xlim.adj<- switch(ineq,
                  c( 0.70, 1.20),
                  c( 0.70, 1.20),
                  c( 0.70, 1.20),
                  c( 0.70, 1.20),
                  c( 0.70, 1.20),
                  c( 0.70, 1.20),
                  c( 0.70, 1.20),
                  c( 0.70, 1.20),
                  c( 0.70, 1.20),
                  c( 0.70, 1.20),
                  c( 0.70, 1.20),
                  c( 0.70, 1.20))

# tick marks on x axis
if(!use.metric){  # for English Units
  x.at    <- switch(ineq,
                    c(min.x, 60, 70, 80, 90, 100, 120, 140, 160, 180,
                      200, 250, 300, 350, 400, 450, 500, 600, 700, 800, 900,
                      seq(1000, 1900, 100), max.x),
                    c(min.x, 50, 60, 70, 80, 90, 100, 120, 140, 170,
                      200, 250, 300, 350, 400, 500, 600, 700, 800, 900, max.x),
                    c(min.x, 20, 30, 40, 50, 60, 70, 80, 90, 100, 120, 140, 170,
                      200, 250, 300, 350, 400, 500, 600, 700, 800, 900, max.x),
                    c(min.x, 50, 60, 70, 80, 90, 100, 120, 140, 170,
                      200, 250, 300, 350, 400, 500, 600, 700, 800, 900, 1000, 1100, max.x),
                    c(min.x, seq(from=50, to=100, by=10), 120, 140, 170,
                      200, 250, 300, 350, 400, 500, 600, 700, 800, 900, max.x),
                    c(min.x, 50, 60, 70, 80, 90, 100, 120, 140, 160, 180,
                      200, 220, 240, 260, 280, 300, 320, 340, 360, 380, 400,
                      440, 480, 520, 560, 600, 640, 680, 720, 760, 800, 850,
                      900, 950, max.x),
                    c(min.x, 60, 70, 80, 90, 100, 125, 150, 175,
                      200, 220, 240, 260, 280, 300, 320, 340, 360, 380, 400,
                      450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, max.x),
                    c(min.x, 60, 70, 80, 90, 100, 120, 140, 160, 180,
                      200, 250, 300, 350, 400, 450, 500, 600, 700, 800, 900,
                      seq(1000, 1900, 100), max.x),
                    c(min.x, 90, 100, 120, 140, 170,
                      200, 250, 300, 350, 400, 450, 500, 600, 700, 800, 900,
                      seq(1000, 1900, 100), max.x),
                    c(min.x, 60, 70, 80, 90, 100, 120, 140, 160, 180,
                      200, 250, 300, 350, 400, 450, 500, 600, 700, 800, 900,
                      seq(1000, 1900, 100), max.x),
                    c(min.x, 60, 70, 80, 90, 100, 120, 140, 160, 180,
                      200, 250, 300, 350, 400, 450, 500, 600, 700, 800, 900,
                      seq(1000, 1900, 100), max.x),
                    c(min.x, 60, 70, 80, 90, 100, 120, 140, 160, 180,
                      200, 250, 300, 350, 400, 450, 500, 600, 700, 800, 900,
                      seq(1000, 1900, 100), max.x) )
}else{  # for metric
  x.at    <- switch(ineq,
                    c(min.x, 160, 200, 250, 300, 350, 400, 500, 600, 700, 800,
                      900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000,
                      2200, 2400, 2600, 2800, 3000, 3200, 3400, 3600, 3800, 4000, 4200,
                      4400, 4600, 4800, max.x),
                    c(min.x, 120, 150, 170, 200, 230, 260, 300, 350, 400,
                      500, 600, 700, 850, 1000, 1250, 1500, 1750, 2000, 2250, max.x),
                    c(min.x, 60, 80, 100, 125, 150, 170, 200, 230, 260, 300, 350, 400, 500,
                      600, 700, 800, 900, 1000, 1250, 1500, 1750, 2000,2250, max.x),
                    c(min.x, 120, 150, 170, 200, 230, 260, 300, 350, 400, 500, 600,
                      700, 800, 900, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, max.x),
                    c(min.x, 150, 200, 250, 300, 350, 400, 450, 500, 600, 700, 800,
                      900, 1000, 1250, 1500, 1750, 2000, 2250, max.x),
                    c(min.x, 150, 200, 250, 300, 350, 400, 450, 500, 600, 700, 800,
                      900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900,
                      2000, 2100, 2200, 2300, 2400, max.x),
                    c(min.x, 150, 170, 200, 230, 260, 300, 350, 400, 450, 500,
                      550, 600, 650, 700, 750, 800, 850, 900, 950, 1000, 1100, 1200, 1300,
                      1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300,
                      2400, max.x),
                    c(min.x, 160, 200, 250, 300, 350, 400, 500, 600, 700, 800,
                      900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000,
                      2200, 2400, 2600, 2800, 3000, 3200, 3400, 3600, 3800, 4000, 4200,
                      4400, 4600, 4800, max.x),
                    c(min.x, 250, 300, 350, 400, 500, 600, 700, 800, 900, 1000,
                      1200, 1400, 1600, 1800, 2000, 2200, 2400, 2600, 2800, 3000,
                      3200, 3400, 3600, 3800, 4000, 4200, 4400, 4600, 4800, max.x),
                    c(min.x, 160, 200, 250, 300, 350, 400, 500, 600, 700, 800,
                      900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000,
                      2200, 2400, 2600, 2800, 3000, 3200, 3400, 3600, 3800, 4000, 4200,
                      4400, 4600, 4800, max.x),
                    c(min.x, 160, 200, 250, 300, 350, 400, 500, 600, 700, 800,
                      900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000,
                      2200, 2400, 2600, 2800, 3000, 3200, 3400, 3600, 3800, 4000, 4200,
                      4400, 4600, 4800, max.x),
                    c(min.x, 160, 200, 250, 300, 350, 400, 500, 600, 700, 800,
                      900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000,
                      seq(2200,max.x,200)) )
}

# This is the list of annotations for x-axis (levels of tpa)
if(!use.metric){
  xaxl    <- switch(ineq,
                    x.at[c(1, 2,  4, 6, 8, 11, 13, 15, 17, 19, 22, 27, length(x.at))],    #1
                    x.at[c(seq(1,length(x.at)-2,2), length(x.at))],                       #2
                    x.at[c(seq(1, 13,2), 16, 19, 22, length(x.at))],                      #3
                    x.at[c(1, seq(3,length(x.at)-4,2), length(x.at))],                    #4
                    x.at[c(1, seq(3,length(x.at)-2,2), length(x.at))],                    #5
                    x.at[c(seq(1,9,2), 12, 17, 22, 27, 32, length(x.at))],                #6
                    x.at[c(1, 2,  4, 6, 8, 10,  15, 20, 24, 28, length(x.at))],           #7
                    x.at[c(1, 2,  4, 6, 8, 11, 13, 15, 17, 19, 22, 27, length(x.at))],    #8
                    x.at[c(1, 3, 5, 7, 9, 11, 13, 15, 18, 23, length(x.at))],             #9
                    x.at[c(1, 2,  4, 6, 8, 11, 13, 15, 17, 19, 22, 27, length(x.at))],    #10
                    x.at[c(1, 2,  4, 6, 8, 11, 13, 15, 17, 19, 22, 27, length(x.at))],    #11
                    x.at[c(1, 2,  4, 6, 8, 11, 13, 15, 17, 19, 22, 27, length(x.at))] )   #12
} else{
  xaxl    <- switch(ineq,
                    x.at[c(1, 3, 5, 7, 9, 11, 13, 18, 23, 28, length(x.at))],    #1
                    x.at[c(seq(1,length(x.at)-2,2), length(x.at))],              #2
                    x.at[c(seq(1, 13,2), 16, 19, 22, length(x.at))],             #3
                    x.at[c(1, seq(3,length(x.at)-2,2), length(x.at))],           #4
                    x.at[c(1, seq(3,length(x.at)-2,2), length(x.at))],           #5
                    x.at[c(seq(1, 11, 2), 14, 19, length(x.at))],                #6
                    x.at[c(seq(1, 9, 2), 13, 17, 21, 26, 31, length(x.at))],     #7
                    x.at[c(1, 3, 5, 7, 9, 11, 13, 18, 23, 28, length(x.at))],    #8
                    x.at[c(1, 3, 5, 7, 9, 11, 16, 21, length(x.at))],            #9
                    x.at[c(1, 3, 5, 7, 9, 11, 13, 18, 23, 28, length(x.at))],    #10
                    x.at[c(1, 3, 5, 7, 9, 11, 13, 18, 23, 28, length(x.at))],    #11
                    x.at[c(1, 3, 5, 7, 9, 11, 13, 18, 23, 28, length(x.at))] )   #12
}
# tick marks on y-axis
if(!use.metric){
y.at    <- switch(ineq,
                  seq(from=min.y, to=max.y, by=1),                   #1
                  c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),  #2
                  c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),  #3
                  c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),  #4
                  c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),  #5
                  c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),  #6
                  c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),  #7
                  c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),  #8
                  c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),  #9
                  seq(from=min.y, to=max.y, by=0.5),                 #10
                  c(seq(from=min.y, to=16.0, by=0.5), 17:max.y),     #11
                  c(seq(from=min.y, to=16.0, by=0.5), 17:max.y) )    #12
} else{
y.at    <- switch(ineq,
                  c(min.y:30, seq(from=33, to=max.y, by=3)),
                  c(min.y:30, seq(from=33, to=max.y, by=3)),
                  c(min.y:30, seq(from=33, to=max.y, by=3)),
                  c(min.y:30, seq(from=33, to=max.y, by=3)),
                  c(min.y:30, seq(from=33, to=max.y, by=3)),
                  c(min.y:30, seq(from=33, to=max.y, by=3)),
                  c(min.y:30, seq(from=33, to=max.y, by=3)),
                  c(min.y:30, seq(from=33, to=max.y, by=3)),
                  c(min.y:30, seq(from=33, to=max.y, by=3)),
                  c(min.y:20, seq(from=22, to=max.y, by=2)),
                  c(min.y:30, seq(from=33, to=max.y, by=3)),
                  c(min.y:30, seq(from=33, to=max.y, by=3)) )
}

# This is the list of annotations for y-axis (levels of qmd)
if(!use.metric){ #English units
  yaxl   <- switch(ineq,
                   c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #1
                   c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #2
                   c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #3
                   c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #4
                   c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #5
                   c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #6
                   c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #7
                   c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #8
                   c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #9
                   c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #10
                   c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)),  #11
                   c(seq(from=min.y, to=10, by=1), seq(12, max.y, 2)) ) #12
} else{ # metric units
  yaxl   <- switch(ineq,
                   c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)),       #1
                   c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)),       #2
                   c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)),       #3
                   c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)),       #4
                   c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)),       #5
                   c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)),       #6
                   c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)),       #7
                   c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)),       #8
                   c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)),       #9
                   c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)),       #10
                   c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)),       #11
                   c(seq(min.y, 30, 3), seq(36, to=max.y, by=6)) )      #12
}
# vertical grid lines at:
v.grid <- x.at[2:length(x.at)]

# horizontal gridlines at:
h.grid <- y.at[2:length(y.at)]

#grid.lw <- switch(ineq, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)

# this is the tx[] array offset for starting the plot of max sdi line
sdi.strt<- switch(ineq,
                  1,
                  7,
                  4,
                  4,
                  1,
                  4,
                  4,
                  4,
                  4,
                  trunc(max.sdi*(max.y/sdi.index)^(-(slp)) )- min.x + 1,
                  trunc(max.sdi*(max.y/sdi.index)^(-(slp)) )- min.x + 1,
                  4)

# this is the adjustments on the upper limit annotation
ul.loc<- switch(ineq,
                c(2.21, 1.08, 0.94, 1.25),
                c(2.21, 1.08, 0.94, 1.25),
                c(2.41, 1.08, 0.94, 1.35),
                c(2.21, 1.08, 0.94, 1.25),
                c(2.51, 1.08, 0.94, 1.60),
                c(2.21, 1.08, 0.94, 1.25),
                c(2.21, 1.08, 0.94, 1.25),
                c(2.21, 1.08, 0.94, 1.25),
                c(2.21, 1.08, 0.94, 1.25),
                c(2.21, 1.08, 0.94, 1.25),
                c(2.21, 1.08, 0.94, 1.25),
                c(2.21, 1.08, 0.94, 1.25) )
if(!use.metric){ #English units
  sdi.lines<- switch(ineq,
                     sdi.lines,
                     c(50,  100, 150, 200, 250, 350 ),
                     c(50,  100, 150, 200, 250, 325 ),
                     c(50,  100, 150, 200, 250, 325 ),
                     c(50,  100, 150, 200, 250, 300 ),
                     c(50,  100, 150, 200, 300, 450 ),
                     c(100, 150, 200, 300, 400, 500 ),
                     c(100, 200, 300, 400, 500, 600 ),
                     c(100, 200, 300, 400, 500, 600 ),
                     c(118, 177, 207, 325, 354, 395 ),
                     c(200, 300, 400, 500, 600, 800 ),
                     c(100, 200, 300, 400, 500, 700 ) )
} else{  # metric units
  sdi.lines<- switch(ineq,
                     sdi.lines,
                     c(125, 250,  375,  500,  625, 875  ),
                     c(125, 250,  375,  500,  625, 800  ),
                     c(125, 250,  375,  500,  625, 800  ),
                     c(125, 250,  375,  500,  625, 750  ),
                     c(125, 250,  375,  500,  750, 1100 ),
                     c(250, 370,  500,  740, 1000, 1240 ),
                     c(250, 500,  750, 1000, 1250, 1500 ),
                     c(250, 500,  750, 1000, 1250, 1500 ),
                     c(118, 177,  207,  325,  354,  395 ),
                     c(500, 700, 1000, 1200, 1500, 2000 ),
                     c(250, 400,  500,  750, 1000, 1240 ))

}
# upper limit of the mz in x dim
ux.mz      <- switch(ineq, max.x, max.x, max.x, max.x, max.x, max.x, max.x, max.x, max.x, max.x,
                           max.x, max.x)

# these are who knows? not used now.
#mx.parms<- switch(ineq,
#                  c(sdi.lines[5], sdi.lines[3], min.x*(1+2/3), max.x),  #1
#                  c(250, 150, 25, max.x),  #2
#                  c(220, 100, 20, max.x),  #3
#                  c(220, 100, 20, max.x),
#                  c(220, 100, 20, max.x),
#                  c(220, 100, 20, max.x),
#                  c(220, 100, 20, max.x),
#                  c(220, 100, 20, max.x),
#                  c(220, 100, 20, max.x))

# these guide placement of the iso lines and annotation
# What the hell are these? I don't remember how this works, mwr 5/22
# well the last one is the point (x) at which the isoline gets drawn
# for most situations
# the first is vestigal, no longer in use
# the second is measured in sdi english units only,
# this is a bug because it does not account for metric
isod.adj<- switch(ineq,
                  c(160, 300, 12, 5),   #1
                  c(360, 440, 12, 5),   #2
                  c(210, 300, 12, 5),   #3
                  c(330, 400, 12, 5),   #4
                  c(210, 300, 12, 5),   #5
                  c(210, 300, 12, 5),   #6
                  c(210, 300, 12, 5),   #7
                  c(210, 300, 12, 5),   #8
                  c(210, 400, 12, 5),   #9
                  c(210, 510, 12, 5),   #10
                  c(210, 1000, 12, 5),   #11
                  c(210, 510, 12, 5) )  #12

# this is an annotation vert adjustment for max sdi
#                     1    2    3    4     5    6    7    8    9    10
ulanny<-switch(ineq, 0.0, 0.0, 0.0, 0.0, -4.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                     0.0, 0.0)

# place holder not used right now
#sdl.x <- switch(ineq,                #placeholder
#                ifelse(!use.metric, c(1,2,3,4,5,6,7), c(1,2,3,4,5,6,7)),
#                ifelse(!use.metric, c(1,2,3,4,5,6,7), c(1,2,3,4,5,6,7)),
#                ifelse(!use.metric, c(1,2,3,4,5,6,7), c(1,2,3,4,5,6,7)),
#                ifelse(!use.metric, c(1,2,3,4,5,6,7), c(1,2,3,4,5,6,7)),
#                ifelse(!use.metric, c(1,2,3,4,5,6,7), c(1,2,3,4,5,6,7)),
#                ifelse(!use.metric, c(1,2,3,4,5,6,7), c(1,2,3,4,5,6,7)),
#                ifelse(!use.metric, c(1,2,3,4,5,6,7), c(1,2,3,4,5,6,7)),
#                ifelse(!use.metric, c(1,2,3,4,5,6,7), c(1,2,3,4,5,6,7)),
#                ifelse(!use.metric, c(1,2,3,4,5,6,7), c(1,2,3,4,5,6,7)))
# this is a placeholder for adjustment on left annotations
#sdl.y <- switch(ineq,                #placeholder
#                ifelse(!use.metric, c(1,2,3,4,5,6,7), c(1,2,3,4,5,6,7)),
#                ifelse(!use.metric, c(1,2,3,4,5,6,7), c(1,2,3,4,5,6,7)),
#                ifelse(!use.metric, c(1,2,3,4,5,6,7), c(1,2,3,4,5,6,7)),
#                ifelse(!use.metric, c(1,2,3,4,5,6,7), c(1,2,3,4,5,6,7)),
#                ifelse(!use.metric, c(1,2,3,4,5,6,7), c(1,2,3,4,5,6,7)),
#                ifelse(!use.metric, c(1,2,3,4,5,6,7), c(1,2,3,4,5,6,7)),
#                ifelse(!use.metric, c(1,2,3,4,5,6,7), c(1,2,3,4,5,6,7)),
#                ifelse(!use.metric, c(1,2,3,4,5,6,7), c(1,2,3,4,5,6,7)),
#               ifelse(!use.metric, c(1,2,3,4,5,6,7), c(1,2,3,4,5,6,7)))
#adjustment for annotations on right side of graph
sdra<- switch(ineq,
              ifelse(!use.metric, 1.150, 1.150),
              ifelse(!use.metric, 1.150, 1.150),
              ifelse(!use.metric, 1.150, 1.150),
              ifelse(!use.metric, 1.150, 1.150),
              ifelse(!use.metric, 1.150, 1.150),
              ifelse(!use.metric, 1.150, 1.150),
              ifelse(!use.metric, 1.150, 1.150),
              ifelse(!use.metric, 1.150, 1.150),
              ifelse(!use.metric, 1.150, 1.150),
              ifelse(!use.metric, 1.150, 1.150),
              ifelse(!use.metric, 1.150, 1.150),
              ifelse(!use.metric, 1.150, 1.150) )

# lower sdi limit of the management zone
lsd     <- switch(ineq,
                  ifelse(!is.na(mgt.zone[1]),          #1
                         round(mgt.zone[1]*max.sdi,0),
                         sdi.lines[3]),
                  ifelse(!use.metric,                  #2
                         150,
                         round(150*2.47, 0) ),
                  10* round((0.30*max.sdi/10), 0),     #3
                  10* round((0.20*max.sdi/10), 0),     #4
                  ifelse(!use.metric,                  #5
                         60,
                         round(60*2.47,0) ),
                  ifelse(!is.na(mgt.zone[1]),
                         round(mgt.zone[1]*max.sdi,0),
                         sdi.lines[3]),
                  10* round((0.30*max.sdi/10), 0),
                  10* round((0.30*max.sdi/10), 0),
                  10* round((0.30*max.sdi/10), 0),
                  ifelse(!is.na(mgt.zone[1]),
                         round(mgt.zone[1]*max.sdi,0),
                         sdi.lines[3]),
                  10* round((0.35*max.sdi/10), 0),
                  10* round((0.35*max.sdi/10), 0) )

# upper sdi limit of the management zone
usd     <- switch(ineq,
                  ifelse(!is.na(mgt.zone[2]),
                         round(mgt.zone[2]*max.sdi,0),
                         200 ),
                  ifelse(!use.metric,
                         250,
                         618 ),
                  10 * round((0.55*max.sdi/10),0),
                  10 * round((0.60*max.sdi/10),0),
                  ifelse(!use.metric,
                         round(max.sdi*(-0.36+0.01*bsi),0 ),
                         round(max.sdi*(-0.36+0.01*bsi/feet.to.m),0 ) ),
                  ifelse(!is.na(mgt.zone[2]),
                         round(mgt.zone[2]*max.sdi,0),
                         300 ),
                  10 * round((0.55*max.sdi/10),0),
                  10 * round((0.55*max.sdi/10),0),
                  10 * round((0.55*max.sdi/10),0),
                  ifelse(!is.na(mgt.zone[2]),
                         round(mgt.zone[2]*max.sdi,0),
                         300 ),
                  10 * round((0.55*max.sdi/10),0),
                  10 * round((0.55*max.sdi/10),0) )

if( lsd>=usd || is.na(lsd) || is.na(usd)){
  message("Invalid management zone limits, figure failed to render")
  return()
}

if( !(reineke.term <= 2.0 && reineke.term >= 1.3)){
  message("Invalid Reineke slope. DMD failed to render")
  e.code<-3
  return(e.code)
}
if(!use.metric){ # for English units
  if( !(max.sdi <= 1000 && max.sdi >300) ){
    message("Invalid Limiting SDI. DMD failed to render")
    return()
  }
} else {         # for metric units
  if( !(max.sdi <= 2470 && max.sdi >740) ){
    message("Invalid Limiting SDI. DMD failed to render")
    return()
  }
}

# lower limit of the mgt zone in x dim was at line 403; moved on 5/22 mwr
#lx.mz.1    <- lsd*(max.y/sdi.index)^(-(slp))
lx.mz.1 <- min.x

#lx.mz.2    <- usd*(max.y/sdi.index)^(-(slp))
lx.mz.2 <- min.x

# parameters for volume
vty <- 1   # one solid, two dashed, three dotted
vwd <- 1.5 # this is the line width
vcex<- 0.75

# parameters for dominant height
hty <- 2  # two is for dashed line
hwd <- 2  # this is the line width
hcex<-0.75

# various functions used
# long and shaw volume function
#ls2005.vol <- function(ttt, ddd){
#  vvv <- -152 + 0.017*ttt*(ddd)^2.8
#  return(vvv)
#}
# Ritchie and zhang volume function
#rz2017.vol <- function(ttt, ddd){
#  vvv <- (ttt^0.9648)*(exp(-3.8220-1.3538/sqrt(ttt)))*(ddd^2.7863)
#  return(vvv)
#}

fhgrid<-function(h.grid,rt,mxsdi,mxx,mnx){
  ngrid<-length(h.grid)
  for(g in 1:ngrid){
    ulgh <- mxsdi*( h.grid[g]/sdi.index )^(-rt)
    if( ulgh > mxx ){
      graphics::segments(mnx,  h.grid[g], mxx, h.grid[g], lwd=0.2, col="grey")
    }
    else{
      graphics::segments(mnx,  h.grid[g], ulgh, h.grid[g], lwd=0.2, col="grey")
    }
  }
}

fvgrid<-function(v.grid,rt,mxsdi,mxy,mny){
  ng<-length(v.grid)
  for(g in 1:ng){
    ulgv <- ((mxsdi/v.grid[g])^(1/rt))*sdi.index
    if( ulgv > mxy){
      graphics::segments(v.grid[g], mny, v.grid[g],    mxy, lwd=gridlw, col=gridcol)
    }
    else{
      graphics::segments(v.grid[g], mny, v.grid[g],  ulgv, lwd=gridlw, col=gridcol)
    }
  }
}



######################################################
######################################################
### Walter Meyer maximum line for CFVOL
wmx<- c(45, 65, 100, 175, 400, 482, 600, 760, 1000, 1350, 1900)
wmy<- c(30, 25,  20,  15,  10,   9,   8,   7,    6,    5,    4)
######################################################

############################Begin Plot #######################################################

  graphics::plot(NA,
     frame.plot=FALSE,
     main=main.t,
     ylab="",
     xlab="",
     ylim=c(min.y*ylim.adj[1], max.y*ylim.adj[2]),
     xlim=c(min.x*xlim.adj[1], max.x*xlim.adj[2]),
     log="xy",
     axes=FALSE,
     asp=8/6)

# draw x axis#
  graphics::axis(side=1,
       at= x.at,
       labels=FALSE,
       pos=min.y,
       cex=0.3, lwd=1.25)

  if(!use.metric){
    graphics::mtext(expression(paste("Trees Acre"^-1)),
                    side=1, line=-0.75, cex=1.0)
  }else{
    graphics::mtext(expression(paste("Trees Hectare"^-1)),
                     side=1, line=-0.75, cex=1.0)
  }

  for(jj in 1:(length(xaxl)-1)){
    graphics::text(xaxl[jj],
                   min.y*0.90,
                   paste(xaxl[jj]), cex=acex)
  }
    graphics::text(xaxl[length(xaxl)]*1.03,
                 min.y*0.90,
                 paste(xaxl[length(xaxl)]), cex=acex)

# draw y axis#
  graphics::axis(side=2,
     at = y.at,
     labels= FALSE,
     pos=min.x,
     cex=0.25, lwd=1.25)

  if(!use.metric){
    graphics::mtext("Diameter (inches)      ",
                    side=2, line=-1.00, cex=1.0)
  }else{
    graphics::mtext("Diameter (cm)      ",
                    side=2, line=-1.00, cex=1.0)
  }
# yaxis values
  for(jj in 1:length(yaxl)){
    graphics::text(min.x*0.81, yaxl[jj], as.character(yaxl[jj]), cex=acex)
  }

# draw vertical and horizontal grids
  fvgrid(v.grid, slp, max.sdi, max.y, min.y)

  fhgrid(h.grid, slp, max.sdi, max.x, min.x)

# now draw in the max sdi line and annotate ###################
  graphics::lines(tx[sdi.strt:length(tx)],
                  ((max.sdi/tx[sdi.strt:length(tx)])^islp)*sdi.index,
                  type="l", col=sdicol, lwd=sdilw)

# sdi annotation elements
  if(inul){
    yyloc <- max.y + ulanny
    graphics::text((max.sdi*(yyloc/sdi.index)^(-slp))*ul.loc[1], yyloc*ul.loc[2] ,
                   "Stand Density Index",      cex=scex, col=sdicol)
    graphics::text((max.sdi*(yyloc/sdi.index)^(-slp))*ul.loc[1], yyloc      ,
                   paste("Upper Limit of:", max.sdi), cex=scex, col=sdicol)
    graphics::text((max.sdi*(yyloc/sdi.index)^(-slp))*ul.loc[1], yyloc*ul.loc[3] ,
                   paste("Reineke Value of", slp), cex=scex, col=sdicol)
    if(inply){
      graphics::text((max.sdi*(yyloc/sdi.index)^(-slp))*ul.loc[1],
                     yyloc*ul.loc[3]*0.92,
                     paste("UMZ of ", usd), cex=scex, col="darkgrey")
    }

    graphics::segments( (max.sdi*(yyloc/sdi.index)^(-slp)),           yyloc,
                        (max.sdi*(yyloc/sdi.index)^(-slp))*ul.loc[4], yyloc,
                        lwd=1, col=sdicol)
  }

# now draw mgt zone box for the management zone if inply = 1 ###########
  if(inply){

    mzx <- c( lx.mz.1, lx.mz.2, ux.mz, ux.mz )  #x-coords for mz polygon
    mzy <- c(sdi.index*( lsd / mzx[1] )^(islp),
             sdi.index*( usd / mzx[2] )^(islp),
             sdi.index*( usd / mzx[3] )^(islp),
             sdi.index*( lsd / mzx[4] )^(islp) )  #y-coords for mz polygon
    mzw <- 2          # linewidth for mzbox

    graphics::polygon( x=mzx, y=mzy, density=NA,
                       col=grDevices::rgb(grDevices::col2rgb(mzcol)[1]/255,
                                          grDevices::col2rgb(mzcol)[2]/255,
                                          grDevices::col2rgb(mzcol)[3]/255, 0.50))
  }

###############################################################

# make iso-density lines below the maximum if insdi = TRUE ########
  if(insdi){
    for(jsd in 1:length(sdi.lines)){
      isd  <- ((sdi.lines[jsd]/tx)^(islp))*sdi.index

      #find where line goes below the minimum
      if(isd[length(tx)] <= min.y) {irng <- which(isd <= min.y)[1]} # if the sdiline goes below ymin stop here
      else {irng <- length(tx)}    # default is draw the whole line
      #

      yyann <- isd[1] # just set this as the default, not really necessary

      if(isd[1] <= max.y * 1.10){  # if it starts no more than 1.10 times the max y
        graphics::lines(tx[1:irng], isd[1:irng],
                        type="l", col=sdicol, lwd=sdilw)
        xxann <- 0.66 * min.x
        yyann <- isd[1]
        if(insdl){                # for drawing short line on left side of graph
          graphics::segments(0.87*min.x, yyann,
                           min.x, yyann,
                           col=sdicol, lwd=sdilw)
        }
      } else if(sdi.lines[jsd]>=isod.adj[2]){
        graphics::lines(tx[isod.adj[3]:length(tx)],
                        isd[isod.adj[3]:length(isd)],
                        type="l", col=sdicol, lwd=sdilw)
        xxann <- 0.90 * tx[isod.adj[3]]
        yyann <- 1.00 * isd[isod.adj[3]]
      } else{
        strtln.x <- trunc(sdi.lines[jsd]*(max.y/sdi.index)^(-(slp)) - (min.x)*0.96)
        graphics::lines(tx[strtln.x:length(tx)],
                        isd[strtln.x:length(isd)],
                        type="l", col=sdicol, lwd=sdilw)
        xxann <- 0.90 * tx[strtln.x]
        yyann <- 1.05 * isd[strtln.x]
      }

      if( (insdl) && (yyann <= (max.y*1.15))){
        graphics::text(xxann,
                       yyann,
                       paste(sdi.lines[jsd]),
                       cex=scex, col=sdicol )
      }
    }
  }
  if(insdr){
    # sdi values annotation on the right of graph
    graphics::text(max.x*sdra,
                   1.1*sdi.index*(max.sdi/max.x)^islp,
                   "SDI", cex=scex, col=sdicol)
    graphics::text(max.x*sdra,
                   sdi.index*(max.sdi/max.x)^islp,
                   paste(max.sdi),           cex=scex, col=sdicol)
    for(iii in 1:6){
      graphics::text(max.x*sdra,
                     sdi.index*(sdi.lines[iii]/max.x)^islp,
                     as.character(sdi.lines[iii]), cex=scex, col=sdicol)
    }
  }
#######################################################
# volume iso lines:
  if(invol){
    if(!use.metric){
    vol.levels<-switch(ineq,
               NULL,
               c(200, 400, 600, 800, 1000, 1500, 2000,
                 3000, 4000, 5000, 6000, 7000, 8000),
               c(200, 400, 600, 800, 1000, 1500, 2000,
                 3000, 4000, 5000, 6000, 7000, 8000),
               NULL,
               NULL,
               c(200, 400, 600, 800, 1000, 1500, 2000,
                 3000, 4000, 5000, 6000, 7000, 8000),
               c(200, 400, 600, 800, 1000, 1500, 2000,
                 3000, 4000, 6000, 8000),
               NULL,
               c(50, 200, 400, 600, 800, 1000, 1500, 2000,
                 3000, 4000, 5000, 6000, 7000, 8000),
               NULL,
               NULL)
    dmd.iso(ineq,
            v.at=vol.levels,
            range.x=c(min.x, max.x),
            max.sdi=max.sdi,
            reineke.term=slp,
            show.vol=TRUE,
            v.ann=TRUE,
            use.metric=FALSE)
    }else{
      vol.levels<-switch(ineq,
                   NULL,
                   c(30, 45, 60, 75, 100, 150,
                     200, 250, 300, 400, 500, 600, 700, 800),
                   c(30, 45, 60, 75, 100, 150,
                     200, 250, 300, 400, 500, 600, 700, 800),
                   NULL,
                   NULL,
                   c(30, 45, 60, 75, 100, 150,
                     200, 250, 300, 400, 500, 600, 700, 800),
                   c(30, 45, 60, 75, 100, 150,
                     200, 300, 400, 600, 800, 1000),
                   NULL,
                   c(30, 45, 60, 75, 100, 150,
                     200, 300, 400, 500, 600, 700, 800),
                   NULL,
                   NULL)
      dmd.iso(ineq,
              v.at=vol.levels,
              range.x=c(min.x, max.x),
              max.sdi=max.sdi,
              reineke.term=slp,
              show.vol=TRUE,
              v.ann=TRUE,
              use.metric=TRUE)
    }
  }
return()
}
