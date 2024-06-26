
gdmd.view<-function(ineq         = 3,
                    inul         = TRUE,
                    inrd         = TRUE,
                    rdlabel      = TRUE,
                    inply        = TRUE,
                    inqmd        = TRUE,
                    inspace      = TRUE,
                    max.sdi      = NA,
                    umz          = NA,
                    lmz          = NA,
                    mgt.zone     = c(0.35,0.60),
                    reineke.term = 1.6,
                    bsi          = 90,
                    dcol         = "blue",
                    rdcol        = "black",
                    mzcol        = "lightgrey",
                    dmd.title    = " ",
                    use.metric   = FALSE){

ineq         <- as.integer(ineq)
max.sdi      <- as.numeric(max.sdi)
mgt.zone     <- as.numeric(mgt.zone)
bsi          <- as.numeric(bsi)
reineke.term <- as.numeric(reineke.term)
umz          <- as.numeric(umz)

dcol         <- as.character(dcol)
rdcol        <- as.character(rdcol)
dmd.title    <- as.character(dmd.title)

bae.to.bam   <- 0.2295681
sdi.index    <- ifelse(!use.metric, 10, 25.4 )
fk           <- ifelse(!use.metric, pi/576, pi/40000)   #Foresters constant
feet.to.m    <- 0.3048
max.ineq     <- 15
#olap.warn    <- 0

if(!(ineq %in% 1:max.ineq)) {
  message("Unacceptable value for argument ineq; Figure not Rendered")
  return()
}
# test for acceptable reineke term must be between 1.50 and 2.0
if(!(reineke.term>=1.30 && reineke.term<=2.00)){
  message("Invalid argument reineke.term; must be between 1.50 and 2.00 diagram not rendered")
  return()
}

if(ineq==1 || ineq==6 || ineq==10 || ineq==11 || ineq==12){
  if(is.na(max.sdi)){
    message("Error: User must specify max.sdi if ineq=1, 6, 10, 11 or 12. DMD not rendered")
    return()
  }
  if(!use.metric){
    if(ineq==1 && !((max.sdi>=200) && (max.sdi<=1000))){
      message("sdi upper limit  for ineq=1 (200-1000 TPA) invalid: DMD not rendered")
      return()
    }
    if(ineq==6 && !((max.sdi>=450) && (max.sdi<=600))){
      message("sdi upper limit for ineq=6 (450-600 TPA) invalid: DMD not rendered")
      return()
    }
    if(ineq==10 && !((max.sdi>=527) && (max.sdi<=564))){
      message("sdi upper limit for ineq=10 (527-564 TPA) invalid: DMD not rendered")
      return()
    }
    if(ineq==11 && !((max.sdi>=547) && (max.sdi<=608))){
      message("sdi upper limit for ineq=10 (547-608 TPA) invalid: DMD not rendered")
      return()
    }
    if(ineq==12 && !((max.sdi>=502) && (max.sdi<=669))){
      message("sdi upper limit for ineq=10 (502-669 TPA) invalid: DMD not rendered")
      return()
    }
    if(ineq==13 && !((max.sdi>=700) && (max.sdi<=1000))){
      message("sdi upper limit for ineq=11 (700-1000 TPA) invalid: DMD not rendered")
      return()
    }
    if(ineq==14 && !((max.sdi>=550) && (max.sdi<=700))){
      message("sdi upper limit for ineq=12 (550-700 TPA) invalid: DMD not rendered")
      return()
    }

  } else{
    if(ineq==1 && !((max.sdi>=494) && (max.sdi<=2470))){
      message("sdi upper limit for ineq=1 (494-2470 TPHA) invalid: DMD not rendered")
      return()
    }
    if(ineq==6 && !((max.sdi>=1112) && (max.sdi<=1483))){
      message("sdi upper limit for ineq=6 (1112-1483 TPHA) invalid: DMD not rendered")
      return()
    }
    if(ineq==10 && !((max.sdi>=1302) && (max.sdi<=1394))){
      message("sdi upper limit for ineq=10 (1302-1394 TPHA) invalid: DMD not rendered")
      return()
    }
    if(ineq==11 && !((max.sdi>=1352) && (max.sdi<=1502))){
      message("sdi upper limit for ineq=11 (1352-1502 TPHA) invalid: DMD not rendered")
      return()
    }
    if(ineq==12 && !((max.sdi>=1240) && (max.sdi<=1653))){
      message("sdi upper limit for ineq=12 (1240-1653 TPHA) invalid: DMD not rendered")
      return()
    }
    if(ineq==13 && !((max.sdi>=1730) && (max.sdi<=2470))){
      message("sdi upper limit for ineq=13 (1730-2470 TPHA) invalid: DMD not rendered")
      return()
    }
    if(ineq==14 && !((max.sdi>=1240) && (max.sdi<=1730))){
      message("sdi upper limit for ineq=14 (1240-1730 TPHA) invalid: DMD not rendered")
      return()
    }

  }
}

# test for acceptable site index must be between 70 and 110
if(!use.metric){
  if(!(bsi>=70 && bsi<=110)){
    message("Invalid argument bsi; Barrett's SI must be between 70 and 110 ft, diagram not rendered")
    return()
  }
} else {
  if(ineq==5 && !(bsi>=21 && bsi<=34 )){
    message("Invalid argument bsi; Barrett's SI must be between 21 m and 34 m, diagram not rendered")
    return()
  }
}
# test for acceptable mgt.zone input
if(!(length(mgt.zone)==2)){
  message("mgt.zone must be vector of length 2, NA is acceptable")
  return()
}
if(!is.na(mgt.zone[1])){
  if(mgt.zone[1]> 0.45 || mgt.zone[1] < 0.15){
    message("unnaceptable lower limit of management zone: mgt.zone[1]")
    return()
  }
}

if(!is.na(mgt.zone[2])){
  if(mgt.zone[2]> 0.80 || mgt.zone[2] < 0.50){
    message("unnaceptable upper limit of management zone: mgt.zone[1]")
    return()
  }
}

# test for acceptable umz direct input
if(is.na(mgt.zone[2])){
  if(is.na (umz)){
    message("Invalid argument umz; must not be NA")
    return()
  }
}
# test for acceptable umz direct input
if(is.na(mgt.zone[1])){
  if(is.na (lmz)){
    message("Invalid argument lmz; must not be NA")
    return()
  }
}
# test for acceptable umz direct input
if(is.na(mgt.zone[2])){
  if(umz > max.sdi){
    message("Invalid argument umz; must be less than max.sdi")
    return()
  }
}
# test for acceptable lmz direct input
if(!is.na(lmz) && !is.na(umz)){
  if(lmz > umz){
    message("Invalid argument lmz; must be less than umz")
    return()
  }
}

axis4.off    <- ifelse(!use.metric, 1.1, 1.1)

#mzcol        = "lightgrey"
main.t<-dmd.title
if(dmd.title==" "){
  main.t <- switch(ineq,
                  dmd.title,                                           #1
                  "Ponderosa Pine Long and Shaw (2005)",               #2
                  "Ponderosa Pine Jang et al. (2021)",                 #3
                  "Ponderosa Pine Edminster (1988)",                   #4
                  "Ponderosa Pine (Cochran 1992)",                     #5
                  "Mixed-Conifer (Long and Shaw 2012)",                #6
                  "Coastal Douglas-Fir (Long et al 1988)",             #7
                  "California White Fir (Zhang et al 2007)",           #8
                  "Lodgepole pine (McCarter and Long 1988)",           #9
                  "Spruce-Fir (Weiskittel and Woodall 2023)",         #10
                  "Red Spruce (Weiskittel and Woodall 2023)",         #11
                  "Balsam Fir (Weiskittel and Woodall 2023)",         #12
                  "Redwood/Douglas-fir (Ritchie and Berrill 2022)",   #13
                  "Douglas-fir/Redwood (Ritchie and Berrill 2022)",   #14
                  "Loblolly pine (Williams 1994)"               )     #15
}
#                        1      2     3     4     5     6     7     8      9    10
#inrd    <- switch(ineq, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,  TRUE, TRUE )
if(!use.metric){        #     1      2     3     4     5     6     7     8     9     10    11    12    13   14   15
  max.x   <- switch(ineq,   600,   600,  600,  600,  600,  600,  600,  800,  800,  1000, 1000, 1000, 1000, 800, 1000 )
  max.y   <- switch(ineq,   450,   350,  300,  350,  250,  450,  500,  700,  550,   450,  450,  450,  800, 600,  450)
} else{
  max.x   <- switch(ineq,  1500,  1500, 1500, 1500, 1500, 1500, 1500, 2000, 2000,  2500, 2500, 2500, 2500, 2000, 2500 )
  max.y   <- switch(ineq,   150,    80,   70,   80,   60,  110,  120,  170,  130,   110,  110,  100,  200,  140,  110 )
}

if(!use.metric){
  max.sdi <- switch(ineq,
                    ifelse(max.sdi<=1000 && max.sdi>=300, max.sdi, 400),
                    450,
                    400,
                    410,
                    365,
                    ifelse(max.sdi<=600 && max.sdi>=450, max.sdi, 550 ),
                    600,
                    800,
                    700,
                    ifelse(max.sdi<=564 && max.sdi>=527, max.sdi, 527 ),
                    ifelse(max.sdi<=608 && max.sdi>=547, max.sdi, 547 ),
                    ifelse(max.sdi<=669 && max.sdi>=502, max.sdi, 502 ),
                    ifelse(max.sdi<=1000 && max.sdi>=700, max.sdi, 900),
                    ifelse(max.sdi<=700 && max.sdi>=500, max.sdi, 700),
                    400)
} else {
  max.sdi <- switch(ineq,
                    max.sdi,
                    1110,
                    988,
                    1013,
                    901,
                    ifelse(!(max.sdi<=1482 && max.sdi>=1112), 1359, max.sdi),
                    1482,
                    1977,
                    1730,
                    ifelse(max.sdi<=1394 && max.sdi>=1302, max.sdi, 1302),
                    ifelse(max.sdi<=1502 && max.sdi>=1352, max.sdi, 1352),
                    ifelse(max.sdi<=1653 && max.sdi>=1240, max.sdi, 1240),
                    ifelse(max.sdi<=2470 && max.sdi>=1730, max.sdi, 2220),
                    ifelse(max.sdi<=1730 && max.sdi>=1240, max.sdi, 1730),
                    988)
}

slp     <- switch(ineq,
                  reineke.term,
                  1.6000,
                  1.7721,
                  1.66113,
                  1.7721,
                  1.600,
                  1.600,
                  1.500,
                  1.605,
                  1.605,
                  1.605,
                  1.605,
                  1.605,
                  1.605,
                  1.5053)

mgt.zone <-switch(ineq,
                 mgt.zone,
                 c(0.35, 0.55),
                 c(0.25, 0.55),
                 c(0.30, 0.60),
                 c(0.20, 0.60),
                 c(0.35, 0.55),
                 c(0.35, 0.55),
                 c(0.20, 0.55),
                 c(0.20, 0.55),
                 mgt.zone,
                 mgt.zone,
                 mgt.zone,
                 c(0.35, 0.55),
                 c(0.35, 0.55),
                 c(0.40, 0.55))

islp    <- 1/slp             # inverse of reineke term
if(!use.metric){
  xr      <- c(-10, max.x+50)  # x range
  yr      <- c(  0, max.y+40)    # y range
} else {
  xr      <- c(-25, max.x+124)  # x range
  yr      <- c(  0, max.y+10)    # y range
}
rdl   <- seq(from=0.2, to=1.0, by=0.1)      # relative density levels

#set maximum diameter limit
if(!use.metric){
  max.d<-switch(ineq,
                24,
                24,
                24,
                24,
                24,
                24,
                24,
                24,
                20,
                18,
                18,
                18,
                30,
                30,
                24)
} else {
  max.d<-switch(ineq,
                60,
                60,
                60,
                60,
                60,
                60,
                60,
                60,
                50,
                45,
                45,
                45,
                75,
                75,
                60)
}

# set the diameter array for display
if(!use.metric){
  if(ineq==15 || ineq==5){
    diso  <- c(3:11, seq(from=12, to=20, by=2), 24) #diameter lines
  }else{
    diso  <- c(3:11, seq(from=12, to=max.d, by=2)) #diameter lines
  }
} else {
  if(ineq==15 || ineq==5){
    diso  <- c(seq(8,32,2), seq(from=35, to=50, by=5), 60) #diameter lines
  } else{
    diso  <- c(seq(8,32,2), seq(from=35, to=max.d, by=5)) #diameter lines
  }
}

# limits for the density lines
frt   <- max(diso)
tot   <- min(diso)
eprd  <-array(0.00, dim=c(length(rdl),2))  # array for rd endpoints

# markers for square spacing on the x-axis:
if(!use.metric){
  space <- switch(ineq,
                  10:18,
                  10:18,
                  10:18,
                  10:18,
                  10:18,
                  10:18,
                  10:18,
                  c(8:16, 18),
                  c(8:16, 18),
                  c(8:14, 16, 18),
                  c(8:14, 16, 18),
                  c(8:14, 16, 18),
                  c(8:14, 16, 18),
                  c(8:14, 16, 18),
                  c(8:14,16))
} else {
  space <- switch(ineq,
                  seq(3.0,5.5,0.5),
                  seq(3.0,5.5,0.5),
                  seq(3.0,5.5,0.5),
                  seq(3.0,5.5,0.5),
                  seq(3.0,5.5,0.5),
                  seq(3.0,5.5,0.5),
                  seq(3.0,5.5,0.5),
                  seq(2.5,5.5,0.5),
                  seq(2.5,5.5,0.5),
                  seq(2.5,5.5,0.5),
                  seq(2.5,5.5,0.5),
                  seq(2.5,5.5,0.5),
                  seq(2.5,5.5,0.5),
                  seq(2.5,5.5,0.5),
                  seq(2.5,4.5,0.5))
}

# lower sdi limit of the management zone
if(!use.metric){
  lsd     <- switch(ineq,
                ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                150,
                10* round((0.30*max.sdi/10), 0),
                10* round((0.20*max.sdi/10), 0),
                60,
                ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                10* round((0.30*max.sdi/10), 0),
                10* round((0.20*max.sdi/10), 0),
                10* round((0.20*max.sdi/10), 0),
                ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                10* round((0.40*max.sdi/10), 0) )
} else {
  lsd     <- switch(ineq,
                ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                34,
                10* round((0.30*max.sdi/10), 0),
                10* round((0.20*max.sdi/10), 0),
                14,
                ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                10* round((0.30*max.sdi/10), 0),
                10* round((0.20*max.sdi/10), 0),
                10* round((0.20*max.sdi/10), 0),
                ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                10* round((0.40*max.sdi/10), 0))
}
# upper sdi limit of the management zone
if(!use.metric){
  usd     <- switch(ineq,
                ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                250,
                10 * round((0.55*max.sdi/10),0),
                10 * round((0.60*max.sdi/10),0),
                round(max.sdi*(-0.36+0.01*bsi),0),
                ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                10 * round((0.55*max.sdi/10),0),
                10 * round((0.55*max.sdi/10),0),
                10 * round((0.55*max.sdi/10),0),
                ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                10 * round((0.55*max.sdi/10),0) )
} else {
  usd     <- switch(ineq,
                ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                57,
                10 * round((0.55*max.sdi/10),0),
                10 * round((0.60*max.sdi/10),0),
                round(max.sdi*(-0.36+0.01*bsi/feet.to.m),0),
                ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                10 * round((0.55*max.sdi/10),0),
                10 * round((0.55*max.sdi/10),0),
                10 * round((0.55*max.sdi/10),0),
                ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                10 * round((0.55*max.sdi/10),0) )
}


# bump of RD annotation on the y-axis (multiplied by rd percent)
if(!use.metric){
  y.off <- 6
  y.bump  <- switch(ineq, 0.025, 0.025, 0.025, 0.025, 0.000, 0.0400, 0.069, 0.200, 0.063, 0.063, 0.063, 0.063, 0.120, 0.066, 0.050)
} else {
  y.off <- 1.3
  y.bump  <- switch(ineq, 0.025, 0.010, 0.002, 0.015, 0.000, 0.0200, 0.044, 0.040, 0.025, 0.020, 0.030, 0.030, 0.030, 0.032, 0.010)
}

# bump the qmd label
if(!use.metric){
  q.offy<-switch(ineq, 1.10, 1.09, 1.10, 1.10, 1.10, 1.10, 1.07, 1.07, 1.08, 1.08, 1.08, 1.08, 1.08, 1.08, 1.08)
} else {
  q.offy<-switch(ineq, 1.08, 1.07, 1.08, 1.08, 1.08, 1.07, 1.06, 1.08, 1.06, 1.06, 1.06, 1.06, 1.06, 1.06, 1.08)
}
# location of x-axis tpa label increment
x.ann.at<-switch(use.metric+1, 100, 200)

#location of diameter annotations
if(!use.metric){
  d.l <- switch(ineq,
                c(11, 5.0, 50,   10.0, 0),
                c(11, 5.0, 50,   10.0, 5),
                c(11, 5.0, 50,   10.0, 0),
                c(11, 5.0, 50,   10.0, 0),
                c(11, 5.0, 50,    5.0, 0),
                c(11, 5.0, 50,   10.0, 0),
                c(11, 5.0, 50,   10.0, 5),
                c(11, 5.0, 50,   13.5, 0),
                c(11, 5.0, 50,   10.0, 0),
                c(11, 5.0, 50,   8.02, 0),
                c(11, 5.0, 50,   8.02, 0),
                c(11, 5.0, 50,   8.02, 0),
                c(11, 5.0, 50,   12.0, 5),
                c(11, 5.0, 50,   10.0, 5),
                c(11, 4.0, 50,   7.00, 7) )
} else{
  d.l <- switch(ineq,
                c(35, 1.02, 150,  3.02, 0),
                c(35, 1.02, 150,  2.02, 10),
                c(35, 1.02, 150,  2.02, 0),
                c(35, 1.02, 150,  3.02, 0),
                c(35, 1.02, 150,  2.02, 0),
                c(35, 1.02, 150,  3.02, 0),
                c(35, 1.02, 150,  3.02, 0),
                c(35, 1.02, 150,  3.52, 0),
                c(35, 1.02, 150,  3.52, 0),
                c(35, 1.02, 170,  2.02, 0),
                c(35, 1.02, 150,  2.02, 0),
                c(35, 1.02, 150,  2.02, 0),
                c(35, 1.02, 150,  4.02, 10),
                c(35, 1.02, 150,  1.02, 10),
                c(35, 1.02, 150,  2.60, 20) )
}
###############################################################################################################
###############################################################################################################
# now draft the plot
graphics::plot(NA,
               frame.plot=FALSE,
               main=main.t,
               ylab="",
               xlab="",
               ylim=yr,
               xlim=xr,
               axes=FALSE)

#AXIS Block
# draw x axis
if(!use.metric){
  graphics::axis(side=1,
                 at= seq(0, max.x*1.1, by=10),
                 labels=FALSE,
                 pos=0,
                 cex=0.3,
                 tck=+0.01,
                 lwd=1.25)
} else {
  graphics::axis(side=1,
                 at= seq(0, max.x*1.1, by=25),
                 labels=FALSE,
                 pos=0,
                 cex=0.3,
                 tck=+0.01,
                 lwd=1.25)
}
if(!use.metric){
  graphics::mtext( expression("Trees Acre"^-1), side=1, line=0.80, cex=1.2, font=2)
} else {
  graphics::mtext( expression("Trees Ha"^-1), side=1, line=0.80, cex=1.2, font=2)
}
for(i in seq(from=0, to=max.x, by=x.ann.at)){
  graphics::mtext(side=1, at=i, paste(i), cex=1.0, line=-0.8)
}
#Enhance tick marks for axis 1
if(!use.metric){
  for(i in seq(from=0, to=max.x, by=x.ann.at)){
    graphics::segments(i, 0, i, 8, lwd=1.25)
  }
} else {
  for(i in seq(from=0, to=max.x, by=x.ann.at)){
    graphics::segments(i, 0, i, 3, lwd=1.25)
  }
}
# add square spacing to x axis
if(inspace){
#  atspc<-ifelse(!use.metric, 43560/(space*space),10000/(space*space))
  if(!use.metric){
    atspc <- 43560/(space*space)
  } else {
    atspc <- 10000/(space*space)
  }

  for(i in 1:length(space)){
    if(!use.metric){
      graphics::segments(atspc, -.013*max.y, atspc, +.011*max.y, lwd=1.0, col="red")
    } else{
      graphics::segments(atspc, -.013*max.y, atspc, +.011*max.y, lwd=1.0, col="red")
    }

    graphics::mtext(side=1, at=atspc[i], paste(space[i]), cex=0.8, col="red", line=-0.1)
  }
#  if(!use.metric) {graphics::mtext(side=1, at=50, paste("Square"), cex=0.8, col="red", line=-0.75)}
  if(!use.metric){
    graphics::mtext(side=1, at=50, paste("Spacing (ft):"), cex=0.8, col="red", line=-0.1)
  } else {
    graphics::mtext(side=1, at=70, paste("Spacing (m):"), cex=0.8, col="red", line=-0.1)
  }
  # think we need fix here so labels show up; see line 542
}

## draw y axis 2
if(!use.metric){
  graphics::axis(side=2,
               at = seq(0, max.y, by=10),
               labels= FALSE,
               pos=0,
               cex=0.3,
               tck=+0.012,
               lwd=1.25)
} else {
  graphics::axis(side=2,
               at = seq(0, max.y, by=2),
               labels= FALSE,
               pos=0,
               cex=0.3,
               tck=+0.012,
               lwd=1.25)
}

if(!use.metric){
  graphics::mtext(expression(paste("Basal Area (ft"^2," ac"^-1,")")),
                side=2, line=+0.2, cex=1.2, font=2)
} else {
  graphics::mtext(expression(paste("Basal Area (m"^2," ha"^-1,")")),
                side=2, line=+0.2, cex=1.2, font=2)
}

if(!use.metric){
  axe<-seq(from=0, to=max.y, by=50)
} else {
  axe<-seq(from=0, to=max.y, by=10)
}

# not sure what this does any more mwr 4/2024
for(i in 1:length(axe)){
  graphics::text(x=-0.029*max.x, y=axe[i], paste(axe[i]), cex=1.0)
  if(i > 0){
    graphics::segments(0, axe[i], 0.020*max.x, axe[i], lwd=1.25)
  }
}

## draw y-axis 4
if(!use.metric){
  graphics::axis(side=4,
               at = seq(0, max.y, by=10),
               labels=FALSE,
               pos=max.x*axis4.off,
               cex=0.3,
               tck=+0.012,
               lwd=1.25)
} else {
  graphics::axis(side=4,
               at = seq(0, max.y, by=2),
               labels=FALSE,
               pos=max.x*axis4.off,
               cex=0.3,
               tck=+0.012,
               lwd=1.25)
}

#Enhance tick marks for axis 4
if(!use.metric){
  for(i in seq(from=0, to=max.y, by=50)){
    graphics::segments((max.x*axis4.off)*.98,  i, max.x*axis4.off,   i,  lwd=1.25)
  }
} else {
  for(i in seq(from=0, to=max.y, by=10)){
    graphics::segments((max.x*axis4.off)*.98,  i, max.x*axis4.off,   i,  lwd=1.25)
  }
}
#make the management zone
if(inply){
  for(mm in 1:length(mgt.zone)){
  #set sdi level for upper and lower threshold
    if(ineq==5 & mm==2){
      mzl <- ifelse(!use.metric,
                    max.sdi*(-0.36+0.01*bsi),
                    max.sdi*(-0.36+0.01*bsi/feet.to.m))
    } else {
      mzl <- max.sdi*mgt.zone[mm]
    }
    fff <- (mzl) * (frt/sdi.index)^(-slp) # set starting point
    ttt <- (mzl) * (tot/sdi.index)^(-slp)
    if(ttt <= max.x){
      tpa.ar<- seq(from=fff, to=ttt, by=10)
      tpa.ar<- c(tpa.ar,ttt)
      ba.ar <- fk*tpa.ar*(sdi.index*(mzl/tpa.ar)^islp)^2
    }else{
      tpa.ar<- seq(from=fff, to=max.x, by=5)
      tpa.ar<- c(tpa.ar,max.x)
      ba.ar <- fk*tpa.ar*(sdi.index*(mzl/tpa.ar)^islp)^2
    }
    if(mm==1){
      mzxl<-tpa.ar
      mzyl<-ba.ar
    }
    if(mm==2){
      mzxu<-rev(tpa.ar)
      mzyu<-rev(ba.ar)
    }
  }

  mzxa<-append(mzxl, mzxu)
  mzya<-append(mzyl, mzyu)
  graphics::polygon( x=mzxa, y=mzya, density=NA, border=NA,
                     col=mzcol)

  graphics::lines(x=mzxu, mzyu, lwd=1.5) # top of mz line
  olap.warn<-min(mzyu) # this sets a variable for annotate check
  # now fix that goofy little triangle at the end of the mz

  if(max(mzxl) < max.x){
    graphics::polygon(x= c(max(mzxl),                    max.x, max.x),
                      y= c(min(mzyl),     max.x*fk*min(diso)^2, min(mzyu)),
                      density=NA, border=NA,
                      col=mzcol)
  }
# grDevices::rgb(0.1, 0.1, 0.1, 0.25)
}
# Draw relative density lines
if(inrd){
  for(j in 1:length(rdl)){
    ird   <- rev(rdl)[j]
    fff   <- (max.sdi*ird) * (frt/sdi.index)^(-slp) # set starting point
    ttt   <- (max.sdi*ird) * (tot/sdi.index)^(-slp)
    if(ttt <= max.x){
      tpa.ar<- seq(from=fff, to=ttt, by=10)
      tpa.ar<- c(tpa.ar,ttt)
      ba.ar <- fk*tpa.ar*(sdi.index*(max.sdi*ird/tpa.ar)^islp)^2
    }else{
      tpa.ar<- seq(from=fff, to=max.x, by=5)
      tpa.ar<- c(tpa.ar,max.x)
      ba.ar <- fk*tpa.ar*(sdi.index*(max.sdi*ird/tpa.ar)^islp)^2
    }
    graphics::lines(x=tpa.ar,  y=ba.ar, lwd=0.8, col=rdcol)
    eprd[j,1] <- rev(tpa.ar)[1] # save x-endpoint of each line
    eprd[j,2] <- rev(ba.ar)[1] # save y-endpoint of each line
  }
}

#next do the diameter lines.
ar.n<-array(0.00,dim=c(length(diso),2))  # x array vars for diameter iso lines
ar.b<-array(0.00,dim=c(length(diso),2))  # y array vars for diameter iso lines
# this next block of code uses a layering of lines to get the effect of different
# line widths and line types on the diameter lines.
for(i in 1:length(diso)){
  if(!use.metric){
    ar.n[i,] <- c( min(max.sdi*0.2*(diso[i]/sdi.index)^(-slp), 100),
                   max.sdi*(diso[i]/sdi.index)^(-slp) )
    if( ar.n[i,2] > max.x ) {
      ar.n[i,2] <- max.x
    }
  } else {
    ar.n[i,] <- c( min(max.sdi*0.2*(diso[i]/sdi.index)^(-slp), 300),
                   max.sdi*(diso[i]/sdi.index)^(-slp) )
    if( ar.n[i,2] > max.x ) {
      ar.n[i,2] <- max.x
    }
  }
  ar.b[i,] <- ar.n[i,] * diso[i]*diso[i]*fk

# draw heavy line if beginning or end
  if(i==1 | i==length(diso)){
    graphics::lines(x=ar.n[i,], y=ar.b[i,], lwd=2, col=dcol, lty=1) #heavy line
  }
# then draw dotted line
  graphics::lines(x=ar.n[i,], y=ar.b[i,], lwd=1, col=dcol, lty=3) # dotted line
# then draw solid line
  t.n<-ar.n[i,]
  t.b<-ar.b[i,]
  if(ar.n[i,2]>=max.x){
    t.n[2]<-ar.n[i,2]*0.92
    t.b[2]<-ar.b[i,2]*0.92
  }
  graphics::lines(x=t.n, y=t.b, lwd=1, col=dcol, lty=1)

# insert annotations for diameter lines
  if(ar.n[i,2]>= max.x){       # annotations on side
    graphics::text(ar.n[i,2]+d.l[1],
                   ar.b[i,2]+d.l[2],
                   paste(diso[i]), col=dcol, cex=0.82)
  }else{
    if(ar.n[i,2]<= max.x-d.l[3]){ # annotations along the top
      graphics::text(ar.n[i,2]+d.l[5],
                     ar.b[i,2]+d.l[4],
                     paste(diso[i]), col=dcol, cex=0.82)
    }
  }
}

# Caption for diameters
if(inqmd){
  if(!use.metric){
    graphics::text(ar.n[length(diso),2], ar.b[length(diso),2]*q.offy,
                 "QMD (inches)", col=dcol)
  } else {
    graphics::text(ar.n[length(diso),2], ar.b[length(diso),2]*q.offy,
                   "QMD (cm)", col=dcol)
  }
  graphics::segments(max.x, (min(diso)^2)*fk*max.x,
                     max.x, max.x*fk*(sdi.index*(max.sdi/max.x)^islp)^2,
                     lwd=1.5)
}

if(inul){
  graphics::text(max.x*.90, max.y*1.05, paste("Reineke Value= ", round(slp,3) ), col=rdcol)
  graphics::text(max.x*.90, max.y*1.00, paste("Max. SDI=", max.sdi), col=rdcol)
  graphics::text(max.x*.90, max.y*0.95, paste("UMZ=", round(mzl,0)), col=rdcol)
}
#Annotate RD lines
if(rdlabel){
  for(j in 1:length(rdl)){
    rdpct<-100*rev(rdl)[j]
    yspot<-eprd[j,2]+y.off+y.bump*rdpct
    if(!(yspot < olap.warn*1.1 & yspot > olap.warn*0.90)){
      graphics::text(eprd[j,1]*0.96,
                     eprd[j,2]+y.off+y.bump*rdpct,
                     paste0(rdpct,"%"), cex=0.80, col=rdcol)
    }
  }
  if(ineq==15){
    graphics::text(max.x*0.96,
                   1.21*fk*(max.x)*(sdi.index*(max.sdi/(max.x))^islp)^2+y.off+y.bump*rdpct,
                   "Relative", cex=0.80, col=rdcol)

    graphics::text(max.x*0.96,
                   1.12*fk*(max.x)*(sdi.index*(max.sdi/(max.x))^islp)^2+y.off+y.bump*rdpct,
                   "Density",  cex=0.80, col=rdcol)

  }else{
  graphics::text(max.x*0.96,
                 1.16*fk*(max.x)*(sdi.index*(max.sdi/(max.x))^islp)^2+y.off+y.bump*rdpct,
                 "Relative", cex=0.80, col=rdcol)

  graphics::text(max.x*0.96,
                 1.09*fk*(max.x)*(sdi.index*(max.sdi/(max.x))^islp)^2+y.off+y.bump*rdpct,
                 "Density",  cex=0.80, col=rdcol)
  }
}

}
