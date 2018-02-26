
gdmd.view<-function(ineq         = 1,
                    inul         = TRUE,
                    inrd         = TRUE,
                    rdlabel      = TRUE,
                    inply        = TRUE,
                    inqmd        = TRUE,
                    inspace      = TRUE,
                    max.sdi      = 400,
                    umz          = 250,
                    lmz          = 100,
                    mgt.zone     = c(0.35,0.60),
                    reineke.term = 1.6,
                    bsi          = 90,
                    dcol         = "blue",
                    rdcol        = "black",
                    dmd.title    = " "){

ineq         <- as.numeric(ineq)
max.sdi      <- as.numeric(max.sdi)
mgt.zone     <- as.numeric(mgt.zone)
bsi          <- as.numeric(bsi)
reineke.term <- as.numeric(reineke.term)
umz          <- as.numeric(umz)

dcol         <- as.character(dcol)
rdcol        <- as.character(rdcol)
dmd.title    <- as.character(dmd.title)

# test for acceptable reineke term must be between 1.50 and 2.0
if(!(reineke.term>=1.50 | bsi<=2.00)){
  message("Invalid argument reineke.term; must be between 1.50 and 2.00 diagram not rendered")
  return()
}

# test for acceptable site index must be between 70 and 110
if(!(bsi>=70 & bsi<=110)){
  message("Invalid argument bsi; Barrett's SI must be between 70 and 110, diagram not rendered")
  return()
}

# test for acceptable umz direct input
if(is.na (umz)){
  message("Invalid argument umz; must not be NA")
  return()
}

# test for acceptable umz direct input
if(is.na (lmz)){
  message("Invalid argument lmz; must not be NA")
  return()
}

# test for acceptable umz direct input
if(umz > max.sdi){
  message("Invalid argument umz; must be less than max.sdi")
  return()
}

# test for acceptable lmz direct input
if(lmz > umz){
  message("Invalid argument lmz; must be less than umz")
  return()
}

# test for acceptable mgt.zone input
if(!(length(mgt.zone)==2)){
  message("mgt.zone must be vector of length 2, NA is acceptable")
  return()
}

fk <- pi/576   #Foresters constant
mzcol        = "lightgrey"

dmd.title<-switch(ineq,
                  dmd.title,                                     #1
                  "Ponderosa Pine Long and Shaw (2012)",         #2
                  "Ponderosa Pine Ritchie and Zhang (2018)",     #3
                  "Ponderosa Pine Edminster (1988)",             #4
                  "Ponderosa Pine (Cochran 1992)",               #5
                  "Mixed-Conifer (Long and Shaw 2012)",          #6
                  "Coastal Douglas-Fir (Long et al 1988)",       #7
                  "White Fir (Zhang et al 2007)",                #8
                  "Lodgepole pine (McCarter and Long 1988)" )    #9

#                        1      2     3     4     5     6     7     8      9
#inrd    <- switch(ineq, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,  TRUE )
max.x   <- switch(ineq,  600,  600,  600,  600,  600,  600,  600,  800,    900 )
max.y   <- switch(ineq,  350,  350,  300,  350,  250,  450,  450,  700,    550 )
max.sdi <- switch(ineq,  450,  450,  400,  410,  365,  550,  600,  800,    700)
slp     <- switch(ineq,
                  reineke.term,
                  1.6000,
                  1.7721,
                  1.66113,
                  1.7721,
                  1.600,
                  1.600,
                  1.500,
                  1.605)

mgt.zone <-switch(ineq,
                 mgt.zone,
                 c(0.35, 0.55),
                 c(0.25, 0.55),
                 c(0.30, 0.60),
                 c(0.20, 0.60),
                 c(0.35, 0.55),
                 c(0.15, 0.55),
                 c(0.20, 0.55),
                 c(0.20, 0.55),
                 c(0.20, 0.55))

islp    <- 1/slp             # inverse of reineke term
xr      <- c(-10, max.x+50)  # x range
yr      <- c(  0, max.y+40)    # y range

rdl   <- seq(from=0.2, to=1.0, by=0.1)      # relative density levels
diso  <- c(3:11, seq(from=12, to=24, by=2)) #diameter lines

# limits for the density lines
frt   <- max(diso)
tot   <- min(diso)
eprd  <-array(0.00, dim=c(length(rdl),2))  # array for rd endpoints

# markers for square spacing on the x-axis:
space <- switch(ineq,
                10:18,
                10:18,
                10:18,
                10:18,
                10:18,
                10:18,
                10:18,
                c(8:16,18),
                c(8:16,18))

# lower sdi limit of the management zone
lsd     <- switch(ineq,
                  ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                  150,
                  10* round((0.30*max.sdi/10), 0),
                  10* round((0.20*max.sdi/10), 0),
                  60,
                  ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), lmz),
                  10* round((0.30*max.sdi/10), 0),
                  10* round((0.20*max.sdi/10), 0),
                  10* round((0.20*max.sdi/10), 0))

# upper sdi limit of the management zone
usd     <- switch(ineq,
                  ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                  250,
                  10 * round((0.55*max.sdi/10),0),
                  10 * round((0.60*max.sdi/10),0),
                  round(max.sdi*(-0.36+0.01*bsi),0),
                  ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), umz),
                  10 * round((0.55*max.sdi/10),0),
                  10 * round((0.55*max.sdi/10),0),
                  10 * round((0.55*max.sdi/10),0))

rds     <- switch(ineq,
                c(10, 20, 30, 40, 50, 60, 70, 80, 100),
                c(10, 20, 30, 50, 60, 80, 100),
                c(10, 20, 30, 50, 60, 70, 80, 90, 100),
                c(10, 20, 30, 50, 60, 70, 80, 90, 100),
                c(10, 20, 30, 40, 50, 70, 90, 100),
                c(10, 30, 40, 50, 60, 80, 90, 100),
                c(10, 50, 60, 70, 90, 100),
                c(10, 20, 30, 40, 50, 60, 70, 90, 100),
                c(10, 20, 40, 50, 70, 80, 90, 100))

# bump of RD annotation on the y-axis (multiplied by rd percent)
y.bump  <- switch(ineq, 0.025, 0.025, 0.025, 0.025, 0.000, 0.0400, 0.070, 0.200, 0.065)

# now draft the plot
graphics::plot(NA,
               frame.plot=FALSE,
               main=dmd.title,
               ylab="",
               xlab="",
               ylim=yr,
               xlim=xr,
               axes=FALSE)

#AXIS Block
# draw x axis
graphics::axis(side=1,
               at= seq(0, max.x+50, by=50),
               labels=FALSE,
               pos=0,
               cex=0.3,
               tck=+0.01,
               lwd=1.25)

graphics::mtext( expression("Trees Acre"^-1), side=1, line=0.80, cex=1.2, font=2)

for(i in seq(from=0, to=max.x, by=100)){
  graphics::mtext(side=1, at=i, paste(i), cex=1.0, line=-0.8)
}
# add spacing to x axis
if(inspace){
  atspc<-43560/(space*space)
  for(i in 1:length(space)){
    graphics::segments(atspc, -5, atspc, 0, lwd=1.1, col="red")
    graphics::mtext(side=1, at=atspc[i], paste(space[i]), cex=0.8, col="red", line=-0.1)
  }
  graphics::mtext(side=1, at=50, paste("Square"), cex=0.8, col="red", line=-0.75)
  graphics::mtext(side=1, at=70, paste("Spacing (ft):"), cex=0.8, col="red", line=-0.1)
}
# draw y axis
graphics::axis(side=2,
               at = seq(0, max.y, by=10),
               labels= FALSE,
               pos=0,
               cex=0.3,
               tck=+0.012,
               lwd=1.25)

graphics::mtext(expression(paste("Basal Area (ft"^2," ac"^-1,")")),
                side=2, line=+0.2, cex=1.2, font=2)

axe<-seq(from=0, to=max.y, by=50)
for(i in 1:length(axe)){
  graphics::text(x=-0.029*max.x, y=axe[i], paste(axe[i]), cex=1.0)
  if(i > 0){
    graphics::segments(0, axe[i], 0.020*max.x, axe[i], lwd=1.1)
  }
}

graphics::axis(side=4,
               at = seq(0, max.y, by=10),
               labels=FALSE,
               pos=max.x+60,
               cex=0.3,
               tck=+0.012,
               lwd=1.25)

#Enhance tick marks for axis 4
for(i in seq(from=0, to=max.y, by=50)){
  graphics::segments(max.x+45,  i, max.x+60,   i,  lwd=1.1)
}
#Enhance tick marks for axis 1
for(i in seq(from=0, to=max.x, by=100)){
  graphics::segments(i, 0, i, 8, lwd=1.1)
}

#make the management zone
if(inply){
  for(mm in 1:length(mgt.zone)){
  #set sdi level for upper and lower threshold
    ifelse(ineq==5 & mm==2,
           mzl <- max.sdi*(-0.36+0.01*bsi),
           mzl <- max.sdi*mgt.zone[mm])
    fff <- (mzl) * (frt/10)^(-slp) # set starting point
    ttt <- (mzl) * (tot/10)^(-slp)
    if(ttt <= max.x){
      tpa.ar<- seq(from=fff, to=ttt, by=10)
      tpa.ar<- c(tpa.ar,ttt)
      ba.ar <- fk*tpa.ar*(10*(mzl/tpa.ar)^islp)^2
    }else{
      tpa.ar<- seq(from=fff, to=max.x, by=5)
      tpa.ar<- c(tpa.ar,max.x)
      ba.ar <- fk*tpa.ar*(10*(mzl/tpa.ar)^islp)^2
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
  graphics::polygon( x=mzxa, y=mzya, density=NA, border=NULL,
                     col=grDevices::rgb(0.1, 0.1, 0.1, 0.25))
  graphics::lines(x=mzxu, mzyu, lwd=2)
}
# Draw relative density lines
if(inrd){
  for(j in 1:length(rdl)){
    ird   <- rev(rdl)[j]
    fff   <- (max.sdi*ird) * (frt/10)^(-slp) # set starting point
    ttt   <- (max.sdi*ird) * (tot/10)^(-slp)
    if(ttt <= max.x){
      tpa.ar<- seq(from=fff, to=ttt, by=10)
      tpa.ar<- c(tpa.ar,ttt)
      ba.ar <- fk*tpa.ar*(10*(max.sdi*ird/tpa.ar)^islp)^2
    }else{
      tpa.ar<- seq(from=fff, to=max.x, by=5)
      tpa.ar<- c(tpa.ar,max.x)
      ba.ar <- fk*tpa.ar*(10*(max.sdi*ird/tpa.ar)^islp)^2
    }
    graphics::lines(x=tpa.ar,  y=ba.ar, lwd=1.0, col=rdcol)
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
  ar.n[i,] <- c( min(max.sdi*0.2*(diso[i]/10)^(-slp), 100),
                 max.sdi*(diso[i]/10)^(-slp) )
  if( ar.n[i,2] > max.x ) {
    ar.n[i,2] <- max.x
  }
  ar.b[i,] <- ar.n[i,] * diso[i]*diso[i]*fk
  graphics::lines(x=ar.n[i,], y=ar.b[i,], lwd=1, col=dcol, lty=3)
  if(ar.n[i,2]==max.x){
    graphics::lines(x=ar.n[i,]*0.92, y=ar.b[i,]*0.92, lwd=1, col=dcol, lty=1)
  } else{
    graphics::lines(x=ar.n[i,], y=ar.b[i,], lwd=1, col=dcol, lty=1)
  }
  if(i==1 | i==length(diso)){
    graphics::lines(x=ar.n[i,], y=ar.b[i,], lwd=2, col=dcol, lty=1)
  }
  if(ar.n[i,2]>= max.x){
    graphics::text(ar.n[i,2]+10, ar.b[i,2]+5, paste(diso[i]), col=dcol)
  }else{
    if(ar.n[i,2]<= max.x-50){
      graphics::text(ar.n[i,2], ar.b[i,2]+12,
                     paste(diso[i]), col=dcol)
    }
  }
}

#Annotate diameters
if(inqmd){
  graphics::text(ar.n[length(diso),2], ar.b[length(diso),2]+32,
                 "QMD (inches)", col=dcol)
  graphics::segments(max.x, (min(diso)^2)*fk*max.x,
                     max.x, max.x*fk*(10*(max.sdi/max.x)^islp)^2,
                     lwd=1.5)
}
if(inul){
  graphics::text(max.x-65, max.y*1.05, paste("Reineke Slope= -", slp ), col=rdcol)
  graphics::text(max.x-65, max.y*1.00, paste("Max. SDI=", max.sdi), col=rdcol)
  graphics::text(max.x-65, max.y*0.95, paste("UMZ=", round(mzl,0)), col=rdcol)
}
#Annotate RD
if(rdlabel){
  for(j in 1:length(rdl)){
    rdpct<-100*rev(rdl)[j]
#    if(rdpct %in% rds){
      graphics::text(eprd[j,1]*0.96,
                     eprd[j,2]+6+y.bump*rdpct,
                     paste0(rdpct,"%"), cex=0.80, col=rdcol)
#    }
  }
  graphics::text(max.x*0.96,
                 1.14*fk*(max.x)*(10*(max.sdi/(max.x))^islp)^2+6+y.bump*rdpct,
                 "Relative", cex=0.80, col=rdcol)
  graphics::text(max.x*0.96,
                 1.08*fk*(max.x)*(10*(max.sdi/(max.x))^islp)^2+6+y.bump*rdpct,
                 "Density",  cex=0.80, col=rdcol)
}

}
