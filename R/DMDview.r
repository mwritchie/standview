# Code written by Martin Ritchie, USFS, Pacific Southwest Research Station, Redding Laboratory

dmd.view<-function(ineq         = 1,
                   inul         = TRUE,
                   insdi        = TRUE,
                   inply        = TRUE,
                   insdr        = FALSE,
                   insdl        = TRUE,
                   max.sdi      = 550,
                   dmd.title    = " ",
                   sdi.lines    = c( 50, 100, 150, 200, 250, 350 ),
                   mgt.zone     = c(0.35,0.60),
                   reineke.term = 1.605,
                   bsi          = 90,
                   sdicol       = "red",
                   invol        = FALSE,
                   vcol         = "blue",
                   use.metric   = FALSE){

ineq<-as.numeric(ineq)
max.sdi<-as.numeric(max.sdi)
sdi.lines<-as.numeric(sdi.lines)
mgt.zone<-as.numeric(mgt.zone)
bsi<- as.numeric(bsi)
reineke.term<-as.numeric(reineke.term)
mzcol="grey"

e.code <- 0 # error code 0 indicates successful process
# test for acceptable range of values
if(!(ineq %in% 1:9)) {
  message("Unacceptable value for argument ineq; Figure not Rendered")
  return()
}
# test for acceptable title, must be character string
if(!is.character(dmd.title)){
  message("Invalid argument dmd.title; must be a character. Figured not rendered by dmd.view")
  return()
}
# test for acceptable site index must be between 70 and 110
if(!(bsi>=70 & bsi<=110)){
  message("Invalid argument bsi; Barrett's SI must be between 70 and 110")
  return()
}
if(sum(is.na(sdi.lines))){
  message("Invalid sdi.lines array, contains NA or negative values")
  return()
}
sdi.lines<-sort(sdi.lines)

if(!(max.sdi>200)){
  message("Invalid sdi maximum, max.sdi must exceed 200")
  return()
}

# check to see if the sdi.lines are less than max.sdi
if(min(max.sdi-sdi.lines)<=0){
  message("Invalid sdi.lines are not all less than sdi maximum")
  return()
}

# test for acceptable reineke term must be between 1.50 and 2.0
if(!(reineke.term>=1.50 | bsi<=2.00)){
  message("Invalid argument reineke.term; must be between 1.50 and 2.00")
  return()
}

#####Prescription #######################################################
main.t  <- switch(ineq,
                  dmd.title,                                     #1
                  "Ponderosa Pine (Long and Shaw 2005)",         #2
                  "Ponderosa Pine (Ritchie and Zhang 2018)",     #3
                  "Ponderosa Pine (Edminster 1988)",             #4
                  "Ponderosa Pine (Cochran 1992)",               #5
                  "Mixed-Conifer (Long and Shaw 2012)",          #6
                  "Coastal Douglas-Fir (Long et al 1988)",       #7
                  "White Fir (Zhang et al. 2007)",               #8
                  "Lodgepole Pine McCarter and Long (1986)")     #9

max.sdi <- switch(ineq,
                  ifelse(max.sdi<=1000 & max.sdi>=300, max.sdi , 400),
                  450,
                  400,
                  410,
                  365,
                  ifelse(!(max.sdi<=600 & max.sdi>=450), 550, max.sdi),
                  600,
                  800,
                  700)

tcex    <- switch(ineq, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50)
acex    <- switch(ineq, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75)
scex    <- switch(ineq, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75)
sdilw   <- switch(ineq, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00)

gridcol <- switch(ineq, "grey","grey","grey","grey","grey","grey","grey","grey","grey")
gridlw  <- switch(ineq, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)

# min for x and y axis
min.x   <- switch(ineq, 40, 40, 15, 40, 40, 40, 50, 50, 80)
min.y   <- switch(ineq,  1,  1,  1,  1,  1,  1,  1,  1,  1)

# max for x and y axis
max.x   <- switch(ineq, 1000,     1000,   1000,  1200,  1000,  1000,  1000,  2000, 2000 )
max.y   <- switch(ineq,   36,       36,    36,    30,    36,    36,     36,    36,   26 )

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
                  c( 0.88, 1.10))
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
                  c( 0.70, 1.20))
# tick marks on x axis
x.at    <- switch(ineq,
                  c(min.x, 50, 60, 70, 80, 90, 100, 120, 140, 170,
                    200, 250, 300, 350, 400, 500, 600, 700, 800, 900, max.x),
                  c(min.x, 50, 60, 70, 80, 90, 100, 120, 140, 170,
                    200, 250, 300, 350, 400, 500, 600, 700, 800, 900, max.x),
                  c(min.x, 20, 30, 40, 50, 60, 70, 80, 90, 100, 120, 140, 170,
                    200, 250, 300, 350, 400, 500, 600, 700, 800, 900, max.x),
                  c(min.x, 50, 60, 70, 80, 90, 100, 120, 140, 170,
                    200, 250, 300, 350, 400, 500, 600, 700, 800, 900, 1000, 1100, max.x),
                  c(min.x, seq(from=50, to=100, by=10), 120, 140, 170,
                    200, 250, 300, 350, 400, 500, 600, 700, 800, max.x),
                  c(min.x, 50, 60, 70, 80, 90, 100, 120, 140, 170,
                    200, 250, 300, 350, 400, 500, 600, 700, 800, 900, max.x),
                  c(min.x, 50, 60, 70, 80, 90, 100, 120, 140, 170,
                    200, 250, 300, 350, 400, 500, 600, 700, 800, 900, max.x),
                  c(min.x, 50, 60, 70, 80, 90, 100, 120, 140, 170,
                    200, 250, 300, 350, 400, 500, 600, 700, 800, 900,
                    seq(1000, 1900, 100), max.x),
                  c(min.x, 90, 100, 120, 140, 170,
                    200, 250, 300, 350, 400, 500, 600, 700, 800, 900,
                    seq(1000, 1900, 100), max.x))

# This is the list of annotations for x-axis (levels of tpa)
xaxl    <- switch(ineq,
                  c(min.x, 60, 80, 100, 140, 200, 300, 400,
                    600, 800, max.x),
                  c(min.x, 50, 60, 80, 100, 140, 200, 300,
                    400, 600, 800, max.x),
                  c(min.x, 20, 40, 60, 80, 100, 140, 200,
                    300, 400, 600, 800, max.x),
                  c(min.x, 60, 80, 100, 140, 200, 300, 400,
                    600, 800, max.x),
                  c(min.x, 60, 80, 100, 140, 200, 300,
                    400, 600, 800, max.x),
                  c(min.x, 60, 80, 100, 140, 200, 300, 400,
                    600, 800, max.x),
                  c(min.x, 60, 80, 100, 140, 200, 300, 400,
                    600, 800, max.x),
                  c(min.x, 60, 80, 100, 140, 200, 300,
                    400, 600, 800, 1000, 1500, max.x),
                  c(min.x, 100, 140, 200, 300,
                    400, 600, 800, 1000, 1500, max.x))

y.at    <- switch(ineq,
                  seq(from=min.y, to=max.y, by=1),
                  c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),
                  c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),
                  c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),
                  c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),
                  c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),
                  c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),
                  c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y),
                  c(min.y, seq(from=1.5, to=6.5, by=0.5), 7:max.y))

yaxl   <- switch(ineq,
                 c(1:12,seq(from=14, to=max.y, by=2)),
                 c(1:12,seq(from=14, to=max.y, by=2)),
                 c(1:12,seq(from=14, to=max.y, by=2)),
                 c(1:12,seq(from=14, to=max.y, by=2)),
                 c(1:12,seq(from=14, to=34   , by=2)),
                 c(1:12,seq(from=14, to=max.y, by=2)),
                 c(1:12,seq(from=14, to=max.y, by=2)),
                 c(1:12,seq(from=14, to=max.y, by=2)),
                 c(1:12,seq(from=14, to=max.y, by=2)) )

# vertical grid lines at:
v.grid <- switch(ineq,
                 c(50,60,70,80, 90, 100, 120, 140,
                   170, 200,250,300,350,400,500,600,700,800,900,1000), #1
                 c(50,60,70,80, 90, 100, 120, 140,
                   170, 200,250,300,350,400,500,600,700,800,900,1000), #2
                 c(20, 30, 40, 50, 60, 70, 80, 90, 100, 120, 140, 170,
                   200,250,300,350,400,500,600, 700, 800, 900, 1000), #3
                 c(50, 60, 70, 80, 90, 100, 120, 140, 170, 200, 250,
                   300,350,400,500,600, 700, 800, 900, 1000, 1100, 1200), #4
                 c(50, 60, 70, 80, 90, 100, 120, 140, 170,
                   200,250,300,350,400,500,600, 700, 800, 900, 1000), #5
                 c(60, 70, 80, 90, 100, 120, 140, 170,
                   200,250,300,350,400,500,600, 700, 800, 900, 1000), #6
                 c(60, 70, 80, 90, 100, 120, 140, 170,
                   200,250,300,350,400,500,600, 700, 800, 900, 1000),
                 c(60, 70, 80, 90, 100, 120, 140, 170,
                   200,250,300,350,seq(400, 1900, 100), max.x),
                 c(90, 100, 120, 140, 170,
                   200,250,300,350,seq(400, 1900, 100), max.x))
# horizontal gridlines at:
h.grid <- switch(ineq,
                 c(seq(from=1.5, to=6.5, by=0.5), 7:max.y),   #1
                 c(seq(from=1.5, to=6.5, by=0.5), 7:max.y),   #2
                 c(seq(from=1.5, to=6.5, by=0.5), 7:max.y),   #3
                 c(seq(from=1.5, to=6.5, by=0.5), 7:max.y),   #4
                 c(seq(from=1.5, to=6.5, by=0.5), 7:max.y),   #5
                 c(seq(from=1.5, to=6.5, by=0.5), 7:max.y),   #6
                 c(seq(from=1.5, to=6.5, by=0.5), 7:max.y),   #7
                 c(seq(from=1.5, to=6.5, by=0.5), 7:max.y),   #8
                 c(seq(from=1.5, to=6.5, by=0.5), 7:max.y))   #9

grid.lw <- switch(ineq, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)

# this is the tx[] array offset for starting the plot of max sdi line
sdi.strt<- switch(ineq, 1, 7, 4, 4, 1, 4, 4, 4, 4 )

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
                c(2.21, 1.08, 0.94, 1.25))

sdi.lines<- switch(ineq,
                   sdi.lines,
                   c(50, 100, 150, 200, 250, 350 ),
                   c(50, 100, 150, 200, 250, 325 ),
                   c(50, 100, 150, 200, 250, 325 ),
                   c(50, 100, 150, 200, 250, 300 ),
                   c(50, 100, 150, 200, 300, 450),
                   c(100, 150, 200, 300, 400, 500),
                   c(100, 150, 200, 300, 400, 500),
                   c(100, 200, 300, 400, 500, 600))

# lower limit of the mgt zone in x dim
lx.mz    <- switch(ineq,
                   50,
                   50,
                   20,
                   50,
                   min.x+1,
                   min.x+1,
                   min.x+1,
                   min.x+1,
                   min.x+1)

# upper limit of the mz in x dim
ux.mz      <- switch(ineq, max.x, max.x, max.x, max.x, max.x, max.x, max.x, max.x, max.x)

# these are who knows? not used now.
mx.parms<- switch(ineq,
                  c(sdi.lines[5], sdi.lines[3], min.x*(1+2/3), max.x),  #1
                  c(250, 150, 25, max.x),  #2
                  c(220, 100, 20, max.x),  #3
                  c(220, 100, 20, max.x),
                  c(220, 100, 20, max.x),
                  c(220, 100, 20, max.x),
                  c(220, 100, 20, max.x),
                  c(220, 100, 20, max.x),
                  c(220, 100, 20, max.x))
# these guide placement of the iso lines and annotation
isod.adj<- switch(ineq,
                  c(160, 300, 12, 0),
                  c(360, 440, 12, 5),
                  c(210, 300, 12, 5),
                  c(330, 400, 12, 5),
                  c(210, 300, 12, 5),
                  c(210, 300, 12, 5),
                  c(210, 300, 12, 5),
                  c(210, 300, 12, 5),
                  c(210, 400, 12, 5))

# this is an annotation vert adjustment for max sdi
ulanny<-switch(ineq, 0.0, 0.0, 0.0, 0.0, -2.0, 0.0, 0.0, 0.0, 0.0, 0.0)

# not used right now
sdl.x <- switch(ineq,                #placeholder
                c(1,2,3,4,5,6,7),
                c(1,2,3,4,5,6,7),
                c(25, 35, 45, 55, 65, 75, 85),
                c(1,2,3,4,5,6,7),
                c(1,2,3,4,5,6,7),
                c(1,2,3,4,5,6,7),
                c(1,2,3,4,5,6,7),
                c(1,2,3,4,5,6,7),
                c(1,2,3,4,5,6,7))
# this is a placeholder for adjustment on left annotations
sdl.y <- switch(ineq,                #placeholder
                c(1,2,3,4,5,6,7),
                c(1,2,3,4,5,6,7),
                c(36, 36, 36, 36, 36, 36, 38),
                c(1,2,3,4,5,6,7),
                c(1,2,3,4,5,6,7),
                c(1,2,3,4,5,6,7),
                c(1,2,3,4,5,6,7),
                c(1,2,3,4,5,6,7),
                c(1,2,3,4,5,6,7))
#adjustment for annotations on right side of graph
sdra<- switch(ineq,
              1.150,
              1.150,
              1.150,
              1.150,
              1.150,
              1.150,
              1.150,
              1.150,
              1.150)
# I have no idea why this is here...
sd.segoff<-switch(ineq, 11.4, 11.4, 11.4, 11.4, 11.4, 11.4, 11.4, 11.4, 11.4)

# lower sdi limit of the management zone
lsd     <- switch(ineq,
                  ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), sdi.lines[3]),
                  150,
                  10* round((0.30*max.sdi/10), 0),
                  10* round((0.20*max.sdi/10), 0),
                  60,
                  ifelse(!is.na(mgt.zone[1]), round(mgt.zone[1]*max.sdi,0), sdi.lines[3]),
                  10* round((0.30*max.sdi/10), 0),
                  10* round((0.30*max.sdi/10), 0),
                  10* round((0.30*max.sdi/10), 0))

# upper sdi limit of the management zone
usd     <- switch(ineq,
                  ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), sdi.lines[5]),
                  250,
                  10 * round((0.55*max.sdi/10),0),
                  10 * round((0.60*max.sdi/10),0),
                  round(max.sdi*(-0.36+0.01*bsi),0),
                  ifelse(!is.na(mgt.zone[2]), round(mgt.zone[2]*max.sdi,0), sdi.lines[5]),
                  10 * round((0.55*max.sdi/10),0),
                  10 * round((0.55*max.sdi/10),0),
                  10 * round((0.55*max.sdi/10),0))

if( lsd>=usd | is.na(lsd) | is.na(usd)){
  message("Invalid management zone limits, figure failed to render")
  return()
}

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
ls2005.vol <- function(ttt, ddd){
  vvv <- -152 + 0.017*ttt*(ddd)^2.8
  return(vvv)
}
# Ritchie and zhang volume function
rz2017.vol <- function(ttt, ddd){
  vvv <- (ttt^0.9648)*(exp(-3.8220-1.3538/sqrt(ttt)))*(ddd^2.7863)
  return(vvv)
}

fhgrid<-function(h.grid,rt,mxsdi,mxx,mnx){
  ngrid<-length(h.grid)
  for(g in 1:ngrid){
    ulgh <- mxsdi*( h.grid[g]/10 )^(-rt)
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
    ulgv <- ((mxsdi/v.grid[g])^(1/rt))*10
    if( ulgv > mxy){
      graphics::segments(v.grid[g], mny, v.grid[g],    mxy, lwd=gridlw, col=gridcol)
    }
    else{
      graphics::segments(v.grid[g], mny, v.grid[g],  ulgv, lwd=gridlw, col=gridcol)
    }
  }
}

if( !(reineke.term < 1.9 && reineke.term > 1.4)){
  message("Invalid Reineke slope. DMD failed to render")
  e.code<-3
  return(e.code)
}
if( !(max.sdi <= 1000 && max.sdi >300) ){
  message("Invalid Limiting SDI. DMD failed to render")
  e.code<-4
  return(e.code)
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

  graphics::mtext("Trees per Acre", side=1, line=-1.0, cex=1.0)
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

  graphics::mtext("Diameter (inches)   ", side=2, line=-1.00, cex=1.0)
# yaxis values
  for(jj in 1:length(yaxl)){
    graphics::text(min.x*0.81, yaxl[jj], as.character(yaxl[jj]), cex=acex)
  }

# draw vertical and horizontal grids
  fvgrid(v.grid, slp, max.sdi, max.y, min.y)

  fhgrid(h.grid, slp, max.sdi, max.x, min.x)

# now draw in the max sdi line and annotate ###################
  graphics::lines(tx[sdi.strt:length(tx)],
                  ((max.sdi/tx[sdi.strt:length(tx)])^islp)*10,
                  type="l", col=sdicol, lwd=sdilw)

# sdi annotation elements
  if(inul){
    yyloc <- max.y + ulanny
    graphics::text((max.sdi*(yyloc/10)^(-slp))*ul.loc[1], yyloc*ul.loc[2] ,
                   "Stand Density Index",      cex=scex, col=sdicol)
    graphics::text((max.sdi*(yyloc/10)^(-slp))*ul.loc[1], yyloc      ,
                   paste("Upper Limit of:", max.sdi), cex=scex, col=sdicol)
    graphics::text((max.sdi*(yyloc/10)^(-slp))*ul.loc[1], yyloc*ul.loc[3] ,
                   paste("Reineke Value of", slp), cex=scex, col=sdicol)
    if(inply){
      graphics::text((max.sdi*(yyloc/10)^(-slp))*ul.loc[1],
                     yyloc*ul.loc[3]*0.92,
                     paste("UMZ of ", usd), cex=scex, col="darkgrey")
    }

    graphics::segments( (max.sdi*(yyloc/10)^(-slp)),           yyloc,
                        (max.sdi*(yyloc/10)^(-slp))*ul.loc[4], yyloc,
                        lwd=1, col=sdicol)
  }

# now draw box for the management zone if inply = 1 ###########
  if(inply){

    mzx <- c( lx.mz, lx.mz, ux.mz, ux.mz )  #x-coords for mzbox
    mzy <- c(10*( lsd / mzx[1] )^(islp),
             10*( usd / mzx[2] )^(islp),
             10*( usd / mzx[3] )^(islp),
             10*( lsd / mzx[4] )^(islp) )  #y-coords for mzbox
    mzw <- 2          # linewidth for mzbox

    graphics::polygon( x=mzx, y=mzy, density=NA, border=mzcol,
                       col=grDevices::rgb(0.1, 0.1, 0.1, 0.25))
  }

###############################################################

# make iso-density lines below the maximum if insdi =1 ########
  if(insdi){
    for(jsd in 1:length(sdi.lines)){
      isd  <- ((sdi.lines[jsd]/tx)^(islp))*10
      if(isd[1] <= max.y*1.15){
        graphics::lines(tx, isd,
                        type="l", col=sdicol, lwd=sdilw)
        xxann<-0.66*min.x
        yyann<-isd[1]
        graphics::segments(0.87*min.x, yyann,
                           min.x, yyann,
                           col=sdicol, lwd=sdilw)
      } else if(sdi.lines[jsd]>=isod.adj[2]){
        graphics::lines(tx[isod.adj[3]:length(tx)],
                        isd[isod.adj[3]:length(isd)],
                        type="l", col=sdicol, lwd=sdilw)
        xxann<-0.90*tx[isod.adj[3]]
        yyann<-1.00*isd[isod.adj[3]]
      } else{
        graphics::lines(tx[isod.adj[4]:length(tx)],
                        isd[isod.adj[4]:length(isd)],
                        type="l", col=sdicol, lwd=sdilw)
        xxann<-0.90*tx[isod.adj[4]]
        yyann<-1.00*isd[isod.adj[4]]
      }
      if(insdl & yyann<=max.y*1.2){
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
                   1.1*10*(max.sdi/max.x)^islp,
                   "SDI", cex=scex, col=sdicol)
    graphics::text(max.x*sdra,
                   10*(max.sdi/max.x)^islp,
                   paste(max.sdi),           cex=scex, col=sdicol)
    for(iii in 1:6){
      graphics::text(max.x*sdra,
                     10*(sdi.lines[iii]/max.x)^islp,
                     as.character(sdi.lines[iii]), cex=scex, col=sdicol)
    }
  }
#######################################################
  if(invol){
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
                 3000, 4000, 5000, 6000, 7000, 8000))
    dmd.iso(ineq,
            v.at=vol.levels,
            range.x=c(min.x, max.x),
            max.sdi=max.sdi,
            reineke.term=slp,
            show.vol=TRUE,
            v.ann=TRUE)
  }

}
