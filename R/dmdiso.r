
dmd.iso<-function(eq           = 2,
                  v.at         = c(200, 400, 600, 800, 1000, 2000,
                                   3000, 4000, 6000, 8000),
                  range.x      = c(15,1000),
                  max.sdi      = 450,
                  reineke.term = 1.60,
                  vty          = 2,
                  vcex         = 0.75,
                  vcol         = "blue",
                  v.ann        = TRUE,
                  show.vol     = TRUE){

if(!(eq %in% c(2, 3, 6, 7, 9))){
  message("Invalid equation number provided to dmd.iso, iso lines not rendered")
  return()
}

max.sdi <- switch(eq,
                  ifelse(max.sdi<=1000 & max.sdi>=300, max.sdi , 400),
                  450,
                  400,
                  410,
                  365,
                  ifelse(!(max.sdi<=600 & max.sdi>=450), 550, max.sdi),
                  600,
                  800,
                  700)

vf2<-function(vvv, ttt){
    ivv <- (( 0.017*ttt )/( vvv  + 152 ))^(-1/2.8)
    return(ivv)
}

vf3<-function(vvv, ttt){
  ivv <- ( vvv*(ttt^(-.9648))*exp(3.8220+1.3538/sqrt(ttt)) )^(1/2.7863)
  return(ivv)
}

# Long and shaw (2012)
vf6<-function(vvv, ttt){
  ivv <- ((vvv/0.007)*ttt^(-1.146))^(1/2.808)
  return(ivv)
}

# from Drew and Flewelling (1979) equation 4:
vf7<-function(vvv, ttt){
  mxdv<-exp(12.644)*(ttt)^-1.5
  ivv <- ((68.682*(vvv/ttt)-6.8084)^0.36716)*(1 - (0.32375*((vvv/ttt)/mxdv)^0.44709))
  return(ivv)
}
# from McCarter and Long (1986)
vf9<-function(vvv, ttt){
  ivv <- ((54.4*(vvv/ttt) + 5.14)^0.361)*((1 - 0.00759*ttt^0.446))
  return(ivv)
}

if(show.vol){

  v.array<-v.at

  range.x<-sort(range.x)
  tx<-range.x[1]:range.x[2]
  islp<-1/reineke.term
  # array of volumes to be produced in cubic feet per acre
  v.array<-sort(v.array)
  for(k in 1:length(v.array)){

    ivol<- switch(eq,
                  NULL,
                  vf2(v.array[k], tx),
                  vf3(v.array[k], tx),
                  NULL,
                  NULL,
                  vf6(v.array[k], tx),
                  vf7(v.array[k], tx),
                  NULL,
                  vf9(v.array[k], tx))
    iaa  <- 10*(max.sdi/tx)^(islp)
    qq   <- sum((iaa-ivol)>0)
    # now draw the volume iso line
    graphics::lines(tx[1:qq], ivol[1:qq], type="l", col=vcol, lwd=0.5, lty=vty )
    # now annotate the volume
    if(k==length(v.array)){
      if(v.ann){
        graphics::text(range.x[2]*1.15, ivol[qq]*1.18,  "Volume", cex=vcex, col=vcol)
        graphics::text(range.x[2]*1.15, ivol[qq]*1.09,
                       expression("ft"^3~"acre"^-1), cex=vcex, col=vcol)
      }
    }
    if(v.ann){
      graphics::text(range.x[2]*1.15, ivol[qq], as.character(v.array[k]), cex=vcex, col=vcol)
    }
    # now draw a segment linking the line and annotation if needed
    if(min( iaa ) < ivol[qq] ){
      graphics::segments(tx[qq],   ivol[qq], range.x[2]*0.98,
                         ivol[qq],  col=vcol, lwd=0.5,   lty=1)
    }
  }
}

return()

}
