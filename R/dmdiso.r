
dmd.iso<-function(ineq         = 2,
                  v.at         = NULL,
                  range.x      = NULL,
                  max.sdi      = NULL,
                  reineke.term = 1.60,
                  vty          = 2,
                  vcex         = 0.75,
                  vcol         = "blue",
                  show.vol     = TRUE,
                  v.ann        = TRUE,
                  use.metric   = FALSE){

if(!(ineq %in% c(2, 3, 6, 7, 9))){
  message("Invalid equation number provided to dmd.iso, iso-lines not rendered")
  return()
}

if(is.null(range.x)){
    message("Range of x values not specified by range.x for dmd.iso, iso-lines not rendered")
    return()
}

if(is.null(v.at)){
  message("Volumes not specified by v.at for dmd.iso, iso-lines not rendered")
  return()
}

if(is.null(max.sdi) & (ineq %in% c(1, 6))){
  message("Limiting sdi not specified by max.sdi for dmd.iso, iso-lines not rendered")
  return()
}

if(!use.metric){   # English Units
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
} else{            # metric units
  max.sdi <- switch(ineq,
                    ifelse(max.sdi<=2470 & max.sdi>=741, max.sdi , 988),
                    1112,
                    988,
                    1013,
                    902,
                    ifelse(!(max.sdi<=1482 & max.sdi>=1112), 1359, max.sdi),
                    1482,
                    1976,
                    1729)
}

# volume functions
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
    if(!use.metric){
      ivol<- switch(ineq,
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
    }else{
      txe <- tx*.404686 #convert to Englgish
      v.arraye <- v.array*35.3147*.404686 #Convert volume to English
      ivol<- switch(ineq,
                    NULL,
                    vf2(v.arraye[k], txe),
                    vf3(v.arraye[k], txe),
                    NULL,
                    NULL,
                    vf6(v.arraye[k], txe),
                    vf7(v.arraye[k], txe),
                    NULL,
                    vf9(v.arraye[k], txe))
      iaa  <- 10*((max.sdi*.404686)/txe)^(islp)
      qq   <- sum((iaa-ivol)>0)
    }

    # now draw the volume iso line
    if(!use.metric){
      graphics::lines(tx[1:qq], ivol[1:qq], type="l", col=vcol, lwd=0.5, lty=vty )
    }else{
      graphics::lines(tx[1:qq], ivol[1:qq]*2.54, type="l", col=vcol, lwd=0.5, lty=vty )
    }
    # now annotate the volume
    if(k==length(v.array)){

      if(v.ann){
        if(!use.metric){
          graphics::text(range.x[2]*1.15, ivol[qq]*1.18,  "Volume", cex=vcex, col=vcol)
          graphics::text(range.x[2]*1.15, ivol[qq]*1.09,
                       expression("ft"^3~"acre"^-1), cex=vcex, col=vcol)
        }else{
          graphics::text(range.x[2]*1.15, 2.54*ivol[qq]*1.18,  "Volume", cex=vcex, col=vcol)
          graphics::text(range.x[2]*1.15, 2.54*ivol[qq]*1.09,
                         expression("m"^3~"ha"^-1), cex=vcex, col=vcol)
        }
      }
    }
    if(v.ann){
      if(!use.metric){
        graphics::text(range.x[2]*1.15,
                       ivol[qq], as.character(v.array[k]), cex=vcex, col=vcol)
      }else{
        graphics::text(range.x[2]*1.15,
                       2.54*ivol[qq], as.character(v.array[k]), cex=vcex, col=vcol)
      }
    }
    # now draw a segment linking the line and annotation if needed
    if(min( iaa ) < ivol[qq] ){
      if(!use.metric){
        graphics::segments(tx[qq],   ivol[qq], range.x[2]*0.98,
                         ivol[qq],  col=vcol, lwd=0.5,   lty=1)
      }else{
        graphics::segments(tx[qq],   2.54*ivol[qq],
                           range.x[2]*0.98,
                           2.54*ivol[qq],
                           col=vcol, lwd=0.5,   lty=1)
      }
    }
  }
}

return()

}
