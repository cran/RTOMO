`FANCY.TOMO` <-
function(MOD, i, COL=NULL, LIM=NULL, MAP=NULL, MAPLIM=NULL,
                     STA=NULL, staparams=list(col='green', pch=6, cex=.8, name=FALSE),
                     PTS=NULL, ptsparams=list(col='green', pch=6, cex=.8, name=FALSE),
                     TIT="Layer", UNITS="", bkgr="DarkSlateGray4" )
  {

    

    if(missing(COL)) { COL=tomo.colors(100) }
    if(missing(LIM)) { LIM=NULL }
    if(missing(MAP)) { MAP=NULL }
    if(missing(MAPLIM)) { MAPLIM=NULL }
    
    if(missing(STA)) { STA=NULL }
    if(missing(staparams)) { staparams=list(col='green', pch=6, cex=.8, name=FALSE) }

    if(missing(PTS)) { PTS=NULL }
    if(missing(ptsparams)) { ptsparams=list(col='green', pch=6, cex=.8, name=FALSE) }

   
    if(missing(UNITS)) { UNITS="%" }
    if(missing(bkgr)) { bkgr="DarkSlateGray4" }
    
    if(missing(TIT)) { TIT= paste(sep=' ', "LAYER=", i, "Depth", MOD$D[i], "-", MOD$D[i+1]) }

    if(is.null(LIM)) { LIM= range(MOD$MOD[[i]], na.rm=TRUE) } 
    
    pltomo(MOD$x,MOD$y,MOD$MOD,i, COL, zlim=LIM, bkgr=bkgr, xlab="km", ylab="km")
    HOZscale(LIM , col=COL  , units=UNITS, SIDE=1)
    if(require(GEOmap)==TRUE)
      {
        
   
    
### HOZscale(MOD$MOD[[i]] , col=tomocolors  , units="%", SIDE=2)
        
        
###  image.SCALE(MOD$MOD[[i]] , col=rainbow(100), nlab=2)	
        ## PROJmap(JAPmap,  ADD=TRUE, COL=TRUE)
        if(!is.null(MAP))
          {
            ##  plotGEOmapXY(MAP, LIM=MAPLIM   , PROJ =MAP$PROJ ,   add=TRUE)
            plotGEOmapXY(MAP, PROJ=MAP$PROJ, add=TRUE, xpd=FALSE )
          }
        if(!is.null(STA))
          {
            if(is.null(STA$FLAG)) { sflag = rep(TRUE, length(STA$x))} else { sflag = STA$FLAG }
            
            pointsGEOmapXY(STA$lat[sflag], STA$lon[sflag], PROJ=MAP$PROJ, pch=staparams$pch, col=staparams$col, cex=staparams$cex)
            
            if(identical(staparams$name, TRUE))
              {
                
                textGEOmapXY(STA$lat[sflag], STA$lon[sflag], labels=STA$name[sflag], PROJ=MAP$PROJ, pos=3, cex=staparams$cex)
              }
            
            
            
          }
        
        if(!is.null(PTS))
          {
            if(is.null(PTS$FLAG)) { sflag = rep(TRUE, length(PTS$lat))} else { sflag = PTS$FLAG }
            pointsGEOmapXY(PTS$lat[sflag], PTS$lon[sflag], PROJ=MAP$PROJ, pch=ptsparams$pch, col=ptsparams$col, cex=ptsparams$cex)
            
            if(identical(ptsparams$name, TRUE))
              {
                
                textGEOmapXY(PTS$lat[sflag], PTS$lon[sflag], PROJ=MAP$PROJ, PTS$name[sflag], pos=3, cex=ptsparams$cex)
              }
        
          }
        
      }
    
    
    ##	text(STA$x[STAFLAG], STA$y[STAFLAG], labels=STA$nam[STAFLAG], pos=3, col='green', cex=.65)
    ## print(TIT)
    
    if(!is.null(TIT))
      {
        title(main=TIT, sub=TIT)
      }
    
    
  }

