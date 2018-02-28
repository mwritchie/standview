\name{dmd.volume}
\alias{dmd.volume}
\title{
Density Management Volume Estimation
}
\description{
This package provides functions to generate volumes and dominant height for stand density management diagrams.

Typically in a DMD the volume is roughly interpolated between plotted lines. With dmd.volume
  the user can specify qmd and tpa values as input to generate estimated volumes from published
  volume equations
}
\usage{
dmd.volume(ineq  = 1,
           max.sdi=600,
           tpa=NA,
           qmd=NA,
           ba=NA)
}
\arguments{
  \item{ineq}{
Selects volume and dominant height equations. This must be an integer value  2, 3, 6, 7, or 9.

    Value is 2 for Long and Shaw (2005) Ponderosa pine equations,

    Value of 3 for Ritchie and Zhang (unpublished) Ponderosa pine equations.

    Value of 6 for Long and Shaw (2012) California mixed-conifer

    Value of 7 for Drew and Flewelling (1979); Long et al. (1988).

    Value of 9 for McCarter and Long (1986).

    Default=1.
}
  \item{max.sdi}{
Input of limiting stand density index; default=600.
}
  \item{tpa}{
Input of stand trees per acre as a vector of values. Function requires two of three stand descriptors (tpa, qmd, ba)
}
  \item{qmd}{
Input of stand average diameter (inches). Function requires two of three stand descriptors (tpa, qmd, ba)
}
  \item{ba}{
Input of stand basal area (square feet per acre). Function requires two of three stand descriptors (tpa, qmd, ba).
}

}
\value{
Returns a data frame with tpa, qmd, ba, volume (cubic feet per acre), stand height (feet) and (if available) above-ground biomass (pounds       per acre).
}
\references{
    Drew, T.J. and J.W. Flewelling (1979). Stand density management: an alternative
      approach and its application to Douglas-fir plantations. For. Sci. 25(3):518-532

    Long, J.N., J.B. McCarter, and S.B. Jack. (1988) A modified density management
      diagram for coastal Douglas-fir. West. J. Appl. For. 3(3):88-89.

    Long, J.N. and J.D. Shaw. (2005) A density management diagram for even-aged
      ponderosa pine stands. West. J. Appl. For. 20(4):205-215.

    Long, J.N. J.D. Shaw, J.D. (2012) A density management diagram for even-aged
      Sierra Nevada mixed-conifer Stands. West. J. Appl. For. 27(4):187-195.

    McCarter, J.D. and J.N. Long (1986) A logdepole pine density management diagram.
      West. J. Appl. For 1:6-11.

    Reineke, L.H. (1933) Perfecting a stand density index for even-aged forests.
      J. Ag. Res. 46:627-638.
}
\author{
Martin W. Ritchie, Research Forester, US Forest Service, Pacific Southwest Research Station.
  Email: \email{mritchie@fs.fed.us}
}
\note{
This function returns a dataframe that could represent observations for a stand prescription over time.
    The observations could also be for sampled independent stands. The species for the observations
    should match the species identified by the value ineq. So if ineq=2 is chosen, then the volumes
    returned will be from Long and Shaw (2005) for ponderosa pine.
    Vectors passed in must match in length.
    See Examples below.
}

\examples{
# 1. This example generates a prescription and plots over Long and Shaw (2005)

library(standview)
DF<-dmd.volume(ineq=2, qmd=c(2, 2.5, 14, 15.5, 19), tpa=c(300, 150, 140, 80, 80))
DF
pdf("myplotv1.pdf", width=6, height=8)
opar <- par(mar = c(3.0,2.0,2.5,2.5))

dmd.view(ineq=2 ) # call dmd.view.

points(x=DF$tpa, y=DF$qmd, cex=0.75, pch=21, col="black")
segments(300, 1, DF[1,]$tpa, DF[1,]$qmd, lwd=2)
for(i in 1:4){
  segments(DF$tpa[i], DF$qmd[i], DF$tpa[i+1], DF$qmd[i+1], lwd=2)
}
arrows(280, 19, 120, 14.8, length=0.10 )
text(300, 22, "Commercial thin")
text(300, 20, paste(round(DF$vol[3]-DF$vol[4]), "c.f. removed"))

par(opar)
dev.off()


# 2. Obtaining volume estimates for a file using gama05 data example
mygama<-gama05  # make a copy of the data file provided with standview package
DF<-dmd.volume(ineq=3, qmd=mygama$qmd, tpa=mygama$tpa) # generate volume and dom. ht.
# append values to the dataframe.
mygama$volume <- DF$volume
mygama$height <- DF$height

}

