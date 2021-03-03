\name{dmd.volume}
\alias{dmd.volume}
\title{
Density Management Volume Estimation
}
\description{
This function provides estimates for volume and dominant height for stand density management diagrams, where available.

Typically, in a DMD, the volume is roughly interpolated between plotted iso-volume lines.
With dmd.volume, the user can specify qmd and tpa values as input to generate estimated volumes
from published volume equations.
}
\usage{
dmd.volume(ineq  = 2,
           max.sdi=NULL,
           tpa=NULL,
           qmd=NULL,
           ba=NULL,
           use.metric=FALSE)
}
\arguments{
  \item{ineq}{
Selects supplimental equations. This must be an integer value  2, 3, 6, 7, or 9.

    Value is 2 for Long and Shaw (2005) Ponderosa pine equations,

    Value of 3 for Jang et al. (2021) Ponderosa pine equations.

    Value of 6 for Long and Shaw (2012) California mixed-conifer

    Value of 7 for Drew and Flewelling (1979); Long et al. (1988).

    Value of 9 for McCarter and Long (1986).

    Default=3.
}
  \item{max.sdi}{
Input of limiting stand density index (units are trees per acre for use.metric=FALSE or
trees per hectare for use.metric=TRUE); default=NULL. This value must be supplied for ineq=7.
}
  \item{tpa}{
Input of stand trees per unit area as a vector of values.
Function requires two of three stand descriptors (tpa, qmd, ba).
Units are trees per acre for use.metric=FALSE and trees per hectare for use.metric=TRUE.
}
  \item{qmd}{
Input of stand average diameter (inches for use.metric=FALSE or cm for use.metric=TRUE).
Function requires two of three stand descriptors (tpa, qmd, ba)
}
  \item{ba}{
Input of stand basal area (square feet per acre for use.metric=FALSE or square meters per
hectare for use.metric=TRUE). Function requires two of three stand descriptors (tpa, qmd, ba).
}
  \item{use.metric}{
Use metric units (logical), default= FALSE.
}

}
\value{
Returns a data frame with trees per unit area, quadratic mean diameter, basal area per unit area. Additionally
the data may contain volume, volume standar error, height, height standar error, biomass, biomass standard error
crown closure percent and crown closure percent standard error, if functions are available for these metrics.
These supplimental metrics are not available for all values of ineq. If unavailable the output is NA (missing).
Units are metric if use.metric=TRUE. The metric units are:
stems per hectare, centimeters, square meters per hectare, cubic meters per hectare, and meters respectively).
Units are English if use.metric=FALSE (stems per acre, inches, square feet per acre, cubic feet per acre, and feet
respectively). Note that standard error estimates are only available for ineq=3; other published equation sets do
not include sufficient information for this. The standard error of estimates from ineq=3 is calculated by means of
the delta method (manuscript in preparation).
}
\references{
    Drew, T.J. and J.W. Flewelling (1979). Stand density management: an alternative
      approach and its application to Douglas-fir plantations. For. Sci. 25(3):518-532

    Jang, W.S., M.W. Ritchie and J. Zhang (2021) Improved equations for the density management
      diagram isolines of ponderosa pine stands. For. Sci. 67(1):93-102

    Long, J.N., J.B. McCarter, and S.B. Jack. (1988) A modified density management
      diagram for coastal Douglas-fir. West. J. Appl. For. 3(3):88-89.

    Long, J.N. and J.D. Shaw. (2005) A density management diagram for even-aged
      ponderosa pine stands. West. J. Appl. For. 20(4):205-215.

    Long, J.N. and J.D. Shaw. (2012) A density management diagram for even-aged
      Sierra Nevada mixed-conifer Stands. West. J. Appl. For. 27(4):187-195.

    McCarter, J.D. and J.N. Long (1986) A logdepole pine density management diagram.
      West. J. Appl. For 1(1):6-11.

    Reineke, L.H. (1933) Perfecting a stand density index for even-aged forests.
      J. Ag. Res. 46:627-638.
}
\author{
Martin W. Ritchie, Research Forester, US Forest Service, Pacific Southwest Research Station.
  Email: \email{martin.ritchie@usda.gov}
}
\note{
This function returns a dataframe that represents multiple observations; these could be a single chronosequence.
    The observations could also be for sampled independent stands. The species for the observations
    should match the species identified by the value ineq. So if ineq=2 is chosen, then the volumes
    returned will be from Long and Shaw (2005) for ponderosa pine. Currently biomass and crown cover percent
    are produced only for ineq=3. Note that it is assumed that the units of tpa and qmd match the specification
    of use.metric. If use.metric=TRUE then it is assumed qmd is cm and tpa=trees per ha.
    If use.metric=FALSE, then qmd is inches and tpa=trees per acre. There is no provision for using
    English units to derive metric output or vice-versa.
    Vectors passed in must match in length.
    See Examples below.
}

\examples{
# 1. This example generates a prescription and plots over Long and Shaw (2005)

library(standview)
DF<-dmd.volume(ineq=2,
               qmd=c(2, 2.5, 14, 15.5, 19),
               tpa=c(300, 150, 140, 80, 80),
               use.metric=FALSE)
DF
#pdf("myplotv1.pdf", width=6, height=8) # recommended to set a 6x8 canvas
opar <- par(mar = c(3.0,2.0,2.5,2.5))

dmd.view(ineq=2, use.metric=FALSE ) # call dmd.view.

points(x=DF$tpa.ac, y=DF$qmd.in, cex=0.75, pch=21, col="black")
segments(300, 1, DF[1,]$tpa.ac, DF[1,]$qmd.in, lwd=2)
for(i in 1:4){
  segments(DF$tpa.ac[i], DF$qmd.in[i], DF$tpa.ac[i+1], DF$qmd.in[i+1], lwd=2)
}
arrows(280, 19, 120, 14.8, length=0.10 )
text(300, 22, "Commercial thin")
text(300, 20, paste(round(DF$volume.ft3ac[3]-DF$volume.ft3ac[4]), "cu.ft./ac removed"))

par(opar)
#dev.off()


# 2. Obtaining volume estimates for a file using gama05 data example
mygama<-gama05  # make a copy of the data file provided with standview package
DF<-dmd.volume(ineq=3,
               qmd=mygama$qmd,
               tpa=mygama$tpa,
               use.metric=FALSE) # generate volume and dom. ht.
# add values to the dataframe.
mygama$volume <- DF$volume
mygama$height <- DF$height

}

