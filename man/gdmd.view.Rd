\name{gdmd.view}
\alias{gdmd.view}
\title{
Gingrich Density Mangement Diagram generator
}
\description{
This package provides function(s) to generate customizable stand density management diagrams
  using Gingrich (1967) space. Note that this figure is designed for a 8x6 aspect ratio in contrast
  to the 6x8 ratio for dmd.view in translated Reineke space (Reineke 1933).
}
\usage{
gdmd.view(ineq  = 1,
          inul  = TRUE,
          inrd = TRUE,
          rdlabel = TRUE,
          inply = TRUE,
          inqmd = TRUE,
          inspace=TRUE,
          max.sdi=400,
          umz=250,
          lmz=100,
          mgt.zone = c(0.35,0.60),
          reineke.term=1.6,
          bsi=90,
          dcol = "blue",
          rdcol= "black",
          dmd.title    = " ")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ineq}{
Selects limiting SDI and other parameters. This must be an integer value from 1 to 9.
    Value of 1 generates a generic DMD space, no published volumes.

    Value is 2 for Long and Shaw (2005) Ponderosa pine equations,

    Value of 3 for Ritchie and Zhang (unpublished) Ponderosa pine equations.

    Value of 4 for Edminster (1988) Ponderosa pine.

    Value of 5 for Cochran and Barrett (1992) Ponderosa pine.

    Value of 6 for Long and Shaw (2012) California mixed-conifer.

    Value of 7 for Long et al. (1988) Douglas-fir.

    Value of 8 for Zhang et al. (2007) White fir.

    Value of 9 for McCarter and Long (1986) Lodgepole pine.

    Default=1.
}
  \item{inul}{
Selects to add annotation for the upper limit, including slope and umz. Default=TRUE.
}
  \item{inrd}{
Selects intermediate sdi lines to be drawn. The placement of these lines is
    determined by the values in sdi.lines and mgt.zone, Default=TRUE.
}
  \item{rdlabel}{
Selects annotation for relative densities, Default is TRUE.
}
  \item{inply}{
Selects for presentation of a management zone polygon. Default=TRUE.
}
  \item{inqmd}{
Selects for presentation qmd labels. Default=TRUE.
}
  \item{inspace}{
Selects for presentation of square spacing guide on x-axis. Default=TRUE.
}
  \item{max.sdi}{
Defines upper limit of stand density index (maximum sdi).
  This is only used for ineq=1. Default is 400.
}
  \item{umz}{
Defines upper limit of management zone, only used when mgt.zone[2] is NA.
}
  \item{lmz}{
Defines lower limit of management zone, only used when mgt.zone[1] is NA.
}
  \item{mgt.zone}{
Defines upper and lower limit of management zone for generic diagram only as
    prop. of max. sdi. These value will override values in umz and lmz. vector
    of length 2; mgt.zone[1] is proportion of sdi.max for lower limit;
    mgt.zone[2] is the proportion of sdi.max for upper limit.
}
  \item{reineke.term}{
This is the inverse of the slope of sdi lines in sdi space. Default=1.605,
  the value derived by Reineke (1933).
}
  \item{bsi}{
This is Barrett's (1978) site index for use in Cochrans (1992)diagram only;
  it sets the upper mgt. zone limit.
}
  \item{dcol}{
This is the color for the diameter lines: default is blue. Must be a color
  from the r color pallette.
}
  \item{rdcol}{
This is the color for the relative density lines: default is black. Must be a color from the r colorpallette.
}
  \item{dmd.title}{
This is the title for the management zone: default is lightgrey. Must be a color from the r colorpallette.
}

}
\value{
Returns error code e.code; value of zero indicates success in rendering figure.
}
\references{
    Barrett, J.W. (1978) Height growth and site index curves for managed
      even-aged stands of ponderosa pine in the Pacific Northwest. Res.
      Pap. PNW-232. USDA For. Serv. Pac. Northwest For. and Range Exp. Stn. 14p.

    Drew, T.J. and J.W. Flewelling (1979). Stand density management: an alternative
      approach and its application to Douglas-fir plantations. For. Sci. 25(3):518-532

    Cochran, P.H. (1992) Stocking Levels and Underlying Assumptions for Uneven-Aged
      Ponderosa Pine Stands. Res. Note PNW-509. USDA For. Serv. Pac. Northwest For.
      and Range Exp. Stn. 10p

    Gingrich, S. (1967) Measuring and evaluating stocking and stand density in upland
      hardwood forests in the central states. For. Sci. 13(1):38-53.

    Long, J.N. and J.D. Shaw (2005) A density management diagram for even-aged ponderosa
      pine stands. West. J. Appl. For. 20(4):205-215.

    Reineke, L.H. (1933) Perfecting a stand density index for even-aged forests.
      J. Ag. Res. 46:627-638.
}
\author{
Martin W. Ritchie, Research Forester, US Forest Service, Pacific Southwest Research Station.
  Email: \email{mritchie@fs.fed.us}
}
\note{
It is important to note that the gdmd.view function is designed to render a 8x4 inch canvas (pdf).
    Any other size than that and the annotations and axis labels may not place correctly.
    For this reason, it is suggested that one open a pdf with the function call:
    pdf("myfile.pdf", width=8, height=4) prior to invoking the gdmd.view function. When the figure
    is completely rendered, then one must close the device with dev.off().
    See Examples below for how this can be done.
}

\examples{
# 1. This example will produce a generic density mgt space in a file called myplot1.pdf:

#setwd("c:/mydir") # optional, to specify location (directory) for file input and output

pdf("myplotg1.pdf", width=8, height=6) # optional, open a pdf file called myplot0.pdf
opar <- par(mar = c(3.0,2.0,2.5,2.5)) # set some reasonable margins
# build a dmd for Long and Shaw Ponderosa pine

gdmd.view(ineq=2)

par(opar)  # reset margins to default settings
dev.off()  # close the pdf file

# 2. This example shows a plot of data from gama05 for treated plots
#       and calls for volume calculation.

pdf("myplotg2.pdf", width=8, height=6)
opar <- par(mar = c(3.0,2.0,2.5,2.5))

gdmd.view(ineq=3 ) # call dmd.view and request volume iso lines.

points(x=gama05[!gama05$treatment=="Control",]$tpa,
       y=gama05[!gama05$treatment=="Control",]$ba,
       cex=0.75, pch=21, col="black")

par(opar)
dev.off()


}
\keyword{ color }
\keyword{ device }
