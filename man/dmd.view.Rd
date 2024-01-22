\name{dmd.view}
\alias{dmd.view}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Density Mangement Diagram generator
}
\description{
This package provides function(s) to generate customizable stand density management diagrams
  in translated Reineke space, with log of qmd on the y-axis and log of tpa on the x-axis.
}
\usage{
dmd.view(ineq = 3,
    inul = TRUE,
    insdi = TRUE,
    inply = TRUE,
    insdr = FALSE,
    insdl = FALSE,
    max.sdi=NA,
    dmd.title=" ",
    sdi.lines=NA,
    mgt.zone=c(0.35,0.60),
    reineke.term=1.605,
    bsi=90,
    mzcol="grey",
    sdicol="red",
    invol=FALSE,
    vcol="blue",
    use.metric=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ineq}{
Selects the values from a published DMD source (ineq=1-10) or a generic
  DMD with user-specified thresholds as well as published DMD volume and
  dominant height equations if available. This must be an integer value from 1 to 10.
  Default value is 3.

    Value of 1 generates a generic DMD space, no published volumes.

    Value is 2 for Long and Shaw (2005) ponderosa pine equations,

    Value of 3 for Jang et al. (2021) ponderosa pine equations.

    Value of 4 for Edminster (1988) ponderosa pine.

    Value of 5 for Cochran and Barrett (1992) ponderosa pine.

    Value of 6 for Long and Shaw (2012) California mixed-conifer.

    Value of 7 for Long et al. (1988) Douglas-fir.

    Value of 8 for Zhang et al. (2007) white fir.

    Value of 9 for McCarter and Long (1986) lodgepole pine.

    Value of 10 for Weiskittel and Woodall (2023). Maine spruce-fir

    Value of 11 for Ritchie and Berrill (2022) redwood/Douglas-fir

    Value of 12 for Ritchie and Berrill (2022) Douglas-fir/redwood


    Default=3.
}
  \item{inul}{
Selects to add label for the upper limit to the sdi line. Default=TRUE.
}
  \item{insdi}{
Selects intermediate sdi lines to be drawn. The placement of these lines is
    determined by the values in sdi.lines and mgt.zone, Default=TRUE.
}
  \item{inply}{
Selects for presentation of a management zone polygon. Boundaries for the
    management zone are determined by the values in sdi.lines and mgt.zone.
    Default=TRUE.
}
  \item{insdr}{
Selects for annotation of sdi lines on right of graph. Default=FALSE. Note that selecting true for this variable makes no
  sense unless insdi=TRUE.
}
  \item{insdl}{
Selects for annotation of sdi lines on left of graph. Default=FALSE. Note that selecting true for this variable makes no
  sense unless insdi=TRUE. Note that if the sdi.lines settings place lines too close together, these annotations may overlap

}
  \item{max.sdi}{
Defines upper limit of stand density index (maximum sdi). There is no default limit to SDI. This value is set
  by the selection of ineq. If ineq=1, 6, 10, 11 or 12 the user must specify a value. If use.metric=TRUE this value must be in
  metric units. Currently ineq=1 (generic DMD), ineq=6 (Long and Shaw 2012), ineq=10 (Weiskittel and Woodall 2023),
  ineq=11 or ineq=12 (Ritchie and Berrill 2022) are the only DMD options where the
  user must actually specify this value. For most published DMDs the upper limit is fixed by the author and cannot be changed
  by user over-ride; in such cases, any user-specified upper limit will be ignored and the published value will be used to
  render the DMD.
  A value of ineq=1, requires English-unit max.sdi to be from 300-1000 (inclusive).
  A value of ineq=6, requires English-unit max.sdi to be from 450-600 (inclusive).
  A value of ineq=10 requires English-unit max.sdi to be from 527-564 (inclusive).
  A value of ineq=11 requires English-unit max.sdi to be from 750-1000 (inclusive).
  A value of ineq=12 requires English-unit max.sdi to be from 550-700 (inclusive).

}
  \item{dmd.title}{
Character string for title to be displayed. As an example could take the form of dmd.title="Ponderosa Pine".
}
  \item{sdi.lines}{
An array of values for plotting sdi iso-lines in the density management diagram. There is no default
    value for ineq=1. Note that, as with max.sdi, the sdi.lines are fixed
    unless a generic DMD (ineq=1) is chosen. If the user selects to make a generic DMD in metric
    units, then this argument must be set to metric units.
}
  \item{mgt.zone}{
Explicitly sets the upper and lower limits of the management zone by proportion of the max.sdi.
    By defalut this is set to mgt.zone=c(0.35, 0.60). As such this will overwrite the 3rd and 5th
    elements in the sdi.lines array. If the User wishes to use fixed values for the management zone using
    the argument sdi.lines, then set mgt.zone=c(NA, NA). Acceptable Values for mgt.zone are 0.2 to 0.4 for
    lower limit (mgt.zone[1]), and 0.5 to 0.7 for upper limit (mgt.zone[2]).
}
  \item{reineke.term}{
This is the inverse of the slope of sdi lines in sdi space. Default=1.605, the value derived by Reineke (1933).
    Limits are 1.3 and 2.0.
}
  \item{bsi}{
This is Barrett's site index (Barrett 1978) for use in the Cochran (1992) pine DMD (ineq=5). The default units
  for this argument is feet. If use.metric=TRUE is selected then the value for bsi should be reset by the user
  to meters.
}
  \item{mzcol}{
  Specifies color for management zone. Default="grey".
}
  \item{sdicol}{
Specifies color for stand density index lines. Default="red".
}
  \item{invol}{
Draws iso-volume lines. Default=FALSE.
}
  \item{vcol}{
Specifies color for volume iso-lines. Default="blue".
}
  \item{use.metric}{
Specifies use of metric, logical. Default=FALSE.
}

}
\value{
Returns no value.
}
\references{
    Barrett, J.W. (1978) Height growth and site index curves for managed
    even-aged stands of ponderosa pine in the Pacific Northwest. Res.
    Pap. PNW-232. USDA For. Serv. Pac. Northwest For. and Range Exp. Stn. 14p.

    Drew, T.J. and J.W. Flewelling (1979). Stand density management: an alternative
    approach and its application to Douglas-fir plantations. For. Sci. 25(3):518-532

    Cochran, P.H. and J.W. Barrett (1992) Stocking levels and underlying assumptions for
    uneven-aged ponderosa pine stands. Res. Note PNW-509. USDA For. Serv. Pac. Northwest
    For. and Range Exp. Stn. 10p.

    Jang, W.S., M.W. Ritchie, and J. Zhang (2021) Improved equations for the density management
    diagram isolines of ponderosa pine stands. For. Sci. 67(1):93-102.

    Long, J.N., J.B. McCarter, and S.B. Jack (1988) A modified density management
    diagram for coastal Douglas-fir. West. J. Appl. For. 3(3):88-89.

    Long, J.N. and J.D. Shaw (2005) A density management diagram for even-aged
    ponderosa pine stands. West. J. Appl. For. 20(4):205-215.

    Long, J.N. and J.D. Shaw (2012) A Density Management Diagram for Even-Aged
    Sierra Nevada Mixed-Conifer Stands. West. J. Appl. For. 27(4):187-195

    McCarter, J.B. and J.N. Long (1986) A lodgepole pine density management diagram.
    West. J. Appl. For. 1(1)6-11.

    Reineke, L.H. (1933) Perfecting a stand density index for even-aged forests.
    J. Ag. Res. 46:627-638.

    Ritchie, M.W. and J.-P. Berrill (2022) Precommercial thinning in California Forests. In:
    Stewart, W. (editor) Reforestation practices for conifers in California. Davis CA:
    University of California, Agriculture and Natural Resources.

    Weiskittel, A. & Woodall, C. (2023) Stand density index and relative density calculator
    for the United States. figshare. Software. https://doi.org/10.6084/m9.figshare.24412246.v1

    Zhang, J., W.W. Oliver and M.W. Ritchie (2007) Effect of stand densities on stand
    dynamics in white fir (Abies concolor) forests in northeast California, USA.
    For. Ecol. and Manage. 244:50-59.

}
\author{
Martin W. Ritchie, Research Forester, US Forest Service, Pacific Southwest Research Station.
  Email: \email{martin.ritchie@usda.gov}
}
\note{
It is important to note that the dmd.view function is designed to render a 6wx8h canvas.
    Any other aspect may produce annotations and axis labels that are misplaced.
    For this reason, it is suggested that one open a pdf device with the function call:
    pdf("myfile.pdf", width=6, height=8) prior to invoking the dmd.view function. When the
    figure is completely rendered, one should always close the device with dev.off().
    See Examples below for how this can be done; to use this in examples, remove the comment
    on the pdf() call and the dev.off(). Also, the reineke.term for Coastal Douglas-fir
    was derived as 1.605, but from Drew and Flewelling there should be a 6 percent adjustment at
    lower densities. If one wishes to account for this a reineke.term of 1.71 is a compromise
    slope to remove some of this apparent error. See Drew and Flewelling (1979), page 521.
Also, some default values will depend on the ineq value selected. Defaults shown here are for ineq=1
    and may change according to selection of ineq to match published data.

}

\examples{
# 1. This example will produce a generic density mgt space in a file called myplot1.pdf:

#setwd("c:/mydir") # optional, to specify location (directory) for file input and output

#pdf("myplotr1.pdf", width=6, height=8) # recommended, open a 6x8 pdf file
opar <- par(mar = c(3.0,2.0,2.5,2.5)) # set some reasonable margins

dmd.view(ineq=1, use.metric=FALSE,
         sdi.lines=c(50, 100, 150, 200, 300, 400),
         max.sdi=420,
         insdr=TRUE,
         dmd.title="DMD with limit at 420")

par(opar)  # reset margins to default settings
#dev.off()  # close the recommended pdf file

# 2. This example will produce density mgt diagram myplot2.pdf :

#setwd("c:/mydir") # optional to select location for read and write

#pdf("myplotr2.pdf", width=6, height=8) # recommended, open a 6x8 pdf file
opar <- par(mar = c(3.0,2.0,2.5,2.5))

dmd.view(ineq=3, invol=TRUE ) # call dmd.view and request volume iso lines.

par(opar)
#dev.off() # close the recommended pdf file

# 3. This example shows a plot of data separating controls from treatments

#pdf("myplotr3.pdf", width=6, height=8) # recommended, open a 6x8 pdf file
opar <- par(mar = c(3.0,2.0,2.5,2.5))

dmd.view(ineq=2 ) # call dmd.view to generate DMD.

gama05$ppch<-21  # create a default character for plot
gama05[gama05$treatment=="Control",]$ppch<-2 # set plotting character for Control
points(x=gama05$tpa,
       y=gama05$qmd,
       cex=0.75, pch=gama05$ppch, col="black") # overlay data points

par(opar)
#dev.off()  # close optional pdf

# 4. Example calculating volumes for prescription, potting and annotations
DF<-dmd.volume(ineq=2, qmd=c(2, 2.5, 14, 15.5, 19), tpa=c(300, 150, 140, 80, 80))
DF

#pdf("myplotv1.pdf", width=6, height=8) # recommended, open a 6x8 pdf file
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
#dev.off()  # close optional pdf

# 5. Example using spruce/fir option

#pdf("myplotsf.pdf", width=6, height=8) # recommended, open a 6x8 pdf file
opar <- par(mar = c(3.0,2.0,2.5,2.5))

dmd.view(ineq=10, max.sdi=590, insdr=TRUE ) # call dmd.view.
points(x=c(700,700,600,400,200),
       y=c(2.5,5.5,6.8,9.0,13.8), pch=21, cex=1, bg="black", col="black")
lines(x=c(700,700,600,400,200),
      y=c(2.5,5.5,6.8,9.0,13.8), lwd=2, col="black")
par(opar)
#dev.off()  # close optional pdf

}
\keyword{ color }
\keyword{ device }
\keyword{ dplot }
