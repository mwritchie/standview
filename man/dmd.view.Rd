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
    insdl = TRUE,
    max.sdi=NA,
    dmd.title=" ",
    sdi.lines=NA,
    mgt.zone=c(0.35,0.60),
    reineke.term=1.605,
    bsi=90,
    sdicol="red",
    invol=FALSE,
    vcol="blue",
    use.metric=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ineq}{
Selects the values from a published DMD source (ineq=1-9) or a generic
  DMD with user-specified thresholds as well as published DMD volume and
  dominant height equations if available. This must be an integer value from 1 to 9.
  Default value is 3.

    Value of 1 generates a generic DMD space, no published volumes.

    Value is 2 for Long and Shaw (2005) ponderosa pine equations,

    Value of 3 for Ritchie and Zhang (unpublished) ponderosa pine equations.

    Value of 4 for Edminster (1988) ponderosa pine.

    Value of 5 for Cochran and Barrett (1992) ponderosa pine.

    Value of 6 for Long and Shaw (2012) California mixed-conifer.

    Value of 7 for Long et al. (1988) Douglas-fir.

    Value of 8 for Zhang et al. (2007) white fir.

    Value of 9 for McCarter and Long (1986) lodgepole pine.
    Default=1.
}
  \item{inul}{
Selects to add label for the upper limit to the sdi line. Default=TRUE.
}
  \item{insdi}{
Selects intermediate sdi lines to be drawn. The placement of these lines is
    determined by the values in sdi.lines and mgt.zone, Default=TRUE.
}
  \item{inply}{
Selects for presentation of a management zone polygon. Boundaries for the management
    management zone are determined by the values in sdi.lines and mgt.zone.
    Default=TRUE.
}
  \item{insdr}{
Selects for annotation of sdi lines on right of graph. Default=FALSE. Note that selecting true for this variable makes no sense unless insdi=TRUE.
}
  \item{insdl}{
Selects for annotation of sdi lines on left of graph. Default=FALSE. Note that selecting true for this variable makes no sense unless insdi=TRUE.
}
  \item{max.sdi}{
Defines upper limit of stand density index (maximum sdi). There is no default limit to SDI. This value is set
  by the selection of ineq. If ineq=1, or ineq=6, the user must specify a value. If use.metric=TRUE this value must be in
  metric units. Currently ineq=1 (generic DMD) and ineq=6 (Long and Shaw 2012) are the only DMD options where the
  user may actually specify this value. For most published DMDs the upper limit is fixed by the author and cannot be changed
  by user over-ride; in such cases, any user-specified upper limit will be ignored and the published value will be used to render the DMD.
}
  \item{dmd.title}{
Character string for title to be displayed. As an example could take the form of dmd.title="Ponderosa Pine".
}
  \item{sdi.lines}{
An array of values for plotting sdi iso-lines in the density management diagram. There is no default
    value in English units (trees per acre). Note that, as with max.sdi, the sdi.lines are fixed
    unless a generic DMD (ineq=1) is chosen. If the user selects to make a generic DMD in metric
    units, then this argument must be set to metric units.
}
  \item{mgt.zone}{
Explicitly sets the upper and lower limits of the management zone by proportion of the max.sdi.
    By defalut this is set to mgt.zone=c(0.35, 0.60). As such this will overwrite the 3rd and 5th
    elements in the sdi.lines array. If the User wishes to use fixed values for the management zone using
    the argument sdi.lines, then set mgt.zone=c(NA, NA). Acceptable Values for mgt.zone are 0.2 to 0.4 for
    mgt.zone[1], and 0.5 to 0.7 for mgt.zone[2].
}
  \item{reineke.term}{
This is the inverse of the slope of sdi lines in sdi space. Default=1.605, the value derived by Reineke (1933).
}
  \item{bsi}{
This is Barrett's site index (Barrett 1978) for use in the Cochran (1992) pine DMD (ineq=5). The default units
  for this argument is feet. If use.metric=TRUE is selected then the value for bsi should be reset by the user
  to meters.
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
Specifies use of metric, logical. Default=FALSE. Note this doesn't work yet
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

    Long, J.N., J.B. McCarter, and S.B. Jack. (1988) A modified density management
    diagram for coastal Douglas-fir. West. J. Appl. For. 3(3):88-89.

    Long, J.N. and Shaw, J.D. (2005) A density management diagram for even-aged
    ponderosa pine stands. West. J. Appl. For. 20(4):205-215.

    Long, J.N. and Shaw, J.D. (2012) A Density Management Diagram for Even-Aged
    Sierra Nevada Mixed-Conifer Stands. West. J. Appl. For. 27(4):187-195

    Reineke, L.H. (1933) Perfecting a stand density index for even-aged forests.
    J. Ag. Res. 46:627-638.

    Zhang, J., W.W. Oliver and M.W. Ritchie (2007) Effect of stand densities on stand
    dynamics in white fir (Abies concolor) forests in northeast California, USA.
    For. Ecol. and Manage. 244:50-59.

}
\author{
Martin W. Ritchie, Research Forester, US Forest Service, Pacific Southwest Research Station.
}
\note{
It is important to note that the dmd.view function is designed to render a 6wx8h canvas (pdf).
    Any other aspect ratio than that and the annotations and axis labels may not place well.
    For this reason, it is suggested that one open a pdf device with the function call:
    pdf("myfile.pdf", width=6, height=8) prior to invoking the dmd.view function. When the
    figure is completely rendered, then one must close the device with dev.off().
    See Examples below for how this can be done. Also, the reineke.term for Coastal Douglas-fir
    was derived as 1.605, but from Drew and Flewelling there should be a 6 percent adjustment at
    lower densities. If one wishes to account for this a reineke.term of 1.71 is a compromise
    slope to remove some of this apparent error. See Drew and Flewelling (1979), page 521.
}

\examples{
# 1. This example will produce a generic density mgt space in a file called myplot1.pdf:

#setwd("c:/mydir") # optional, to specify location (directory) for file input and output

pdf("myplotr1.pdf", width=6, height=8) # optional, open a pdf file called myplot1.pdf
opar <- par(mar = c(3.0,2.0,2.5,2.5)) # set some reasonable margins

dmd.view(ineq=1, use.metric=FALSE,
         sdi.lines=c(50, 100, 150, 200, 300, 400),
         max.sdi=420,
         insdr=TRUE,
         dmd.title="DMD with limit at 420")

par(opar)  # reset margins to default settings
dev.off()  # close the pdf file

# 2. This example will produce density mgt diagram myplot2.pdf :

#setwd("c:/mydir") # optional to select location for read and write

pdf("myplotr2.pdf", width=6, height=8)
opar <- par(mar = c(3.0,2.0,2.5,2.5))

dmd.view(ineq=3, invol=TRUE ) # call dmd.view and request volume iso lines.

par(opar)
dev.off()

# 3. This example shows a plot of data separating controls from treatments

pdf("myplotr3.pdf", width=6, height=8)
opar <- par(mar = c(3.0,2.0,2.5,2.5))

dmd.view(ineq=2 ) # call dmd.view to generate DMD.

gama05$ppch<-21  # create a default character for plot
gama05[gama05$treatment=="Control",]$ppch<-2 # set plotting character for Control
points(x=gama05$tpa,
       y=gama05$qmd,
       cex=0.75, pch=gama05$ppch, col="black") # overlay data points

par(opar)
dev.off()

# 4. Example calculating volumes for prescription, potting and annotations
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

}
\keyword{ color }
\keyword{ device }
\keyword{ dplot }
