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
dmd.view(ineq = 1, inul = TRUE, insdi = TRUE, inply = TRUE,
    insdr = FALSE,
    insdl = TRUE,
    max.sdi=550,
    dmd.title,
    sdi.lines=c(50, 100, 150, 200, 250, 350),
    mgt.zone=c(0.35,0.60),
    reineke.term=1.605,
    bsi=90,
    sdicol="red",
    invol=FALSE,
    vcol="blue")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ineq}{
Selects the values from a published DMD source (ineq=1-9) or a generic
DMD with user-specified thresholds as well as published DMD volume and
dominant height equations if available. This must be an integer value from 1 to 9.

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
Defines upper limit of stand density index (maximum sdi). Default is 450.
}
  \item{dmd.title}{
Character string for title to be displayed. As an example could take the form of dmd.title="Ponderosa Pine".
}
  \item{sdi.lines}{
An array of values for plotting sdi iso-lines in the density management diagram. Default value is
    sdi.lines=c(50, 100, 150, 200, 250, 350).
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
This is Barrett's site index (Barrett 1978) for use in the Cochran (1992) pine DMD.
}
  \item{sdicol}{
Specifies color for stand density index lines. Default="red".
}
  \item{invol}{
Draws iso volume lines. Default=FALSE.
}
  \item{vcol}{
Specifies color for volume iso-lines. Default="blue".
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

pdf("myplot1.pdf", width=6, height=8) # optional, open a pdf file called myplot1.pdf
opar <- par(mar = c(3.0,2.0,2.5,2.5)) # set some reasonable margins

dmd.view() # When called with no arguments, displays generic space with default values

par(opar)  # reset margins to default settings
dev.off()  # close the pdf file

# 2. This example will produce density mgt diagram myplot2.pdf :

#setwd("c:/mydir") # optional to select location for read and write

pdf("myplot2.pdf", width=6, height=8)
opar <- par(mar = c(3.0,2.0,2.5,2.5))

dmd.view(ineq=2 ) # call dmd.view and request volume iso lines.

par(opar)
dev.off()

pdf("myplot3.pdf", width=6, height=8)
opar <- par(mar = c(3.0,2.0,2.5,2.5))

dmd.view(ineq=3 ) # call dmd.view and request volume iso lines & tabluar output.

par(opar)
dev.off()

}
\keyword{ color }
\keyword{ device }
\keyword{ dplot }
