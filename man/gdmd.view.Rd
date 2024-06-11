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
gdmd.view(ineq  = 3,
          inul  = TRUE,
          inrd = TRUE,
          rdlabel = TRUE,
          inply = TRUE,
          inqmd = TRUE,
          inspace=TRUE,
          max.sdi=NA,
          umz=NA,
          lmz=NA,
          mgt.zone = c(0.35,0.60),
          reineke.term=1.6,
          bsi=90,
          dcol = "blue",
          rdcol= "black",
          mzcol= "lightgrey",
          dmd.title = " ",
          use.metric=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ineq}{
Selects limiting SDI and other parameters. This must be an integer value from 1 to 14.
    Value of 1 generates a generic DMD space, no published volumes.

    Value is 2 for Long and Shaw (2005) Ponderosa pine equations,

    Value of 3 for Jang et al. (2021) Ponderosa pine equations.

    Value of 4 for Edminster (1988) Ponderosa pine.

    Value of 5 for Cochran and Barrett (1992) Ponderosa pine.

    Value of 6 for Long and Shaw (2012) California mixed-conifer.

    Value of 7 for Long et al. (1988) Douglas-fir.

    Value of 8 for Zhang et al. (2007) White fir.

    Value of 9 for McCarter and Long (1986) Lodgepole pine.

    Value of 10 for Weiskittel and Woodall (2023) spruce-fir

    Value of 11 for Weiskittel and Woodall (2023) red spruce

    Value of 12 for Weiskittel and Woodall (2023) balsam fir

    Value of 13 for Ritchie and Berrill (2022) redwood/Douglas-fir

    Value of 14 for Ritchie and Berrill (2022) Douglas-fir/redwood

    Value of 15 for Williams (1994) loblolly pine

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
  This is only used some of the ineq values. For ineq=1 Default is 400 and
  acceptable range is . If ineq=1, 6, 10, 11, 12, 13 or 14 the user must specify a value.
  If use.metric=TRUE this value must be in metric units.
  Currently ineq=1 (generic DMD), ineq=6 (Long and Shaw 2012), ineq=10-12 (Weiskittel and Woodall 2023),
  ineq=13-14 (Ritchie and Berrill 2022), or are the only DMD options where the
  user must actually specify this value.
  For most published DMDs the upper limit is fixed by the author and cannot be changed
  by user over-ride; in such cases, any user-specified upper limit will be ignored and
  the published value will be used to render the DMD.
  A value of ineq=1, requires English-unit max.sdi to be from 300-1000 (inclusive).
  A value of ineq=6, requires English-unit max.sdi to be from 450-600 (inclusive).
  A value of ineq=10 requires English-unit max.sdi to be from 527-564 (inclusive).
  A value of ineq=11 requires English-unit max.sdi to be from 547-608 (inclusive).
  A value of ineq=12 requires English-unit max.sdi to be from 502-669 (inclusive).
  A value of ineq=13 requires English-unit max.sdi to be from 700-1000 (inclusive).
  A value of ineq=14 requires English-unit max.sdi to be from 500-700 (inclusive).
}
  \item{umz}{
Defines upper limit of management zone, only used when mgt.zone[2] is NA.
}
  \item{lmz}{
Defines lower limit of management zone, only used when mgt.zone[1] is NA.
}
  \item{mgt.zone}{
Defines upper and lower limit of management zone for generic diagram only as
    prop. of max. sdi. These values take precedence over values in umz and lmz.
    Vector of length 2; mgt.zone[1] is proportion of sdi.max for lower limit;
    mgt.zone[2] is the proportion of sdi.max for upper limit.
}
  \item{reineke.term}{
This is the inverse of the slope of sdi lines in sdi space. Default=1.605,
  the value derived by Reineke (1933).
}
  \item{bsi}{
This is Barrett's (1978) site index for use in Cochrans (1992)diagram only;
  it sets the upper mgt. zone limit. Use feet for use.metric=FALSE, and
  meters for use.metric=TRUE.
}
  \item{dcol}{
This is the color for the diameter lines: default is "blue". Must be a color
  from the r color pallette.
}
  \item{rdcol}{
This is the color for the relative density lines: default is "black".
  Must be a color from the r colorpallette.
}
  \item{mzcol}{
This is the color for the management zone: default is "lightgrey".
  Must be a color from the r colorpallette.
}
  \item{dmd.title}{
This is the title for the management zone.
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

    Cochran, P.H. (1992) Stocking Levels and Underlying Assumptions for Uneven-Aged
      Ponderosa Pine Stands. Res. Note PNW-509. USDA For. Serv. Pac. Northwest For.
      and Range Exp. Stn. 10p.

    Gingrich, S. (1967) Measuring and evaluating stocking and stand density in upland
      hardwood forests in the central states. For. Sci. 13(1):38-53.

    Jang, W.S., M.W. Ritchie, and J. Zhang (2021) Improved equations for the density management
      diagram isolines of ponderosa pine stands. For. Sci. 67(1):93-102.

    Long, J.N. and J.D. Shaw (2005) A density management diagram for even-aged ponderosa
      pine stands. West. J. Appl. For. 20(4):205-215.

    McCarter, J.B. and J.N. Long (1986) A lodgepole pine density management diagram.
      West. J. Appl. For. 1(1)6-11.

    Reineke, L.H. (1933) Perfecting a stand density index for even-aged forests.
      J. Ag. Res. 46:627-638.

    Ritchie, M.W. and J.-P. Berrill (2022) Precommercial thinning in California Forests. In:
      Stewart, W. (editor) Reforestation practices for conifers in California. Davis CA:
      University of California, Agriculture and Natural Resources.

    Weiskittel, A. & Woodall, C. (2023) Stand density index and relative density calculator
      for the United States. figshare. Software. https://doi.org/10.6084/m9.figshare.24412246.v1

    Williams, R.A. (1994) Stand density  management diagram for loblolly pine plantations in north
      Louisiana South. J. Appl. For. 18(1):40-45.

    Zhang, J., W.W. Oliver and M.W. Ritchie (2007) Effect of stand densities on stand
      dynamics in white fir (Abies concolor) forests in northeast California, USA.
      For. Ecol. and Manage. 244:50-59.

}
\author{
Martin W. Ritchie, Research Forester, US Forest Service, Pacific Southwest Research Station.
  Email: \email{martin.ritchie@usda.gov}
}
\note{
It is important to note that the gdmd.view function is designed to render a 8x6 inch canvas (pdf).
    Any size other than that and the annotations and axis labels may not place correctly.
    For this reason, it is suggested that one open a pdf with the function call:
    pdf("myfile.pdf", width=8, height=6) prior to invoking the gdmd.view function. When the figure
    is completely rendered, then one should close the device with dev.off().
    See Examples below for how this can be done.
Also, some default values will depend on the ineq value selected. Defaults shown here are for ineq=1
    and may change according to selection of ineq to match published data.

}

\examples{
# 1. This example will produce a generic density mgt space in a file called myplot1.pdf:

#setwd("c:/mydir") # optional, to specify location (directory) for file input and output

#pdf("myplotg1.pdf", width=8, height=6) # optional (recommended), open a pdf file with 8wx6h
opar <- par(mar = c(3.0,2.0,2.5,2.5)) # set some reasonable margins
# build a dmd for Long and Shaw Ponderosa pine

gdmd.view(ineq=2)

par(opar)  # reset margins to default settings
#dev.off()  # close the optional pdf file

# 2. This example shows a plot of data from gama05 for treated plots
#       and calls for volume calculation.

#pdf("myplotg2.pdf", width=8, height=6) # optional (recommended), open a pdf file with 8wx6h
opar <- par(mar = c(3.0,2.0,2.5,2.5))

gdmd.view(ineq=3 ) # call gdmd.view and plot data.

points(x=gama05[!gama05$treatment=="Control",]$tpa,
       y=gama05[!gama05$treatment=="Control",]$ba,
       cex=0.75, pch=21, col="black")

par(opar)
#dev.off() # close the optional pdf file

# 3. This example tests the Maine spruce/fir option

#pdf("myplotsfg.pdf", width=8, height=6) # optional (recommended), open a pdf file with 8wx6h
opar <- par(mar = c(3.0,2.0,2.5,2.5))

gdmd.view(ineq=10, max.sdi=542) # call gdmd.view and plot data.

par(opar)
#dev.off() # close the optional pdf file


}
\keyword{ color }
\keyword{ device }
