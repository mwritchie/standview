\name{dmd.iso}
\alias{dmd.iso}
\title{
Density Management Iso Line Rendering
}
\description{
This function draws iso-volume lines. It is intended to work with a call to dmd.view function in standview.
}
\usage{
dmd.iso(ineq           = 2,
        v.at         = NULL,
        range.x      = NULL,
        max.sdi      = NULL,
        reineke.term = 1.60,
        vty          = 2,
        vcex         = 0.75,
        vcol         = "blue",
        show.vol     = TRUE,
        v.ann        = TRUE,
        use.metric   = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ineq}{
  Selects volume and dominant height equations. This must be an integer value: 2, 3, 6, 7, or 9.
    Value 2 is for Long and Shaw (2005) Ponderosa pine equations.
    Value 3 is for Ritchie and Zhang (unpublished) Ponderosa pine equations.
    Value 6 is for Long and Shaw (2012) California mixed-conifer.
    Value 7 is for Drew and Flewelling (1979) Douglas-fir.
    Value 9 is for McCarter and Long (1986) lodgepole pine.
    Default=2.
}
  \item{v.at}{
  A vector of volume iso lines. Units are in cubic feet per acre if use.metric=FALSE, and
    units are cubic meters per hectare if use.metric=TRUE.
  }

  \item{range.x}{
  Range of tree density acre from range.x[1] to range.x[2] , must be a vector of
    length 2; e.g. range.x=c(50,500). Units are trees per acre if use.metric=FALSE,
    and units are trees per ha if use.metric=TRUE.
  }
  \item{max.sdi}{
  Maximum stand density index, must be expressed as trees per acre if use.metric=FALSE
    and trees per ha if use.metric=TRUE. This value only needed for ineq=6.
  }
  \item{reineke.term}{
  This is the inverse of the slope of sdi lines in sdi space. Default=1.605, the value derived by Reineke (1933).
  }
  \item{vty}{
  This is the line type for volume, default= 2.
  }
  \item{vcex}{
  This is the character size for volume annotations, default= 0.75.
  }
  \item{vcol}{
  This is the color for volume, default= "blue".
  }
  \item{show.vol}{
  Show volume iso-lines, default= TRUE.
  }
  \item{v.ann}{
  Turn on volume annotations, default= FALSE.
  }
  \item{use.metric}{
  Use metric units (logical), default= FALSE.
  }
}
\value{
Returns a data frame with tpa, qmd, ba, volume (cubic feet per acre),
  stand height (feet) and (if available) above-ground biomass (pounds per acre).
}
\references{
    Drew, T.J. and J.W. Flewelling (1979). Stand density management: an alternative
      approach and its application to Douglas-fir plantations. For. Sci. 25(3):518-532.

    Long, J.N., J.B. McCarter, and S.B. Jack. (1988) A modified density management
      diagram for coastal Douglas-fir. West. J. Appl. For. 3(3):88-89.

    Long, J.N. and J.D. Shaw (2005) A density management diagram for
      even-aged ponderosa pine stands. West. J. Appl. For. 20(4):205-215.

    Long, J.N. and J.D. Shaw (2012) A density management diagram for
      even-aged Sierra Nevada mixed-conifer Stands. West. J. Appl. For. 27(4):187-195.

    McCarter, J.D. and J.N. Long (1986) A logdepole pine density management diagram.
      West. J. Appl. For 1:6-11.

    Reineke, L.H. (1933) Perfecting a stand density index for even-aged
      forests. J. Ag. Res. 46:627-638.
}
\author{
Martin W. Ritchie, Research Forester, US Forest Service, Pacific Southwest Research Station.
  Email: \email{martin.ritchie@usda.gov}
}
\note{
This function returns a dataframe that could represent observations for a stand prescription over time.
    The observations could also be for sampled independent stands. The species for the observations
    should match the species identified by the value ineq. So if ineq=1 is chosen, then the volumes
    returned will be from Long and Shaw 2005.
    Vectors passed in must match in length.
    See Examples below.
}

\examples{
# 1. This example generate a prescription using equations from Ritchie and Zhang:

library(standview)
pdf("Douglasfir.pdf", width=6, height=8)
opar <- par(mar = c(2.0,2.0,2.0,2.0))

dmd.view(ineq=7, insdr=FALSE)
dmd.iso(ineq=7,
        range.x=c(50, 1000),
        v.at=c(200, 400, 600, 800, 1000, 2000, 3000, 4000, 6000, 8000))

par(opar)
dev.off()

# 2. A similar result may be obtained with the invol argument in dmd.view,
#    where invol=TRUE directs dmd.view to call the dmd.iso function:
pdf("Douglasfir2.pdf", width=6, height=8)
opar <- par(mar = c(2.0, 2.0, 2.0, 2.0))

dmd.view(ineq=7, insdr=FALSE, invol=TRUE)

par(opar)
dev.off()

# 3. Converting to metric:
pdf("Douglasfir2metric.pdf", width=6, height=8)
opar <- par(mar = c(2.0,2.0,2.0,2.0))

dmd.view(ineq=7, insdr=FALSE, invol=TRUE, use.metric=TRUE)

par(opar)
dev.off()

}

