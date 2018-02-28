\name{dmd.iso}
\alias{dmd.iso}
\title{
Density Management Iso Line Rendering
}
\description{
This package provides functions to iso lines for volume.
}
\usage{
dmd.iso(ineq           = 2,
        v.at         = c(200, 400, 600, 800, 1000, 2000,
                         3000, 4000, 6000, 8000),
        range.x      = c(15,1000),
        max.sdi      = 450,
        reineke.term = 1.60,
        vty          = 2,
        vcex         = 0.75,
        vcol         = "blue",
        v.ann        = TRUE,
        show.vol     = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ineq}{
  Selects volume and dominant height equations. This must be an integer value: 2, 3, 6, 7
    Value is 2 for Long and Shaw (2005) Ponderosa pine equations,
    Value of 3 for Ritchie and Zhang (unpublished) Ponderosa pine equations.
    Value of 6 for Long and Shaw (2012) California mixed-conifer
    Value of 7 for Drew and Flewelling (1979).
    Value of 9 for McCarter and Long (1986).
    Default=2.
}
  \item{v.at}{
  Volume iso lines in cubic feet per acre.
  }
  \item{range.x}{
  Range of trees per acre from range.x[1] to range.x[2] , must be a vector of
    length 2; e.g. range.x=c(50,500).
  }
  \item{max.sdi}{
  Maximum stand density index, must be between 300 and 1000. If a number beyond
    this range is provided, then it is coerced to 400
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
  \item{v.ann}{
  Turn on volume annotations, default= FALSE.
  }
  \item{show.vol}{
  Turn on volume annotations, default= TRUE.
  }
}
\value{
Returns a data frame with tpa, qmd, ba, volume (cubic feet per acre),
  stand height (feet) and (if available) above-ground biomass (pounds per acre).
}
\references{
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
  Email: \email{mritchie@fs.fed.us}
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
dmd.iso(ineq=7)

par(opar)
dev.off()


}

