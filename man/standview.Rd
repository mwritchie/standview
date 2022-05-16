\name{standview}
\alias{standview}
\title{
R Package for Density Management Diagrams
}
\description{
The standview package provides functions to create forest density management diagrams (DMD):
  dmd.view, gdmd.view, dmd.volume, and dmd.iso. All DMD are rendered in English units.
  Quadratic mean diameter is in inches, basal area is square feet per acre, height is in feet,
  and volume is cubic feet per acre. Functions for creating DMDs in Reineke-space (Reineke 1933)
  with dmd.view and Gingrich-space (Gingrich 1967) with gdmd.view are provided. The user may select
  from published equations (e.g. Long and Shaw 2005). In the current implimentation there are nine
  published equations and one generic option.
}
\details{
    Note that the default arguments of the functions in the standview package are set for English
      units. If use.metric (logical) is set to true, these default settings will no longer serve.
      The user must specifically set values for any arguments which have units associated. So,
      for example if use.metric=TRUE, then the user must change values accordingly.

    The package also includes a sample dataframe: gama05, which contains English-unit observations on
      20 research plots at the Goosenest Adaptive Management Area from 2005 (Ritchie 2005).

    If you happen to find a problem with the package, please email the author with the script causing
      the problem, as well as the version number. The version number is shown below or it
      can be obtained with function call:

    \preformatted{packageDescription("standview")$Version}

    Typical function usages are:
    \preformatted{dmd.view(ineq, ...)}
    \preformatted{gdmd.view(ineq, ...)}
    \preformatted{dmd.iso(ineq, ...)}
    \preformatted{dmd.volume(ineq, ...)}
}
\seealso{
  \code{\link{dmd.view}}
  \code{\link{gdmd.view}}
  \code{\link{dmd.iso}}
  \code{\link{dmd.volume}}
  \code{\link{dmd.htfunc}}
}
\references{
    Gingrich, S. (1967) Measuring and evaluating stocking and stand density in upland
      hardwood forests in the central states. For. Sci. 13(1):38-53.

    Long, J.N. and J.D. Shaw (2005) A density management diagram for even-aged
      ponderosa pine stands. West. J. Appl. For. 20(4):205-215.

    Reineke, L.H. (1933) Perfecting a stand density index for even-aged forests.
      J. Ag. Res. 46:627-638.

    Ritchie, M.W. (2005) Ecological research at the Goosenest Adaptive
      Management Area in northeastern California. Gen. Tech. Rep.
      PSW-192. USDA For. Serv. Pac. SW Res. Stn. 120p.

}
\author{
Martin W. Ritchie, Research Forester, US Forest Service, Pacific Southwest Research Station.
  Email: \email{martin.ritchie@usda.gov}
}
\keyword{dplot}
