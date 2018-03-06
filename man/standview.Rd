\name{standview}
\alias{standview}
\title{
R Package for Density Management Diagrams
}
\description{
The standview package provides functions to create forest density management diagrams (DMD):
  dmd.view, gdmd.view, dmd.volume, and dmd.iso. All DMD are rendered in English units.
  Quadratic mean diameter is in inches, basal area is square feet per acre, height is in feet,
  and volume is cubic feet per acre. Functions for creating DMDs in Reineke-space (dmd.view)
  and Gingrich-space are provided.
}
\details{
    Note that the default arguments of the functions in the standview package are set for English
      units. If use.metric (logical) is set to true, these default settings will no longer serve.
      The user must specifically set values for any arguments which have units associated. So,
      for example if use.metric=TRUE, then the user must change values accordingly.

    The package also includes a sample dataframe: gama05, which contains has observations on
      20 research plots at the Goosenest Adaptive Management Area from 2005.

    Typical usages are:
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
    Barrett, J.W. (1978) Height growth and site index curves for managed
      even-aged stands of ponderosa pine in the Pacific Northwest. Res.
      Pap. PNW-232. USDA For. Serv. Pac. Northwest For. and Range Exp. Stn. 14p.

    Cochran, P.H. (1992) Stocking Levels and Underlying Assumptions for Uneven-Aged
      Ponderosa Pine Stands. Res. Note PNW-509. USDA For. Serv. Pac. Northwest For.
      and Range Exp. Stn. 10p.

    Drew, T.J. and J.W. Flewelling (1979). Stand density management: an alternative
      approach and its application to Douglas-fir plantations. For. Sci. 25(3):518-532

    Edminster, C.B. (1988) Stand density and stocking in even-aged ponderosa pine stands.
      P 253-260 In: Baubmgarner, D.M and J.E. Lotan (Eds.) Proceedings, Ponderosa
      pine -- the species and its management. Washington State Univ. Pullman WA.

    Gingrich, S. (1967) Measuring and evaluating stocking and stand density in upland
      hardwood forests in the central states. For. Sci. 13(1):38-53.

    Long, J.N., J.B. McCarter and S.B. Jack. (1988) A modified density management
      diagram for coastal Douglas-fir. West. J. Appl. For. 3(3):88-89.

    Long, J.N. and J.D. Shaw (2005) A density management diagram for even-aged
      ponderosa pine stands. West. J. Appl. For. 20(4):205-215.

    Long, J.N. and J.D. Shaw (2012) A density management diagram for even-aged
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
\keyword{dplot}
