\name{dmd.htfunc}
\alias{dmd.htfunc}
\alias{jrzheight}
\alias{ls2012height}
\alias{McC1986height}
\alias{Wl1994height}
\title{
Density management height functions
}
\description{
This suite of functions estimates height for forest stands as a function
  of trees per acre and quadratic mean diameter. Since the dmd.volume function
  calls these functions, it is not necessary that user's access these
  directly. These functions are parameterized for English unit input and output.
  User is responsible for metric conversions.
}
\usage{
jrzheight(tpa, qmd)
ls2012height(tpa, qmd)
McC1986height(tpa, qmd)
Wl1994height(qmd, mV)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tpa}{
Trees per acre.
}
  \item{qmd}{
Stand quadratic mean diameter (inches).
}
  \item{mV}{
Mean tree volume (cubic feet).
}

}
\value{
Returns dominant height (feet) for Jang et al. (2021), Long and Shaw (2012), McCarter and Long (1986),
or Williams (1994).
}
\references{
    Jang, W.S., M.W. Ritchie and J. Zhang (2021) Improved equations for the density management
    diagram isolines of ponderosa pine stands. For. Sci. 67(1):93-102

    Long, J.N. and Shaw, J.D. (2005) A density management diagram for even-aged ponderosa pine stands.
    West. J. Appl. For. 20(4):205-215.

    Long, J.N. and Shaw, J.D. (2012) A density management diagram for even-aged Sierra Nevada
    mixed-conifer Stands. West. J. Appl. For. 27(4):187-195.

    McCarter, J.B. and J.N. Long (1986) A lodgepole pine density management diagram.
    West. J. Appl. For. 1(1)6-11.

    Reineke, L.H. (1933) Perfecting a stand density index for even-aged forests. J. Ag. Res. 46:627-638.

    Williams, R.A. (1994) Stand density management diagram for loblolly pine plantations
    in north Louisiana. South. J. Appl. For. 18(1):40-45.
}
\author{
Martin W. Ritchie, Research Forester, US Forest Service, Pacific Southwest Research Station.
  Email: \email{martin.ritchie@usda.gov}
}
\examples{

jrzheight(tpa=200, qmd=10.2)
ls2012height(tpa=200, qmd=10.2)
McC1986height(tpa=200, qmd=10.2)
Wl1994height(qmd=14, mV=40)
}

