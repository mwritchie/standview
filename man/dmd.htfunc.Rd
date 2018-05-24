\name{dmd.htfunc}
\alias{dmd.htfunc}
\alias{rzheight}
\alias{ls2012height}
\alias{McC1986height}
\title{
Density management height functions
}
\description{
This suite of functions estimates height for forest stands as a function
  of trees per acre and quadratic mean diameter. Since the dmd.volume function
  calls these functions, it is not necessary that user's access these
  directly.
}
\usage{
rzheight(tpa, qmd)
ls2012height(tpa, qmd)
McC1986height(tpa, qmd)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tpa}{
Trees per acre.
}
  \item{qmd}{
Stand quadratic mean diameter (inches).
}

}
\value{
Returns dominant height (feet) for Ritchie and Zhang, Long and Shaw (2012), or McCarter and Long (1986).
}
\references{
    Long, J.N. and Shaw, J.D. (2005) A density management diagram for even-aged ponderosa pine stands.
    West. J. Appl. For. 20(4):205-215.

    Long, J.N. and Shaw, J.D. (2012) A density management diagram for even-aged Sierra Nevada
    mixed-conifer Stands. West. J. Appl. For. 27(4):187-195.

    Reineke, L.H. (1933) Perfecting a stand density index for even-aged forests. J. Ag. Res. 46:627-638.
}
\author{
Martin W. Ritchie, Research Forester, US Forest Service, Pacific Southwest Research Station.
  Email: \email{mritchie@fs.fed.us}
}
\examples{

rzheight(200, 10.2)
ls2012height(200, 10.2)
McC1986height(200,10.2)
}

