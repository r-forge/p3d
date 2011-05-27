\name{Smoking}
\alias{Smoking}
\docType{data}
\title{
Data on smoking and life expectancy in 192 countries
}
\description{
Data on smoking and life expectancy in 192 countries
}
\usage{data(Smoking)}
\format{
  A data frame with 192 observations on the following 10 variables.
  \describe{
    \item{\code{Country}}{a factor with 192 levels \code{Afghanistan} \code{Albania} \code{Algeria} \code{Andorra} ...}
    \item{\code{Continent}}{a factor with 6 levels \code{Africa} \code{Asia} \code{Australia} \code{Europe} \code{North America} \code{South America}}
    \item{\code{LE}}{Life expectancy, a numeric vector}
    \item{\code{CigCon}}{Cigarette consumption, a numeric vector}
    \item{\code{LE.q}}{Cigarette consumption, a factor with levels \code{(28.6,40.2]} \code{(40.2,51.8]} \code{(51.8,63.4]} \code{(63.4,75]}}
    \item{\code{Cont}}{short labels for Continent, a factor with levels \code{Afrc} \code{Asia} \code{Astr} \code{Eurp} \code{NrtA} \code{SthA}}
    \item{\code{Cont2}}{short labels for Continent, a factor with levels \code{Africa} \code{Asia} \code{Australia} \code{Europe} \code{N.America} \code{S.America}}
    \item{\code{HealthExpPC}}{Health, a numeric vector}
    \item{\code{Year}}{Year of data, a numeric vector}
    \item{\code{HE}}{Health, a factor with levels \code{(152,476]} \code{(31.1,152]} \code{(476,6.1e+03]} \code{[0.3,31.1]}}
  }
}
%\details{
%%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(Smoking)
str(Smoking) 
}
\keyword{datasets}
