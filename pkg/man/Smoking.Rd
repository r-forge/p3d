\name{Smoking}
\Rdversion{1.1}
\alias{Smoking}
\concept{Simpson's paradox}
\concept{causal infefence}
\concept{observational data}
\concept{ecological correlation}
\docType{data}
\title{
Cigarette consumption and life expectancy by country
}
\description{
The variables for this data set were assembled from various web sources. They consist of
life expectancy, cigarette consumption per capita and health expenditures per capita in 2004.
The relationship between life expectancy and cigarette consumption is paradoxical leading to
reflection on omitted variables and problems of causal inference with observational and aggregated
data.  The association between life expectancy and cigarette consumption could be
termed an 'ecological correlation'.
}
\usage{data(Smoking)}
\format{
  A data frame with 192 observations on the following 10 variables.
  \describe{
    \item{\code{Country}}{a factor with 192 levels \code{Afghanistan} \code{Albania} \code{Algeria} \code{Andorra} ...}
    \item{\code{Continent}}{a factor with 6 levels \code{Africa} \code{Asia} \code{Australia} \code{Europe} \code{North America} \code{South America}}
    \item{\code{LE}}{Life expectancy (combining sexes -- separate would be much more interesting), a numeric vector}
    \item{\code{CigCon}}{Annual cigarette consumption per capita, a numeric vector}
    \item{\code{LE.q}}{Life expectancy quartile, a factor with levels \code{(28.6,40.2]} \code{(40.2,51.8]} \code{(51.8,63.4]} \code{(63.4,75]}}
    \item{\code{Cont}}{short labels for Continent, a factor with levels \code{Afrc} \code{Asia} \code{Astr} \code{Eurp} \code{NrtA} \code{SthA}}
    \item{\code{Cont2}}{short labels for Continent, a factor with levels \code{Africa} \code{Asia} \code{Australia} \code{Europe} \code{N.America} \code{S.America}}
    \item{\code{HealthExpPC}}{Health expenditure per capita in US $, a numeric vector}
    \item{\code{Year}}{relevant year -- all 2004, a numeric vector}
    \item{\code{HE}}{Health expenditure quartile, a factor with levels \code{(152,476]} \code{(31.1,152]} \code{(476,6.1e+03]} \code{[0.3,31.1]}}
  }
}

\details{
Each row consists of the data for one country.
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
http://www.nationmaster.com
}

%\references{
%%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(Smoking)
str(Smoking)
fit1 <- lm( LE ~ CigCon, Smoking)
fit2 <- lm( LE ~ CigCon + HealthExpPC, Smoking)
summary(fit2) 
anova(fit1,fit2)

\dontrun{
    head(Smoking)
    rownames(Smoking) = Smoking$Country

    Init3d(family = 'serif', cex = 1.5)
    Plot3d( LE ~ CigCon + HealthExpPC | Continent, Smoking)
    Axes3d()
    Identify3d(pad=1)


    fit = lm( LE ~ CigCon + log(HealthExpPC) +I(CigCon^2) + I(log(HealthExpPC)^2) + I(CigCon*log(HealthExpPC)), Smoking)
    Fit3d( fit )
    fitl <- lm( LE ~ CigCon + HealthExpPC, Smoking)
    Fit3d( fitl, col = 'pink')
# HEpCap is highly 'skewed': dense on left, long tail on right
    require( lattice )
    densityplot( Smoking$HealthExpPC )

# Useful to use a transformation to make spread more even
#  e.g. log
# First make sure all values are positive:

    sort( Smoking$HealthExpPC)

# Do log transformation:

    Smoking$LogHE <- log(Smoking$HealthExpPC)    # create log HE

    densityplot( Smoking$LogHE )

# Also usefult to have categories:

    Smoking$HECat <- cut(Smoking$LogHE, 7)       # create categories
    summary(Smoking)

    Plot3d( LE ~ CigCon + LogHE |HECat, Smoking )  # condition on level of HEpC
    Axes3d()
    Ell3d()
    Identify3d(pad=1)

# Simple regression

    fit.lin <- lm( LE ~ CigCon, Smoking)
    Fit3d( fit.lin )
    fit.lin2 <- lm(LE ~ CigCon, subset(Smoking, Continent != "Africa"))
    Fit3d( fit.lin2)
    Pop3d(4)

# Use multiple regression (advanced version with quadratic surface)

    fit = lm( LE ~ CigCon + I(LogHE^2) +I(CigCon^2) + I(LogHE^2) + I(CigCon*LogHE), Smoking)
    Fit3d( fit, col = 'red' )
    Pop3d(2)
        
# refit omitting Africa:

    fit.na = lm( LE ~ CigCon + I(LogHE^2) +I(CigCon^2) + I(LogHE^2)
               + I(CigCon*LogHE), Smoking, subset = Continent != "Africa")
    Fit3d( fit.na, col = 'red' )

# Marginal relationship

    Pop3d(4)    # pop twice for each fit
    fit.quad <- lm( LE ~ CigCon + I(CigCon^2) , Smoking)
    Fit3d( fit.quad, col = 'green')

#  A quadratic surface within each Continent (overfitting?!)

    fit2 = lm( LE ~ Continent*(CigCon + HealthExpPC +I(CigCon^2) +
            I(HealthExpPC^2) + I(CigCon*HealthExpPC)), Smoking)
    Fit3d( fit2 )
}

}
\keyword{datasets}
