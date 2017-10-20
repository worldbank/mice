% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cbind.r
\name{cbind.mids}
\alias{cbind.mids}
\title{Columnwise combination of a \code{mids} object.}
\usage{
\method{cbind}{mids}(x, y = NULL, ...)
}
\arguments{
\item{x}{A \code{mids} object.}

\item{y}{A \code{mids} object or a \code{data.frame}, \code{matrix}, 
\code{factor} or \code{vector}.}

\item{\dots}{Additional \code{data.frame}, \code{matrix}, \code{vector} or \code{factor}. 
These can be given as named arguments.}
}
\value{
An S3 object of class \code{mids}
}
\description{
Append \code{mids} objects by columns
}
\details{
This function combines two \code{mids} objects columnwise into a single
object of class \code{mids}, or combines a \code{mids} object with 
a \code{vector}, \code{matrix}, \code{factor} or \code{data.frame} 
columnwise into a \code{mids} object.
The rows in the (incomplete) data \code{x$data} and \code{y} (or
\code{y$data} if \code{y} is a \code{mids} object) should match. If
\code{y} is a \code{mids}, then \code{cbind} only works if the number 
of imputations in \code{x} and \code{y} is equal.
}
\note{
The function construct the elements of the new \code{mids} object as follows:
\tabular{ll}{
\code{call}     \tab Vector, \code{call[1]} creates \code{x}, \code{call[2]} is call to \code{cbind.mids}\cr
\code{data}     \tab Columnwise combination of the (incomplete) data in \code{x} and \code{y}\cr
\code{where}    \tab Columnwise combination of \code{where} arguments\cr
\code{m}        \tab Equals \code{x$m}\cr
\code{nmis}     \tab Equals c(x$nmis, y$nmis)\cr
\code{imp}      \tab Appends \code{x$imp} and \code{y$imp} if \code{y} is \code{mids} object\cr
\code{method}   \tab Combines \code{x$method} and \code{y$method}\cr
\code{predictorMatrix} \tab Combines \code{x$predictorMatrix} and \code{y$predictMatrix} with zero matrices on the off-diagonal blocks\cr
\code{visitSequence}   \tab Taken from \code{x$visitSequence}\cr
\code{seed}            \tab Taken from \code{x$seed}\cr
\code{iteration}       \tab Taken from \code{x$iteration}\cr
\code{lastSeedValue}   \tab Taken from \code{x$lastSeedValue}\cr
\code{chainMean}       \tab Combines \code{x$chainMean} and \code{y$chainMean}\cr
\code{chainVar}        \tab Combines \code{x$chainVar} and \code{y$chainVar}\cr
\code{pad}             \tab Combines \code{x$padModel} and \code{y$padModel}\cr
\code{loggedEvents}    \tab Taken from \code{x$loggedEvents}
}

If a column of \code{y} is categorical this is ignored in the
padded model since that column is not used as predictor for another column.
}
\examples{

# impute four variables at once (default)
imp <- mice(nhanes, m = 1, maxit = 1, print = FALSE)
imp$predictorMatrix

# impute two by two
data1 <- nhanes[, c("age", "bmi")]
data2 <- nhanes[, c("hyp", "chl")]
imp1 <- mice(data1, m = 2, maxit = 1, print = FALSE)
imp2 <- mice(data2, m = 2, maxit = 1, print = FALSE)

# Append two solutions
imp12 <- cbind(imp1, imp2)

# This is a different imputation model
imp12$predictorMatrix

# Append the other way around
imp21 <- cbind(imp2, imp1)
imp21$predictorMatrix

# Append 'forgotten' variable chl
data3 <- nhanes[, 1:3]
imp3  <- mice(data3, maxit = 1,m = 2, print = FALSE)
imp3a <- cbind(imp3, chl = nhanes$chl)

# Of course, chl was not imputed
head(complete(imp3a))

# Note: If one of the arguments is a data.frame 
# we need to explicitly call mice:::cbind.mids()
imp3b <- mice:::cbind.mids(imp3, data.frame(chl = nhanes$chl))
}
\seealso{
\code{\link{rbind.mids}}, \code{\link{ibind}}, \code{\link[=mids-class]{mids}}
}
\author{
Karin Groothuis-Oudshoorn, Stef van Buuren
}
\keyword{manip}