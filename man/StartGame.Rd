\name{StartGame}
\alias{StartGame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Playing TicTacToe
}
\description{
If you want to play against the computer or against a friend. 
}
\usage{
StartGame(Numb = "Single")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Numb}{
If Numb is equal to "Single" then you play against the computer. If you want to play against a friend then write "TwoPlayer".
}
}
\details{
All fields have a number such that top-left is number 1, top-right is number 3, botton-left is number 7, and botton-right is number 9.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
https://en.wikipedia.org/wiki/Tic-tac-toe
}
\author{
Thomas Maltesen
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
StartGame(Numb = "Single")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
