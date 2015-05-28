\name{mlxR}

\alias{mlxR}

\title{
Model Simulation 
}
\description{
Simulation and visualization of complex models for longitudinal data
}

\details{
The mlxR package includes functions for the simulation and visualization of complex models for longitudinal data:

Simulation

\code{\link{simulx.R}}: Compute predictions and sample data from Mlxtran and PharmML models 

\item{pkmodel }{
Compute easily basic pharmacokinetics (PK) models
}
\item{exposure }{Compute the area under the curve, the maximum and minimum values of a funtion of time over an interval, or at steady state} 
\item{monolix2simulx}{Convert a Monolix Project into an executable R script for  \code{Simulx}}

Model visualization
\item{shinymlx}{Create an interactive Shiny application for longitudinal data} 
\item{mlxplore}{Explore and visualize Mlxtran and pharmML models with the Mlxplore software}

Data visualization
\item{prctilemlx}{Compute and display percentiles of the empiricial distribution of longitudinal data}
\item{kmplotmlx}{Plot empirical survival functions using the Kaplan Meier estimate}
\item{catplotmlx}{Plot the empirical distribution of longitudinal categorical data.}

\references{
Lavielle M."Mixed Effects Models for the Population Approach. Models, Tasks, Methods & Tools", Chapman & Hall/CRC Biostatistics Series, 2014.
}

\author{
Marc Lavielle with contributions from Raphaël Kuate, Romain François, Fazia Bellal.
}
\note{
mlxR requires the C++ mlxLibrary: 
\code{http://download.lixoft.com/?software=mlxlibrary}
}

\seealso{
Detailed documentation and examples on the mlxR website: 
\code{http://simulx.webpopix.org}
}
