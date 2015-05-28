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
\item{simulx}  compute predictions and sample data from Mlxtran and PharmML models
\item{pkmodel} compute easily basic pharmacokinetics (PK) models
\item{exposure.R} Compute the area under the curve, the maximum and minimum values of a funtion of time over an interval, or at steady state. 
\item{monolix2simulx} convert a Monolix Project into an executable R script for the  \code{Simulx}.

Model visualization
\item{shinymlx} create an interactive Shiny application for longitudinal data 
\item{mlxplore} explore and visualize Mlxtran and pharmML models with the Mlxplore software

Data visualization
\item{prctilemlx} compute and display percentiles of the empiricial distribution of longitudinal data
\item{kmplotmlx} plot empirical survival functions using the Kaplan Meier estimate
\item{catplotmlx} Plot the empirical distribution of longitudinal categorical data}

\references{
%% ~put references to the literature/web site here ~
}
\author{
Marc Lavielle with contributions from Raphaël Kuate, Romain François, Fazia Bellal.
}
\note{
mlxR requires the C++ mlxLibrary: 
\code{\link{http://download.lixoft.com/?software=mlxlibrary}}
}

\seealso{
Detailed documentation and examples on the mlxR website: 
\code{\link{http://simulx.webpopix.org}}
}
