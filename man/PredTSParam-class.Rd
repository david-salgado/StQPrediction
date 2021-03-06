% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PredTSParam-class.R
\docType{class}
\name{PredTSParam-class}
\alias{PredTSParam-class}
\title{S4 class for the parameters for prediction based on time series techniques}
\description{
Definition of the S4 class named \code{PredTSParam} for the parameters to make
predictions in the optimization approach to selective editing.
}
\section{Slots}{

\describe{
\item{\code{EdData}}{\linkS4class{StQ} object with the edited data.}

\item{\code{VarNames}}{Character vector with the names of the variables whose probability errors are to
be computed.}

\item{\code{DomainNames}}{Character vector with the names of the variables determining the domains of the
population within which predictions are to be calculated separately.}

\item{\code{Imputation}}{\linkS4class{ImputationParam} object with the parameters to imputed missing
values during the computation of predicted values and their standard deviations (predition error
standard deviations).}
}}

\examples{
# An empty PredlmParam object:
new(Class = 'PredTSParam')

\dontrun{
StQList <- readRDS('../E30183.FF.StQList.sub.rds')
TS.list <- list(Reg = list('RegDiffTSPred', forward = 2L),
                Stat = list('StatDiffTSPred', forward = 2L),
                StatReg = list('StatRegDiffTSPred', forward = 2L),
                Arima = list('AutoArimaTSPred', forward = 2L))
VarNames <- c('CifraNeg_13.___', 'Personal_07.__1._1._')
BestTSPredParam <- new(Class='BestTSPredParam', TSPred.list = TS.list, VarNames = VarNames)

ImpParam <- new(Class = 'MeanImputationParam',
                VarNames = 'CifraNeg_13.___',
                DomainNames =  'Tame_05._4.')
PredTSParam <- new(Class = 'PredTSParam',
                   TS = StQList,
                   Param = BestTSPredParam,
                   Imputation = ImpParam)


}

}
