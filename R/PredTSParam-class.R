#' @title S4 class for the parameters for prediction based on time series techniques
#'
#' @description Definition of the S4 class named \code{PredTSParam} for the parameters to make
#' predictions in the optimization approach to selective editing.
#'
#'
#' @slot EdData \linkS4class{StQ} object with the edited data.
#'
#' @slot VarNames Character vector with the names of the variables whose probability errors are to
#' be computed.
#'
#' @slot DomainNames Character vector with the names of the variables determining the domains of the
#' population within which predictions are to be calculated separately.
#'
#' @slot Imputation \linkS4class{ImputationParam} object with the parameters to imputed missing
#' values during the computation of predicted values and their standard deviations (predition error
#' standard deviations).
#'
#' @examples
#' # An empty PredlmParam object:
#' new(Class = 'PredTSParam')
#'
#' \dontrun{
#' StQList <- readRDS('../E30183.FF.StQList.sub.rds')
#' TS.list <- list(Reg = list('RegDiffTSPred', forward = 2L),
#'                 Stat = list('StatDiffTSPred', forward = 2L),
#'                 StatReg = list('StatRegDiffTSPred', forward = 2L),
#'                 Arima = list('AutoArimaTSPred', forward = 2L))
#' VarNames <- c('CifraNeg_13.___', 'Personal_07.__1._1._')
#' BestTSPredParam <- new(Class='BestTSPredParam', TSPred.list = TS.list, VarNames = VarNames)
#'
#' ImpParam <- new(Class = 'MeanImputationParam',
#'                 VarNames = 'CifraNeg_13.___',
#'                 DomainNames =  'Tame_05._4.')
#' PredTSParam <- new(Class = 'PredTSParam',
#'                    TS = StQList,
#'                    Param = BestTSPredParam,
#'                    Imputation = ImpParam)
#'
#'
#' }
#'
#' @import data.table StQ RepoTime StQImputation BestTSPred
#'
#' @export
setClass(Class = "PredTSParam",
         slots = c(TS = 'StQList',
                   Param = 'BestTSPredParam',
                   Imputation = 'ImputationParam'),
         prototype = list(TS = StQList(),
                          Param = new(Class = 'BestTSPredParam'),
                          Imputation = new(Class = 'MeanImputationParam')),
         validity = function(object){


           return(TRUE)
         }
)
