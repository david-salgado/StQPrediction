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
#' @examples
#' # An empty PredTSParam object:
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
#' PredTSParam <- new(Class = 'PredTSParam',
#'                    TS = StQList,
#'                    Param = BestTSPredParam)
#'
#'
#' }
#'
#' @import data.table StQ RepoTime BestTSPred
#'
#' @export
setClass(Class = "PredTSParam",
         slots = c(TS = 'StQList',
                   Param = 'BestTSPredParam'),
         prototype = list(TS = StQList(),
                          Param = new(Class = 'BestTSPredParam')),
         validity = function(object){


           return(TRUE)
         }
)
