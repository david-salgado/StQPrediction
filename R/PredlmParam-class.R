#' @title S4 class for the parameters for prediction based on a linear model
#'
#' @description Definition of the S4 class named \code{PredlmParam} for the parameters to make
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
#' new(Class = 'PredlmParam')
#'
#' \dontrun{
#' ImpParam <- new(Class = 'PredlmParam',
#'                 VarNames = 'CifraNeg_13.___',
#'                 DomainNames =  'Tame_05._4.')
#' ErrorProbMLEParam <- new(Class = 'PredlmParam',
#'                          RawData = FD.StQList,
#'                          EdData = FG.StQList,
#'                          VarNames = 'CifraNeg_13.___',
#'                          Imputation = ImpParam)
#'
#'
#' }
#'
#' @import data.table StQ RepoTime StQImputation
#'
#' @export
setClass(Class = "PredlmParam",
         slots = c(EdData = 'StQ',
                   VarNames = 'character',
                   DomainNames = 'character',
                   Imputation = 'ImputationParam'),
         prototype = list(EdData = new(Class = 'StQ'),
                          VarNames = character(0),
                          DomainNames = character(0),
                          Imputation = new(Class = 'MeanImputationParam')),
         validity = function(object){


           return(TRUE)
         }
)
