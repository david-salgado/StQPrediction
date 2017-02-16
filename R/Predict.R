#' \code{Predict} computes the predicted values and their standard deviations for each
#' statistical unit
#'
#'
#' @param object \linkS4class{data.table} containing the statistical units whose predicted values
#' and standard deviations for each variable is to be computed.
#'
#' @param Param Object of virtual class \linkS4class{PredParam} with the parameters determining
#' the method of computation of the predicted values and standard deviations of each statistical
#' unit.
#'
#' @return Object of class \linkS4class{contObsPredModelParam} with the predicted values and
#' standard deviations computed for each variable and each statistical unit.
#'
#'
#' @examples
#' \dontrun{
#'
#' library(xlsx)
#' library(RepoReadWrite)
#' DD <- RepoXLSToDD('S:/E30183/E30183.NombresVariables_V1.xlsx')
#' FD.StQ <- ReadRepoFile('S:/E30183/E30183.FD_V1.MM032016.P_1', DD, perl = TRUE)
#' FF.StQ <- ReadRepoFile('S:/E30183/E30183.FF_V1.MM022016.D_1', DD, perl = TRUE)
#' ImpParam <- new(Class = 'MeanImputationParam',
#'                 VarNames = c('CifraNeg_13.___', 'Personal_07.__2.__'),
#'                 DomainNames =  c('Tame_05._2.', 'ActivEcono_35._4._2.1.4._0'))
#' PredlmParam <- new(Class = 'PredlmParam',
#'                    EdData = FF.StQ,
#'                    VarNames = c('CifraNeg_13.___', 'Personal_07.__2.__'),
#'                    Imputation = ImpParam)
#' Predict(FD.StQ, PredlmParam)
#'
#' }
setGeneric("Predict", function(object, Param) {standardGeneric("Predict")})

#' @rdname Predict
#'
#' @include PredlmParam-class.R
#'
#' @import data.table StQ
#'
#' @export
setMethod(f = "Predict",
          signature = c("data.table", "MeanImputationParam"),
          function(object, Param){

          return(TRUE)
          }
)