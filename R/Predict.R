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
#' @return \linkS4class{data.table} with the predicted values and
#' standard deviations computed for each variable and each statistical unit.
#'
#'
#' @examples
#' \dontrun{
#'
#' library(xlsx)
#' library(StQ)
#' library(RepoReadWrite)
#' DD <- RepoXLSToDD('S:/E30183/E30183.NombresVariables_V1.xlsx')
#' FD.StQ <- ReadRepoFile('S:/E30183/E30183.FD_V1.MM032016.P_1', DD, perl = TRUE)
#' Units <- getUnits(FD.StQ)
#' IDQuals <- names(Units)
#' VarNames <- c('CifraNeg_13.___', 'Personal_07.__2.__')
#' FD.dm <- dcast_StQ(FD.StQ, ExtractNames(VarNames))[, c(IDQuals, VarNames), with = FALSE]
#' FF.StQ <- ReadRepoFile('S:/E30183/E30183.FF_V1.MM032016.D_1', DD, perl = TRUE)
#' DomainName <- c('Tame_05._2.', 'ActivEcono_35._4._2.1.4._0')
#' FF.dm <- dcast_StQ(FF.StQ, ExtractNames(DomainName))[, c(IDQuals, DomainName), with = FALSE]
#' FD.dm <- merge(FD.dm, FF.dm, by = IDQuals, all.x = TRUE)
#' FF.StQ <- ReadRepoFile('S:/E30183/E30183.FF_V1.MM022016.D_1', DD, perl = TRUE)
#' ImpParam <- new(Class = 'MeanImputationParam',
#'                 VarNames = c('PredCifraNeg_13.___', 'PredErrorSTDCifraNeg_13.___',
#'                              'PredPersonal_07.__2.__', 'PredErrorSTDPersonal_07.__2.__'),
#'                 DomainNames =  c('Tame_05._2.', 'ActivEcono_35._4._2.1.4._0'))
#' PredlmParam <- new(Class = 'PredlmParam',
#'                    EdData = FF.StQ,
#'                    VarNames = c('CifraNeg_13.___', 'Personal_07.__2.__'),
#'                    DomainNames = 'Tame_05._2.',
#'                    Imputation = ImpParam)
#' Predict(FD.dm, PredlmParam)
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
          signature = c("data.table", "PredlmParam"),
          function(object, Param){

          if (!all(Param@DomainNames %in% names(object))) {

            stop('[StQPrediction:: Predict] DomainNames in slot DomainNames not contained in input object Param.')

          }

          if (!all(Param@Imputation@DomainNames %in% names(object))) {

            stop('[StQPrediction:: Predict] DomainNames in slot Imputation not contained in input object Param.')

          }

          Variables <- Param@VarNames
          DomainNames <- unique(c(Param@DomainNames, Param@Imputation@DomainNames))
          EdData.StQ <- Param@EdData
          Units <- getUnits(EdData.StQ)
          IDQuals <- names(Units)
          EdData.dm <- dcast_StQ(EdData.StQ, ExtractNames(Variables))[, c(IDQuals, Variables), with = FALSE]
          byVars <- setdiff(intersect(names(object), names(EdData.dm)), Variables)
          Data <- merge(EdData.dm, object, by = byVars, all.y = TRUE)
          DataSplit <- split(Data, Data[, Param@DomainNames, with = F])

          lms <- lapply(DataSplit, function(DataComponent){

            out <- lapply(Variables, function(Var){

              localData <- DataComponent[get(paste0(Var, '.y')) > 0 & get(paste0(Var, '.x')) > 0]
              localModel <- lm(as.formula(paste(paste0(Var, '.y'), paste0(Var, '.x'), sep = ' ~ ')), localData)
              localPred <- predict(localModel, newdata = DataComponent[, paste0(Var, '.x'), with = FALSE], interval = 'prediction')
              Predstd <- (localPred[, 'upr'] - localPred[, 'lwr']) / (2 * qt(0.95, df.residual(localModel)))
              outLocal <- cbind(DataComponent[, IDQuals, with = FALSE], data.table(localPred[, 'fit'], Predstd))
              setnames(outLocal, c(IDQuals, paste0('Pred', Var), paste0('PredErrorSTD', Var)))
              return(outLocal)
            })
            names(out) <- Variables
            out <- Reduce(function(x, y){merge(x, y, by = intersect(names(x), names(y)))}, out, init = out[[1]])
            return(out)
          })
          output <- Reduce(function(x, y){merge(x, y, by = intersect(names(x), names(y)), all = TRUE)}, lms, init = lms[[1]])
          output <- output[Data[, c(IDQuals, DomainNames), with = FALSE]]
          output <- Impute(output, Param@Imputation)
          return(output)

          return(lms)
          }
)
