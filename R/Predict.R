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
#'
#'
#' FD <- readRDS('../E30183.FD.MM122016.rds')
#' Units <- StQ::getUnits(FD)
#' Units <- Units[sample(1:(dim(Units)[1]), 1000)]
#' StQ::setUnits(FD) <- Units
#' FD.dm <- StQ::dcast_StQ(FD)
#' StQList <- readRDS('../E30183.FF.StQList.rds')
#' StQ::setUnits(StQList) <- Units
#' TS.list <- list(Reg = list('RegDiffTSPred', forward = 2L),
#'                 Stat = list('StatDiffTSPred', forward = 2L),
#'                 StatReg = list('StatRegDiffTSPred', forward = 2L),
#'                 Arima = list('AutoArimaTSPred', forward = 2L))
#' VarNames <- c('CifraNeg_13.___', 'Personal_07.__1._1._')
#' BestTSPredParam <- new(Class='BestTSPredParam', TSPred.list = TS.list, VarNames = VarNames)
#'
#' ImpParam <- new(Class = 'MeanImputationParam',
#'                 VarNames = 'CifraNeg_13.___',
#'                 DomainNames =  'Tame_05._2.')
#' PredTSParam <- new(Class = 'PredTSParam',
#'                    TS = StQList,
#'                    Param = BestTSPredParam,
#'                    Imputation = ImpParam)
#' Predict(FD.dm, PredTSParam)
#'
#' }
#'
#' @include PredlmParam-class.R
#'
#' @import data.table StQ
#'
#' @export
setGeneric("Predict", function(object, Param) {standardGeneric("Predict")})

#' @rdname Predict
#'
#' @export
setMethod(f = "Predict",
          signature = c("data.table", "PredlmParam"),
          function(object, Param){

          if (!all(Param@DomainNames %in% names(object))) {

            stop('[StQPrediction:: Predict] DomainNames in slot DomainNames not contained in input object Param.')

          }

          Variables <- Param@VarNames
          DomainNames <- Param@DomainNames
          EdData.StQ <- Param@EdData
          Units <- getUnits(EdData.StQ)
          IDQuals <- names(Units)
          EdData.dm <- dcast_StQ(EdData.StQ, ExtractNames(Variables))[, c(IDQuals, Variables), with = FALSE]

          byVars <- setdiff(intersect(names(object), names(EdData.dm)), Variables)
          Data <- merge(EdData.dm, object, by = byVars, all.y = TRUE)
          DataSplit <- split(Data, Data[, Param@DomainNames, with = F])
          Preds <- lapply(DataSplit, function(DataComponent){

            out <- lapply(Variables, function(Var){

              localData <- DataComponent[get(paste0(Var, '.y')) > 0 & get(paste0(Var, '.x')) > 0]
              nLocalData <- dim(localData)[1]
              if (nLocalData >= 3){

                localModel <- lm(as.formula(paste(paste0(Var, '.y'), paste0(Var, '.x'), sep = ' ~ ')), localData)
                localPred <- predict(localModel, newdata = DataComponent[, paste0(Var, '.x'), with = FALSE], interval = 'prediction')
                Predstd <- (localPred[, 'upr'] - localPred[, 'lwr']) / (2 * qt(0.95, df.residual(localModel)))
                outLocal <- cbind(DataComponent[, IDQuals, with = FALSE], data.table(localPred[, 'fit'], Predstd))

              } else if (nLocalData >0) {

                outLocal <- cbind(DataComponent[, IDQuals, with = FALSE], data.table(rep(NA_real_, nLocalData), rep(NA_real_, nLocalData)))

              } else {

                outLocal <- cbind(DataComponent[, IDQuals, with = FALSE][0], data.table(numeric(0), numeric(0)))

              }
              setnames(outLocal, c(IDQuals, paste0('Pred', Var), paste0('PredErrorSTD', Var)))
              return(outLocal)
            })
            names(out) <- Variables
            out <- Reduce(function(x, y){merge(x, y, by = intersect(names(x), names(y)))}, out, init = out[[1]])
            return(out)
          })

          output <- Reduce(function(x, y){merge(x, y, by = intersect(names(x), names(y)), all = TRUE)}, Preds, init = Preds[[1]])
          output <- output[Data[, c(IDQuals, DomainNames), with = FALSE]]
          return(output)
          }
)

#' @rdname Predict
#'
#' @export
setMethod(f = "Predict",
          signature = c("data.table", "PredTSParam"),
          function(object, Param){

            EdData.StQList <- Param@TS
            PredVar <- Param@Param@VarNames
            STDVar <- Param@Param@VarNames
            Param@Param@VarNames <- unique(c(PredVar, STDVar))
            Periods <- getPeriods(EdData.StQList)
            IDQuals <- getIDQual(EdData.StQList[[length(Periods)]], 'MicroData')
            Units <- object[, IDQuals, with = FALSE]
            setUnits(EdData.StQList) <- Units
            output <- BestTSPred::BestTSPred(EdData.StQList, Param@Param)
            return(output)
          }
)
