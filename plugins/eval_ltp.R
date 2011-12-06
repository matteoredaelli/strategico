#!/usr/bin/env Rscript

## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

## project name: strategico
## project website: http://code.google.com/p/strategico/
## authors: L. Finos, M. Redaelli, M. Rinaldo
## created: 2011


library(ast)
library(ltp)

ltp.Item.EvalDataByValue <- function(project.name, id, item.data, value, output.path=".", param=NULL, project.config, db.channel) {

    model <- ltp(product = item.data, rule=param$rule, ruleSetting=list(rule.noMaxCVOver=param$rule.noMaxCVOver,rule.noJumpMaxOver=param$rule.noMaxJumpOver),
               try.models = param$try.models, n.ahead = param$n.ahead, n.min = param$n.min, 
               NA2value = param$NA2value, range = param$range, period.freq = project.config$period.freq, 
               period.start = project.config$period.start, period.end = project.config$period.end,diff.sea=1,diff.trend=1,max.p=2,max.q=1,max.P=0,max.Q=1, logtransform.es=FALSE , increment=1 ,idDiff = FALSE, idLog = FALSE,
               formula.right.lm = param$formula.right.lm,stepwise=param$stepwise,logtransform=param$logtransform, negTo0=param$negTo0,naive.values=param$naive.values)


  models.names <- ltp.GetModels("name")

  ## ###################################################################################
  ## Saving model.RData
  ## ###################################################################################

  if ("model" %in% project.config$save) {
    filename <- paste(output.path, "/model.RData", sep = "")
    ##__##logger(DEBUG, paste("Saving Model to file", filename))
    save(file=filename, model)
  }

  ## ###################################################################################
  ## Saving Normalized Data
  ## ###################################################################################
    
  if ( nrow(model@values) == 0) {
    ##__##logger(WARN, "No records in normalized data. No saving to DB")
    normalized.data <- ""
    normalized.period <- ""
  } else {
    normalized.data <- Vector.ToString(model@values$V)
    normalized.periods <- Vector.ToString(rownames(model@values))
  }
    
  if (is.null(model@BestModel)) {
    ##__##logger(WARN, "Best model is null. No saving to DB")
    return(2)
  } 

  
  predicted.periods <-Period.BuildRange(period.start=project.config$period.end,
                                          period.freq=project.config$period.freq,
                                          n=param$n.ahead, shift=1)
  str.predicted.periods <- Vector.ToString(predicted.periods)
  
  ## ###################################################################################
  ## Saving Summary Data
  ## ###################################################################################
  
  if ("summary" %in% project.config$save) {
    onerow.summ = ltp.BuildOneRowSummary(id=id, model=model, param)
    onerow.summ$normalizedPeriods <- normalized.periods
    onerow.summ$normalizedData <- normalized.data
    onerow.summ$predictedPeriods <- str.predicted.periods
    
    if (!is.null(model@BestModel)) {
      summary.models <- data.frame(ltp.GetModelsComparisonTable(model))
      ##summary.models$selected <- NULL
      str.prediced.data <- unlist(lapply(rownames(summary.models), function(m) Vector.ToString(model@models[[m]]$prediction)))
      str.residuals.data <- unlist(lapply(rownames(summary.models), function(m) Vector.ToString(round(model@models[[m]]$Residuals,2))))
      summary.models = cbind(item_id=id, model=rownames(summary.models), summary.models, predictedData=str.prediced.data, residuals=str.residuals.data)
     }

    ## TODO: fails if normalized data is empty
    ## ./strategico.R --cmd eval_items --id.list 5 -n sample
    tablename = DB.GetTableNameSummary(project.name, value)
    DB.DeleteAndInsertData(onerow.summ, tablename=tablename, id=id, rownames="id", addPK=TRUE, db.channel=db.channel)

    if (!is.null(model@BestModel)) {
      tablename = DB.GetTableNameSummaryModels(project.name, value)
      DB.DeleteAndInsertData(summary.models, tablename=tablename, id=id, id.name="item_id", append=TRUE,
                    rownames=NULL, addPK=FALSE, db.channel=db.channel)
    }
  }

  ## ###################################################################################
  ## Saving Predicted Data
  ## ###################################################################################
  ##__##logger(DEBUG, "Saving results for all models")

  
  if (is.null(model@BestModel)) {
    ##__##logger(WARN, "NO BestModel found ;-(")
    data.predicted <- data.frame(V=rep(0, param$n.ahead))
  }
  else {
    ##__##logger(WARN, paste("Best Model is ", model@BestModel))
    data.predicted <- data.frame(V=model@models[[model@BestModel]]$prediction)
  }

  result <- cbind(id, PERIOD=predicted.periods, data.predicted)
  colnames(result) <- c("id", "PERIOD", "V")

   tablename = DB.GetTableNameResults(project.name, value)  
   DB.DeleteAndInsertData(data=result, tablename=tablename, id=id, id.name="id", append=TRUE,
                  rownames=FALSE, addPK=FALSE, db.channel=db.channel)

  
  if (!is.null(model@BestModel)) {
    ## ###################################################################################
    ## Creating Saving Images
    ## ###################################################################################
    if ("images"%in%project.config$save) {
      PlotLtpResults(model, directory=output.path)
    }
    
    ## ###################################################################################
    ## Creating and Saving Reports
    ## ###################################################################################
    if ("report"%in%project.config$save) {
      ltp.HTMLreport(obj=model, id=id, value=value, value.description=project.config$values[value], directory=output.path)
    }
  }
  result
}

