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

suppressPackageStartupMessages(library(ltp))

ltp.Item.EvalDataByValue <- function(project.name, id, item.data, value, output.path=".", param=NULL, project.config, db.channel) {
  loginfo( paste("Running function", project.config$eval.function, "for item id =", id, "and value =", value))
  
  rule.noMaxJumpOver.position <- strtoi(gsub("V", "", value))
  rule.noMaxJumpOver <- param$rule.noMaxJumpOver[rule.noMaxJumpOver.position]
  logdebug(sprintf("rule.noMaxJumpOver=%s", rule.noMaxJumpOver))
  
  model <- ltp(product = item.data, rule=param$rule,
               ruleSetting=list(rule.noMaxCVOver=param$rule.noMaxCVOver,rule.noMaxJumpOver=rule.noMaxJumpOver),
               try.models = param$try.models, n.ahead = param$n.ahead, n.min = param$n.min, 
               NA2value = param$NA2value, range = param$range, period.freq = project.config$period.freq, 
               period.start = project.config$period.start, period.end = project.config$period.end,
               diff.sea=1,diff.trend=1,max.p=2,max.q=1,max.P=0,max.Q=1, logtransform.es=FALSE,
               increment=1 ,idDiff = FALSE, idLog = FALSE,
               formula.right.lm = param$formula.right.lm,stepwise=param$stepwise,
               logtransform=param$logtransform, negTo0=param$negTo0, toInteger=param$toInteger,
               naive.values=param$naive.values,
               naive.ifConstantLastValues=param$naive.ifConstantLastValues,
               naive.ifConstant0LastValues=param$naive.ifConstant0LastValues)

  models.names <- ltp.GetModels("name")

  ## ###################################################################################
  ## Saving model.RData
  ## ###################################################################################

  if ("model" %in% project.config$save) {
    dir.create(output.path, showWarnings = FALSE, recursive = TRUE)
    filename <- paste(output.path, "/model.RData", sep = "")
    logdebug( paste("Saving Model to file", filename))
    save(file=filename, model)
  }

  ## ###################################################################################
  ## Saving Normalized Data
  ## ###################################################################################
    
  logdebug( "Normalized data:")
  logdebug( model@values)
  if ( nrow(model@values) == 0) {
    ##__##logwarn( "No records in normalized data. No saving to DB")
    skip=TRUE
  } else {
    normalized.periods <- rownames(model@values)
    normalized.data <- data.frame(item_id=id, PERIOD=normalized.periods, V=model@values$V)
    tablename = DB.GetTableNameNormalizedData(project.name, value)
    DB.DeleteAndInsertData(normalized.data, tablename=tablename, id=id, db.channel=db.channel)
  }
    
  if (is.null(model@BestModel)) {
    logwarn( "Best model is null. No saving to DB")
    return(2)
  } 

  
  logdebug( "Calculating the predicted periods")
  predicted.periods <-Period.BuildRange(period.start=project.config$period.end,
                                          period.freq=project.config$period.freq,
                                          n=param$n.ahead, shift=1)
  ## ###################################################################################
  ## Saving Summary Data
  ## ###################################################################################
  
  if ("summary" %in% project.config$save) {
    logdebug( "Calculating the onerow.summ table")
    onerow.summ = ltp.BuildOneRowSummary(id=id, model=model, param)
    onerow.summ <- cbind(item_id=id, onerow.summ)
    tablename = DB.GetTableNameSummary(project.name, value)
    DB.DeleteAndInsertData(onerow.summ, tablename=tablename, id=id, db.channel=db.channel)

    if (!is.null(model@BestModel)) {
      summary.models <- data.frame(ltp.GetModelsComparisonTable(model))
      summary.models = cbind(item_id=id, model=rownames(summary.models), summary.models)
     }

    if (!is.null(model@BestModel)) {
      tablename = DB.GetTableNameSummaryModels(project.name, value)
      DB.DeleteAndInsertData(summary.models, tablename=tablename, id=id, append=TRUE,
                             db.channel=db.channel)
    }
  }

  ## ###################################################################################
  ## Saving Results Data
  ## ###################################################################################
  logdebug( "Saving results for all models")

  
  if (is.null(model@BestModel)) {
    logwarn( "NO BestModel found ;-(")
    results <- NULL
  }
  else {
    loginfo( paste("Best Model is ", model@BestModel))
    all.results <- NULL
    all.residuals <- NULL
    for (m in names(model@models)) {
      logdebug( paste("Retreiving results and residuals for model", m))
      predictions <- as.vector(model@models[[m]]$prediction)
      residuals <- as.vector(model@models[[m]]$Residuals)

      if (length(predictions) == 0) {
       logwarn( paste("No predictions for model", m, ".Skipping it"))
      } else {
        model.results <- data.frame(
           item_id=id,
           model=m,
           PERIOD=predicted.periods,
           V=predictions)
        all.results <- rbind(all.results, model.results) 
      }

      if (length(residuals) == 0) {
       logdebug( paste("No residuals for model", m, ".Skipping it"))
      ##} else if (length(residuals) != length(normalized.periods)) {
      ## logwarn( paste("Different number of residuals for model", m, ".Skipping it"))
      } else {
        model.residuals <- data.frame(
           item_id=id,
           model=m,
           PERIOD=normalized.periods,
           V=trunc(residuals, 1))
        all.residuals <- rbind(all.residuals, model.residuals) 
      }
    } ## end for model
  } ## end if


   if (!is.null(all.results)) {
     colnames(all.results) <- c("item_id", "model", "PERIOD", "V")
     tablename = DB.GetTableNameResults(project.name, value)  
     DB.DeleteAndInsertData(data=all.results, tablename=tablename, id=id, append=TRUE,
                            db.channel=db.channel)
   }

   if (!is.null(all.residuals)) {
     colnames(all.residuals) <- c("item_id", "model", "PERIOD", "V")
     tablename = DB.GetTableNameResiduals(project.name, value)  
     DB.DeleteAndInsertData(data=all.residuals, tablename=tablename, id=id, append=TRUE,
                            db.channel=db.channel)
   }
  
  if (!is.null(model@BestModel)) {
    ## ###################################################################################
    ## Creating Saving Images
    ## ###################################################################################
    if ("images"%in%project.config$save) {
      dir.create(output.path, showWarnings = FALSE, recursive = TRUE)
      PlotLtpResults(model, directory=output.path)
    }
    
    ## ###################################################################################
    ## Creating and Saving Reports
    ## ###################################################################################
    if ("report"%in%project.config$save) {
      dir.create(output.path, showWarnings = FALSE, recursive = TRUE)
      ltp.HTMLreport(obj=model, id=id, value=value, value.description=project.config$values[value], directory=output.path)
    }
  }
  0 
}

