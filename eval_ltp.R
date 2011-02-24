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

####################### prediction
## fa tutto: richiama ltp o chi per essa, scrive il report e salva i dati.

source("ltp.R")

EvalItemDataByValue <- function(project.path, keys, item.data, value, output.path=".", param=NULL) {
  
  ##item.data <- as.vector(item.data)
  param=c(param,CONFIG$param[setdiff(names(CONFIG$param),names(param))])
  ## for (param.name in names(CONFIG$param)) {
  ## if (is.character(CONFIG$param[[param.name]]) & (length(CONFIG$param[[param.name]]) ==  1)) {
  ## CONFIG$param[[param.name]] = paste("\"", CONFIG$param[[param.name]], "\"", sep = "")
  ## }
  ## if (!exists(param.name, mode = "numeric")) 
  ## eval(parse(text = paste(param.name, "=", CONFIG$param[param.name], sep = "")))
  ## }

  ## optimize with ltp
  ## LTPCONFIG=CONFIG$param
  model <- ltp(product = item.data[, value, drop = FALSE], criterion=param$criterion, criterionExcludeMaxGreaterThan=param$criterionExcludeMaxGreaterThan,
               try.models = param$try.models, n.ahead = param$n.ahead, n.min = param$n.min, 
               NA2value = param$NA2value, range = param$range, period.freq = CONFIG$period.freq, 
               period.start = CONFIG$period.start, period.end = CONFIG$period.end,diff.sea=1,diff.trend=1,max.p=2,max.q=1,max.P=0,max.Q=1, logtransform.es=FALSE , increment=1 ,idDiff = FALSE, idLog = FALSE,
               formula.right.lm = param$formula.right.lm,stepwise=param$stepwise,logtransform=param$logtransform, negToZero=param$negToZero)
                                        # model <- ltp(product=item.data[,value,drop=FALSE], try.model=LTPCONFIG$try.model, n.ahead=LTPCONFIG$n.ahead,period.freq=CONFIG$period.freq,period.start=CONFIG$period.start,period.end=CONFIG$period.end,n.min=LTPCONFIG$n.min,
                                        # NA2value=LTPCONFIG$NA2value, range=LTPCONFIG$range)
  
  ## write results in .RData
  if("Rdata"%in%CONFIG$save) save(file =  paste(output.path, "/item-results.RData", sep = ""), model)
  if (!is.null(model$BestModel)) {
    ## write data and prediction in .csv
    prediction= data.frame(model[[model$BestModel]]$prediction)
    rownames(prediction)=sapply (0:(length(model[[model$BestModel]]$prediction)-1),function(i) paste(.incSampleTime(now=start(model[[model$BestModel]]$prediction), period.freq = frequency(model[[model$BestModel]]$prediction), increment = i),collapse="-"))
    #keydf = data.frame(t(keys)) 
                                        #names(keydf) = names(CONFIG$keys)
    
    ## write report
    if("report"%in%CONFIG$save) ltp.HTMLreport(model, keys, value, CONFIG$values[value], param,output.path)
  }
  else {
    print("No data")
    prediction=data.frame(rep(0, param$n.ahead))
        rownames(prediction)=
        sapply (1:CONFIG$param$n.ahead, function(i) paste(.incSampleTime(now=CONFIG$period.end, period.freq = CONFIG$period.freq, increment = i),collapse="-"))

  }
    #colnames(prediction)=colnames(model$values)
    colnames(prediction)=value
    fullkeys <- append(keys, rep("", length(CONFIG$keys) - length(keys)))
    names(fullkeys) <- names(CONFIG$keys)
    if("fullcsv"%in%CONFIG$save) {
      #data = cbind(keydf, rbind(model$values[, , drop = FALSE], prediction))
      #data = rbind(model$values[, , drop = FALSE], prediction)
      data = rbind(item.data[, value, drop = FALSE], prediction)
      write.csv(data, file = paste(output.path, "/item-results.csv", sep = ""))
    }
    if("csv"%in%CONFIG$save) {
      data = prediction
      write.csv(data, file = paste(output.path, "/item-results.csv", sep = ""))
    }
    if("t_csv"%in%CONFIG$save) {
      data = t(prediction)
      write.csv(data, file = paste(output.path, "/item-results.csv", sep = ""), row.names = FALSE)
    }
    if("db"%in%CONFIG$save) {
      keydf = data.frame(t(fullkeys)) 
      names(keydf) = names(CONFIG$keys)
      data = rbind(item.data[, value, drop = FALSE], prediction)
      #data = cbind(keydf, prediction)
      data = cbind(keydf, data)
      data$PERIOD = rownames(data)

      tablename = paste(CONFIG$project.name, value, sep="_")
      ExportDataToDB(data, tablename, fullkeys)
    }
  ## create a single-line summary with short summary (to be merged in report-summary.csv or in the DB, see below)
  if(("summary_db"%in%CONFIG$save) | ("summary_csv"%in%CONFIG$save)) {
    onerow.summ = onerow.summary(fullkeys, model)
  }
  if("summary_csv"%in%CONFIG$save) {
    write.table(file = paste(output.path, "/item-summary.csv", sep = ""), onerow.summ, sep = ",", row.names = FALSE, quote = FALSE, col.names = FALSE)
  }
  if("summary_db"%in%CONFIG$save) {
      tablename = paste(CONFIG$project.name, value, "summary", sep="_")
      ExportDataToDB(onerow.summ, tablename, fullkeys)
  }
  prediction
}
