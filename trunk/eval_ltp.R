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

EvalItemByValue <- function(project.path, keys, item.data, value, param=NULL) {
  
  param=c(param,CONFIG$param[setdiff(names(CONFIG$param),names(param))])


### optimize with ltp
  
  model <- ltp(product = item.data[, value, drop = FALSE], 
               try.models = param$try.models, n.ahead = param$n.ahead, n.min = param$n.min, 
               NA2value = param$NA2value, range = param$range, period.freq = CONFIG$period.freq, 
               period.start = CONFIG$period.start, period.end = CONFIG$period.end,diff.sea=1,diff.trend=1,max.p=2,max.q=1,max.P=0,max.Q=1, logtransform.es=FALSE , increment=1 ,idDiff = FALSE, idLog = FALSE,
               formula.right.lm = param$formula.right.lm,stepwise=param$stepwise,logtransform=param$logtransform)
  
  directory = .GetItemPath(keys,project.path,paste("report-",CONFIG$values[value], sep = ""))
  dir.create(directory, showWarnings = FALSE)

  ## write results in .RData
  if("Rdata"%in%CONFIG$save) save(file =  paste(directory, "/item-", CONFIG$values[value], "-results.RData", sep = ""), model)
  if (!is.null(model$BestModel)) {
    ## write data and prediction in .csv
    prediction= data.frame(model[[model$BestModel]]$prediction)
    rownames(prediction)=sapply (0:(length(model[[model$BestModel]]$prediction)-1),function(i) paste(.incSampleTime(now=start(model[[model$BestModel]]$prediction), period.freq = frequency(model[[model$BestModel]]$prediction), increment = i),collapse="-"))
    colnames(prediction)=colnames(model$values)
    keydf = data.frame(t(keys)) 
    ##names(keydf) = names(CONFIG$keys)
    data = cbind(keydf, rbind(model$values[, , drop = FALSE], prediction))
    if("csv"%in%CONFIG$save)  write.csv(file = paste(directory, "/item-", CONFIG$values[value], "-results.csv", sep = ""), data)
    ## write report
    if("report"%in%CONFIG$save) ltp.HTMLreport(model, keys, value,param,directory)
  }
  else {
    print("no data")
  }
  ## write a single-line   item*.summary with short summary \t(to be merged in report-summary.csv)
  if("summary"%in%CONFIG$save) {
    onerow.summ = t(c(FALSE, keys, ifelse(rep(is.null(model$BestModel),3),rep("-",3),model[[model$BestModel]][c("AIC","IC.width","maxJump")]), nrow(model$values)))
    write.table(file = paste(directory, "/item-", CONFIG$values[value], ".summary", sep = ""), onerow.summ, sep = ",", row.names = FALSE, quote = FALSE, col.names = FALSE)
  }
}
