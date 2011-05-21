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

MySource(filename="ltp.R", file.path=GetPluginsPath())

ltp.BuildOneRowSummary <- function(id, model, manual.model, param, return.code) {
  models.names <- ltp.GetModels()[,"name"]
  
	stats=as.list(rep(NA,16))
	names(stats)=c("BestModel","R2","AIC","ICwidth","maxJump",
               "VarCoeff","Points","NotZeroPoints","LastNotEqualValues",
               "MeanPredicted","MeanValues","MeanPredictedRatioMeanValues","SdPredictedRatioSdValues",
               "BestAICNoOutRangeExclude","BestICNoOutRangeExclude","Timestamp")
        #stats["id"] <- id
	#mean values (ie observed data)
	stats["MeanValues"]=mean(model$values,na.rm=TRUE)
	#nunb of points (observations)
	stats["Points"]=nrow(model$values)
	#non zero values
	stats["NotZeroPoints"]=ifelse(nrow(model$values)==0,0, sum(model$values!=0))

	if(!is.null(model$BestModel)){
		stats[c("R2","AIC","maxJump","VarCoeff")]=round(unlist(model[[model$BestModel]][c("R2","AIC","maxJump","VarCoeff")]),4)
		stats["ICwidth"] = round(model[[model$BestModel]][["IC.width"]],0)

		#find (che cum sum of) not equal (ie constant) consecutive values
		temp=cumsum((model$values[-1,]-model$values[-nrow(model$values),])==0)
		#length of last not-constant consecutives serie of values
		stats["LastNotEqualValues"]=sum(temp==max(temp))-1
		
		#mean predicted
		stats["MeanPredicted"]=mean(model[[model$BestModel]]$prediction,na.rm=T)
		#mean predicted over mean values (ie observed data)
		stats["MeanPredictedRatioMeanValues"]=stats[["MeanPredicted"]]/stats[["MeanValues"]]
		#and rounding
		stats[c("MeanPredicted","MeanValues","MeanPredictedRatioMeanValues")]=lapply(stats[c("MeanPredicted","MeanValues","MeanPredictedRatioMeanValues")],round,3)
		#sd predicted over sd values (ie observed data)
		stats["SdPredictedRatioSdValues"]=round(sd(model[[model$BestModel]]$prediction,na.rm=T)/sd(model$values),3)
		
		#Best Model if not exclusion rule were performed
		st=names(which.min(unlist(lapply(model[models.names],function(x) x$AIC))))
		stats["BestAICNoOutRangeExclude"]=ifelse(is.null(st),"None",st)
		st=names(which.min(unlist(lapply(model[models.names],function(x) x$IC.width))))
		stats["BestICNoOutRangeExclude"]=ifelse(is.null(st),"None",st)
		#note: stat is changed from numeric to string
		stats["BestModel"] = model$BestModel
	}

	stats["Timestamp"] = as.character(Sys.time())
	stats["ManualModel"] = manual.model
	stats["Parameters"] = Param.ToString(param)
	stats["ReturnCode"] = return.code
	stats["Run"] = 0
	
	#clean out the (possible) Inf values
	stats= lapply(stats,function(x) ifelse(is.numeric(x) & (!is.finite(x)), NA,x))
	
	summ=as.data.frame(stats)
        rownames(summ) <- c(id)
        summ
}

ltp.Item.EvalDataByValue <- function(project.name, id, item.data, value, output.path=".", param=NULL, project.config, db.channel) {

  model <- ltp(product = item.data, rule=param$rule, rule.noMaxOver=param$rule.noMaxOver,
               try.models = param$try.models, n.ahead = param$n.ahead, n.min = param$n.min, 
               NA2value = param$NA2value, range = param$range, period.freq = project.config$period.freq, 
               period.start = project.config$period.start, period.end = project.config$period.end,diff.sea=1,diff.trend=1,max.p=2,max.q=1,max.P=0,max.Q=1, logtransform.es=FALSE , increment=1 ,idDiff = FALSE, idLog = FALSE,
               formula.right.lm = param$formula.right.lm,stepwise=param$stepwise,logtransform=param$logtransform, negTo0=param$negTo0)

  models.names <- ltp.GetModels()[,"name"]
  
  ## write results in .RData
  if ("model" %in% project.config$save)
    save(file =  paste(output.path, "/model.RData", sep = ""), model)

  data.predicted <- NULL
  prediction.null <- rep(0, param$n.ahead)
  for (m in models.names) {
    if (is.null(model[[m]]) | is.null(model[[m]]$prediction))
      data.predicted <- cbind(data.predicted, prediction.null)
    else
      data.predicted <- cbind(data.predicted, model[[m]]$prediction)
  }
  
  data.predicted <- data.frame(data.predicted)
  predictions.periods <- Period.BuildRange(period.start=project.config$period.end,
                                           period.freq=project.config$period.freq,
                                           n=param$n.ahead, shift=1)
  
  rownames(data.predicted) <- predictions.periods
  colnames(data.predicted) <- models.names
  ##names(data.predicted) <- models.names
  
  if (!is.null(model$BestModel)) {
    logger(WARN, paste("Best Model is ", model$BestModel))
    result <- data.frame(model[[model$BestModel]]$prediction)
    return.code <- 0
    
    ## write report
    if("images"%in%project.config$save) {
      PlotLtpResults(model, directory=output.path)
    }
    
    if("report"%in%project.config$save) {
      ##html.form.eval <- GetStrHTMLformItem.Eval(project.path, .Item.GetPath(keys), value, param)
      ltp.HTMLreport(model, id, value, project.config$values[value], param, directory=output.path)
    }
  }
  else {
    return.code <- 1 
    logger(INFO, "Strategico didn't select any BestModel")
    result <- data.frame(rep(0, param$n.ahead))
  }

  
  rownames(result) <- predictions.periods
  colnames(result) <- "V"
  
  data.normalized = model$values[, , drop = FALSE]
  colnames(data.normalized) <- "V"
  
  if ("csv" %in% project.config$save) {
    write.csv(data.predicted, file = paste(output.path, "/item-results.csv", sep = ""))
  }
  if ("data_db" %in% project.config$save) {
    tablename = DB.GetTableNameNormalizedData(project.name, value)
    Item.Db.SaveData(id=id, data=data.normalized, tablename=tablename, db.channel=db.channel)
      
    tablename = DB.GetTableNameResults(project.name, value)
    Item.Db.SaveData(id=id, data=data.predicted, tablename=tablename, db.channel=db.channel)

  }
  ## create a single-line summary with short summary (to be merged in report-summary.csv or in the DB, see below)
  if (("summary_db" %in% project.config$save) | ("summary_csv" %in% project.config$save)) {
    manual.model <- ifelse(length(param$try.models) > 1, FALSE, TRUE)
    onerow.summ = ltp.BuildOneRowSummary(id=id, model=model, manual.model, param, return.code)
  }
  if ("summary_csv" %in% project.config$save) {
    write.table(file = paste(output.path, "/item-summary.csv", sep = ""),
                onerow.summ, sep = ",", row.names = FALSE, quote = TRUE, col.names = FALSE)
  }
  if ("summary_db" %in% project.config$save) {
    tablename = DB.GetTableNameSummary(project.name, value)
    DB.ImportData(onerow.summ, tablename=tablename, id=id, rownames="id", addPK=TRUE, db.channel=db.channel)
  }

  result
}
 
ltp.GetModels <- function() {
  models <- rbind(
                  c("linear", "LinearModel", "green"),
                  c("arima", "Arima", "red"),
                  c("es", "ExponentialSmooth", "blue"),
                  c("trend", "Trend", "gray"),
                  c("mean", "Mean", "black")
                  )
  colnames(models) <- c("id", "name", "color")
  models
}

ltp.GetModelsComparisonTable <-  function(obj) {
  
  ReporTable = cbind(matrix("--",5,6),"")
  colnames(ReporTable) = c("model", "R2","AIC","IC.width","maxJump","VarCoeff","selected")
  rownames(ReporTable) = models.names <- ltp.GetModels()[,"name"]

  
  if(!is.null(obj$ExponentialSmooth)) {
    terms=sapply(c("drift","seasonality"),
      function(compon){ if(obj$ExponentialSmooth$model[compon]=="none") return() 
                        compon})
    terms=terms[!sapply(terms,is.null)] 
    
    es.string=paste( "level",sep="+", paste(terms,collapse=ifelse(length(grep("multiplicative",obj$ExponentialSmooth$model["seasonality"])>0),"*","+")))
  }
  
  ReporTable[, 1] = c(ifelse(is.null(obj$LinearModel),"--",	gsub("~","=",gsub("stima$qta","y",as.character(obj$LinearModel$model$call[2]),fixed=TRUE))),#paste("Y=",paste(attributes(obj$LinearModel$model$call[[2]])$term.labels,collapse="+"),sep="")), 
              ifelse(is.null(obj$Arima),"--",ifelse(length(obj$Arima$model$coef)==0,"-constant-",paste(obj$Arima$model$series,"=",paste(names(obj$Arima$model$coef), collapse = "+"),sep=""))), 
              ifelse(is.null(obj$ExponentialSmooth),"--", es.string ),
              ifelse(is.null(obj$Trend),"--",paste("y=",paste(attributes(obj$Trend$model$call[[2]])$term.labels,collapse="+"),sep="")),
              ifelse(is.null(obj$Mean),"--",paste("y=",paste(attributes(obj$Mean$model$call[[2]])$term.labels,collapse="+"),sep="")) )
  temp=rbind(unlist(obj$LinearModel[c( "R2","AIC", "IC.width","maxJump","VarCoeff")]), unlist(obj$Arima[c( "R2", "AIC","IC.width","maxJump","VarCoeff")]), 
  unlist(obj$ExponentialSmooth[c("R2", "AIC", "IC.width","maxJump","VarCoeff")]),unlist(obj$Trend[c("R2", "AIC", "IC.width","maxJump","VarCoeff")]),unlist(obj$Mean[c("R2", "AIC", "IC.width","maxJump","VarCoeff")]))
  colnames(temp)= c("R2", "AIC", "IC.width","maxJump","VarCoeff")
  

  temp[,"R2"]=round(temp[,"R2"],4)	
  temp[,"AIC"]=round(temp[,"AIC"],2)
  temp[,"IC.width"]=round(temp[,"IC.width"],0)
  temp[,"maxJump"]=round(temp[,"maxJump"],3)
  temp[,"VarCoeff"]=round(temp[,"VarCoeff"],3)

  ReporTable[which(!(ReporTable[,1]=="--")),c("R2", "AIC", "IC.width","maxJump","VarCoeff")] = as.matrix(temp)
  ReporTable=as.data.frame(ReporTable)
  levels(ReporTable$selected)=c("","BEST")
  ReporTable[obj$BestModel,"selected"]="BEST"

  ReporTable
}

  
