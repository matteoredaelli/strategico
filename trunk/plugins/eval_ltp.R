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

MySource(filename="ltp.R", file.path=GetPluginsPath())

ltp.BuildOneRowSummary <- function(id, model, param) {
  models.names <- ltp.GetModels("name")
  no.values <- nrow(model$values)
  
  return.code <- 0

  if (is.null(model$BestModel))
    return.code <- 1

  if (no.values == 0)
    return.code <- 2
  
  stats=as.list(rep(NA,11))
  names(stats)=c("BestModel",
         "Points","NotZeroPoints","LastNotEqualValues",
         "MeanPredicted","MeanValues","MeanPredictedRatioMeanValues","SdPredictedRatioSdValues",
         "BestAICNoOutRangeExclude","BestICNoOutRangeExclude","Timestamp")
  ##stats["id"] <- id
  ##mean values (ie observed data)
  stats["MeanValues"]=mean(model$values,na.rm=TRUE)
  ##nunb of points (observations)
 

  stats["Points"] <- no.values
  ##non zero values
  stats["NotZeroPoints"]=ifelse(no.values==0,0, sum(model$values!=0))
  stats["BestModel"] = ifelse(is.null(model$BestModel), NA, model$BestModel)
  stats["SuggestedModel"] =  stats["BestModel"]
  stats["Timestamp"] = Sys.time()
  stats["TotModels"] = length(param$try.models)
  stats["Parameters"] = Param.ToString(param)
  stats["ReturnCode"] = return.code
  stats["Run"] = 0

  if (!is.null(model$BestModel) & (stats["MeanValues"] != 0) ) {
    ##stats[c("R2","AIC","maxJump","VarCoeff")]=round(unlist(model[[model$BestModel]][c("R2","AIC","maxJump","VarCoeff")]),4)
    ##stats["ICwidth"] = round(model[[model$BestModel]][["IC.width"]],0)

    ##find (che cum sum of) not equal (ie constant) consecutive values
    temp=cumsum((model$values[-1,]-model$values[-no.values,])==0)
    ##length of last not-constant consecutives serie of values
    stats["LastNotEqualValues"]=sum(temp==max(temp))-1
		
    ##mean predicted
    stats["MeanPredicted"]=mean(model[[model$BestModel]]$prediction,na.rm=T)
    ##mean predicted over mean values (ie observed data)
    stats["MeanPredictedRatioMeanValues"]=stats[["MeanPredicted"]]/stats[["MeanValues"]]
    ##and rounding
    stats[c("MeanPredicted","MeanValues","MeanPredictedRatioMeanValues")]=lapply(stats[c("MeanPredicted","MeanValues","MeanPredictedRatioMeanValues")],round,3)
    ##sd predicted over sd values (ie observed data)
    stats["SdPredictedRatioSdValues"]=round(sd(model[[model$BestModel]]$prediction,na.rm=T)/sd(model$values),3)
		
    ##Best Model if not exclusion rule were performed
    st=names(which.min(unlist(lapply(model[models.names],function(x) x$AIC))))
    stats["BestAICNoOutRangeExclude"]=ifelse(is.null(st),"None",st)
    st=names(which.min(unlist(lapply(model[models.names],function(x) x$IC.width))))
    stats["BestICNoOutRangeExclude"]=ifelse(is.null(st),"None",st)
    ##note: stat is changed from numeric to string
  }

  
  ##clean out the (possible) Inf values
  stats= lapply(stats,function(x) ifelse(is.numeric(x) & (!is.finite(x)), NA,x))	
  summ=data.frame(stats)
  rownames(summ) <- c(id)
  summ
}

ltp.Item.EvalDataByValue <- function(project.name, id, item.data, value, output.path=".", param=NULL, project.config, db.channel) {

  model <- ltp(product = item.data, rule=param$rule, rule.noMaxOver=param$rule.noMaxOver,
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
    logger(DEBUG, paste("Saving Model to file", filename))
    save(file=filename, model)
  }

  ## ###################################################################################
  ## Saving Normalized Data
  ## ###################################################################################
    
  if ( nrow(model$values) == 0) {
    logger(WARN, "No records in normalized data. No saving to DB")
    normalized.data <- ""
    normalized.period <- ""
  } else {
    normalized.data <- Vector.ToString(model$values$V)
    normalized.periods <- Vector.ToString(rownames(model$values))
  }
    
  if (is.null(model$BestModel)) {
    logger(WARN, "Best model is null. No saving to DB")
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
    
    if (!is.null(model$BestModel)) {
      summary.models <- data.frame(ltp.GetModelsComparisonTable(model))
      ##summary.models$selected <- NULL
      str.prediced.data <- unlist(lapply(rownames(summary.models), function(m) Vector.ToString(model[[m]]$prediction)))
      str.residuals.data <- unlist(lapply(rownames(summary.models), function(m) Vector.ToString(round(model[[m]]$Residuals,2))))
      summary.models = cbind(item_id=id, model=rownames(summary.models), summary.models, predictedData=str.prediced.data, residuals=str.residuals.data)
     }

    ## TODO: fails if normalized data is empty
    ## ./strategico.R --cmd eval_items --id.list 5 -n sample
    tablename = DB.GetTableNameSummary(project.name, value)
    DB.DeleteAndInsertData(onerow.summ, tablename=tablename, id=id, rownames="id", addPK=TRUE, db.channel=db.channel)

    if (!is.null(model$BestModel)) {
      tablename = DB.GetTableNameSummaryModels(project.name, value)
      DB.DeleteAndInsertData(summary.models, tablename=tablename, id=id, id.name="item_id", append=TRUE,
                    rownames=NULL, addPK=FALSE, db.channel=db.channel)
    }
  }

  ## ###################################################################################
  ## Saving Predicted Data
  ## ###################################################################################
  logger(DEBUG, "Saving results for all models")

  data.predicted <- NULL
  prediction.null <- cbind(id, NA,PERIOD=predicted.periods, V=rep(0, param$n.ahead))
  
  if (is.null(model$BestModel)) {
    logger(WARN, "NO BestModel found ;-(")
    result <- data.frame(rep(0, param$n.ahead))
    data.predicted <- prediction.null
  }
  else {
    logger(WARN, paste("Best Model is ", model$BestModel))
    result <- data.frame(model[[model$BestModel]]$prediction)
  }
  
  if (!is.null(model$BestModel)) {
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
      ltp.HTMLreport(model, id, value, project.config$values[value], param, directory=output.path)
    }
  }
  result
}

##################################
 ltp.GetModels <- function(what=NULL) {
	model=data.frame(id=c("lm", "arima","es", "trend", "mean","naive"),
			name=c("Linear", "Arima", "ExpSmooth", "Trend", "Mean","Naive"),
			color = c("green", "red", "blue", "gray", "black","yellow"),
			legend= c("Linear","Arima" , "Exp.Smooth" , "Trend" ,"Mean", "Naive"))
	rownames(model)=model$id
	
	if(!is.null(what)) { 
		names=model$id
		model=as.character(model[,what])
		names(model)=names
		}
  
  model
}
#es: ltp.GetModels("es")


ltp.GetModelsComparisonTable <-  function(obj) {
  
  col.names <- c("formula", "R2","AIC","IC.width","maxJump","VarCoeff")

  if (is.null(obj$BestModel)) {
    ReporTable <- cbind( rep(NA, length(col.names)))
    colnames(ReporTable) <- col.names
    rownames(ReporTable) <- "None"
    return (ReporTable)
  }
  
  ReporTable = matrix("--",6,6)

  model.names <- ltp.GetModels("name")
  #model.names <- model.names[! model.names == "Naive"]
  
  colnames(ReporTable) <- col.names
  rownames(ReporTable) <- model.names #c("Linear Model","Arima", "Exponential Smooth","Trend","Mean")

  indicator.list <- c("R2","AIC", "IC.width","maxJump","VarCoeff")
  
  if(!is.null(obj$ExpSmooth)) {
    terms=sapply(c("drift","seasonality"),
      function(compon){ if(obj$ExpSmooth$model[compon]=="none") return() 
                        compon})
    terms=terms[!sapply(terms,is.null)] 
    
    es.string=paste( "level",sep="+", paste(terms,collapse=ifelse(length(grep("multiplicative",obj$ExpSmooth$model["seasonality"])>0),"*","+")))
  }
  ## TODO: pay attention: the list of models is important... maybe it is better to use explicit coordinates ReporTable["Arima"][1] ..
  ReporTable[,1] <- 
    c(ifelse(is.null(obj$Linear),"--", gsub("~","=",gsub("stima$qta","y",as.character(obj$Linear$model$call[2]),fixed=TRUE))),
      ##paste("Y=",paste(attributes(obj$Linear$model$call[[2]])$term.labels,collapse="+"),sep="")), 
      ifelse(is.null(obj$Arima),"--",
             ifelse(length(obj$Arima$model$coef)==0,
                    "-constant-",
                    paste(obj$Arima$model$series,"=", paste(names(obj$Arima$model$coef), collapse = "+"),sep=""))), 
      ifelse(is.null(obj$ExpSmooth),"--", es.string ),
      ifelse(is.null(obj$Trend),"--",paste("y=",paste(attributes(obj$Trend$model$call[[2]])$term.labels,collapse="+"),sep="")),
      ifelse(is.null(obj$Mean),"--",paste("y=",paste(attributes(obj$Mean$model$call[[2]])$term.labels,collapse="+"),sep="")),
      ifelse(is.null(obj$Naive),"--","Fixed Values")   )
  
  temp <- rbind(unlist(obj$Linear[indicator.list]), unlist(obj$Arima[indicator.list]), 
    unlist(obj$ExpSmooth[indicator.list]),unlist(obj$Trend[indicator.list]),unlist(obj$Mean[indicator.list]),unlist(obj$Naive[indicator.list]))
  colnames(temp)= indicator.list
  temp[,"R2"]=round(temp[,"R2"],4)	
  temp[,"AIC"]=round(temp[,"AIC"],2)
  temp[,"IC.width"]=round(temp[,"IC.width"],0)
  temp[,"maxJump"]=round(temp[,"maxJump"],3)
  temp[,"VarCoeff"]=round(temp[,"VarCoeff"],3)

  ReporTable[which(!(ReporTable[,1]=="--")),indicator.list] = as.matrix(temp)
  ReporTable=as.data.frame(ReporTable)
  ReporTable = ReporTable[which(!(ReporTable[,1]=="--")),]
  ReporTable
}

