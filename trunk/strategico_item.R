#!/usr/bin/env Rscript
## This program is fre esoftware: you can redistribute it and/or modify
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

## Authors: L. Finos, M. Redaelli

## project name: strategico
## project website: http://code.google.com/p/strategico/
## created: 2011

Items.Eval <- function(project.name, id.list=c(), keys=NULL, values=NULL, param=NULL,
                      period.start=NULL, period.end=NULL,
                      project.config=NULL, db.channel) {
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name=project.name)
 
  if (is.null(values))
    values <- GetValueNames(project.config=project.config)

  for (id in id.list) {
    try(Item.Eval(project.name=project.name, id=id, keys=keys, values=values, param=param,
             period.start=period.start, period.end=period.end, 
             project.config=project.config, db.channel=db.channel))
  }
}

Item.EmptyFS <- function(project.name, id, value=NULL, recursive = TRUE) {
  if (is.na(id) | id < 0) {
    loginfo( paste("Invalid ID=", id))
  } else {
    project.path <- Project.GetPath(project.name)
    relative.path <- Item.GetRelativePath(id, value)
    item.path <- paste(project.path, relative.path, sep="/")
    loginfo( paste("Deleting Item path:", item.path))
    unlink(item.path, recursive=recursive)
  }
}

Item.Eval <- function(project.name, id=NULL, keys=NULL, values, param=NULL,
                     period.start=NULL, period.end=NULL,
                     project.config, db.channel) {

  for (i in 1:length(values)) {
    Item.EvalData(project.name=project.name, id=id, keys=keys, value=values[i], param=param,
                 period.start=period.start, period.end=period.end, 
                 project.config=project.config, db.channel=db.channel)
  }
}

Item.EvalChildren <- function(project.name, id, keys=NULL, values, param=NULL,
                     period.start=NULL, period.end=NULL,
                     project.config, db.channel) {
  
  id.list <- Item.GetChildren(id=id, keys=keys, project.name=project.name, db.channel=db.channel)

  if (!is.null(id.list))
    Items.Eval(project.name=project.name, id.list=id.list, values=values, param=param,
              period.start=period.start, period.end=period.end, 
              project.config=project.config, db.channel=db.channel)
}

Item.EvalData <- function(project.name, id=NULL, keys=NULL, item.data=NULL,
                         period.start=NULL, period.end=NULL, value,
                         param=NULL, project.config, db.channel) {
  loginfo( "++++++++++++++++++++++++Item.EvalData ++++++++++++++++++++++++")
  logwarn( paste("Project=", project.name, " Loading item ID=", id,
                     " VALUE=",
                     value, 
                     sep=""))

  if (!is.value(value, project.config=project.config)) {
    msg <- paste("Invalid value=", value, ". Skipping prediction") 
    logerror( msg)
    return(NULL)
  }
  
  if (is.null(item.data))
    item.data <- Item.GetData(project.name=project.name,
                                 project.config=project.config, id=id, keys=keys,
                                 period.start=period.start, period.end=period.end,
                                 value=value, db.channel=db.channel)

  if (is.null(item.data)) {
    loginfo( "Empty data: skipping prediction")
    return(NULL)
  }
  
  loginfo( paste("TS length=", nrow(item.data)))
  logdebug( item.data)

  if (nrow(item.data)==0) {
    loginfo( "Empty data: skipping prediction")
    return(NULL)
  }

  n.char <- nchar(project.config$period.freq)
  loginfo( paste("period.start=", Period.ToString(project.config$period.start, n.char=n.char),
                     " period.freq=", project.config$period.freq,
                     " period.end=", Period.ToString(project.config$period.end, n.char=n.char),
                     sep=""))
  
  if (is.null(id)) {
    logwarn( "ID is null, assigning a new value")
    id <- Item.GetNewID()
  } else {
    id <- as.integer(id)
  }
  
  ## param can be a string of parameters: in this case it must be converted to a list
  if (!is.null(param) & !is.list(param))
    param <- Param.EvalString(param)
  
  param <- Param.MergeWithDefault(project.config=project.config, param=param)

  logdebug( paste("Param= ", Param.ToString(param)))
  
  directory = Item.GetPath(project.name, id, value)
  ##dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  
  EvalFunction <- paste(project.config$eval.function,".Item.EvalDataByValue(project.name=project.name, id=id, item.data=item.data,
    value=value, output.path=directory, param=param, project.config=project.config, db.channel=db.channel)", sep="")

  prediction <- eval(parse(text=EvalFunction))
  logdebug( "RESULTS:")
  logdebug( rownames(prediction))
  logdebug( prediction)
  t(prediction)
}

Item.GetData <- function(project.name, project.config=NULL, id=NULL, keys=NULL, value="V1",
                         keys.na.rm=TRUE, period.start=NULL, period.end=NULL, db.channel) {

  ## id or keys must be not null
  
  if (is.null(keys) & is.null(id)) {
    msg <- "Cannot retrive Item data for missing ID and KEYS" 
    logerror( msg)
    return(NULL)
  }
  
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name=project.name)

  if (!is.value(value, project.config=project.config)) {
    msg <- paste("Invalid value=", value, ". No data to retreive") 
    logerror( msg)
    return(NULL)
  }

  if (is.null(keys)) {
    keys <- Item.GetKeys(id=id, project.name=project.name, db.channel=db.channel)

    ## now keys should not be null
     if (is.null(keys)) {
       msg <- paste("NO Keys NO data for id=", id) 
       logerror( msg)
       return(NULL)
     }
  }
            
  if (is.null(period.start)) period.start <- project.config$period.start
  if (is.null(period.end)) period.end <- project.config$period.end
  
  n.char <- nchar(project.config$period.freq)    
  string.period.start <- Period.ToString(period.start, n.char=n.char)
  string.period.end <- Period.ToString(period.end, n.char=n.char)

  filter.key <- BuildFilterWithKeys(key.values=keys, sep="=", collapse=" and ", na.rm=keys.na.rm)
  if (filter.key != "") filter.key <-  paste(filter.key, "and")
  
  filter.period <- paste("period >= '", string.period.start, "' and period <= '", string.period.end, "'", sep="")

  tablename <- DB.GetTableNameProjectData(project.name)
  sql_statement <- paste("select period, sum(", value, ") as V from", tablename, "where", filter.key, filter.period, "group by period", sep=" ")
  logdebug(sql_statement)
  records <- DB.RunSQLQuery(sql_statement=sql_statement, db.channel=db.channel)
  rownames(records) <- records$period
  records$period <- NULL
  records
}

Item.GetKeys <- function(id, project.name, db.channel) {
  tablename = DB.GetTableNameProjectItems(project.name)
  where.condition <- paste("item_id=", id, sep='')
  sql_statement <- paste("select * from", tablename, "where", where.condition, sep=" ")
  records <- DB.RunSQLQuery(sql_statement, db.channel=db.channel)
  if (nrow(records) > 0) {
    records$item_id <- NULL
    result <- as.vector(as.matrix(records[1,]))
  } else {
    logwarn( paste("No Keys found for ID =", id))
    result <- NULL
  }
  result
}

Item.GetParent <- function(id, keys=NULL, project.name=NULL, db.channel) {
  if (is.null(keys))
    keys <- Item.GetKeys(id, project.name=project.name, db.channel=db.channel)

  parent.key <- keys
  parent.key[length(keys)]=''
  
  result <- Project.GetIDs(keys=parent.key, project.name=project.name, keys.na.rm=FALSE, db.channel=db.channel)
  if (!is.na(result))
    result <- result[1]

  result
}

Item.GetChildren <- function(id, keys=NULL, project.name=NULL, db.channel) {
  if (is.null(keys))
    keys <- Item.GetKeys(id, project.name=project.name, db.channel=db.channel)
  
  ## TODO: now it could work only for keys with empty values at the end..."
  id.list <- Project.GetIDs(keys=keys, project.name=project.name, keys.na.rm=TRUE, db.channel=db.channel)
  result <- id.list[id.list != id]
  if (length(result)==0) {
    logwarn( paste("No children for ID=",
                       id,
                       " (keys=",
                       paste(keys, collapse=" - "),
                       ")"
                       )
           )
    result <- NULL
  }
  result
}

Item.GetRelativePath <- function(id, value=NULL) { 
  path <- file.path(as.integer(id / 500), id)
  if( !is.null(value))
    path <- file.path(path, value)
  path
}

Item.GetPath <- function(project.name, id, value=NULL) {
  project.path <- Project.GetPath(project.name)
  relative.path <- Item.GetRelativePath(id, value)
  paste(project.path, relative.path, sep="/")
}

Item.GetUrl <- function(project.name, id, value=NULL) {
  project.url <- Project.GetUrl(project.name)
  relative.path <- Item.GetRelativePath(id, value)
  paste(project.url, relative.path, sep="/")
}

Item.GetNewID <- function(from=strategico.config$id.dummies.from, to=strategico.config$id.dummies.to) {
  sample(from:to,1)
}

Item.AddLink <- function(project.name, value, id.list, new=TRUE) {
  target <-ifelse(new,
    ' target="_blank"',
    ' ')
  paste("<a href=", strategico.config$strategico.url, "/item.brew?project=",project.name, "&id=", id.list, "&value=",value,
    target,
        ">", id.list, "</a>", sep="")
}

Item.GetResultsWithCharts <- function(project.name, project.config=NULL, id=NULL, value="V1", db.channel, only.best=FALSE) {
  options=list(width=800, height=350)
  result <- list()

  ## id or keys must be not null

  if (is.null(id)) {
    msg <- "Cannot retrive Item data for missing ID"
    logerror( msg)
    return(result)
  }

  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name=project.name)

  if (!is.value(value, project.config=project.config)) {
    msg <- paste("Invalid value=", value, ". No data to retreive")
    logerror( msg)
    return(result)
  }

  item.models <- c()
  item.results <- data.frame()
  item.summary <- data.frame()
  item.summar.models <- data.frame()

  item.keys <- try(Item.GetKeys(project.name=project.name, id=id, db.channel=db.channel))
  ##item.keys <- ifelse(is.null(item.keys), c(), item.keys)
  item.keys.string <- paste('Key=', item.keys, collapse="<br />", sep="")

  result$keys <- item.keys
  result$keys.string <- item.keys.string

  ########################################################################################
  ##  Retriving all results
  ########################################################################################

  item.results <- Item.DB.GetNormalizedDataAndResults(project.name=project.name,
                                                      id=id, db.channel=db.channel,
                                                      value=value, only.best=only.best)

  result$item.results <- item.results

  if (is.null(item.results) || nrow(item.results) < 1) return(result)

  item.results$item_id <- NULL
  item.results.pivot <- cast(item.results, PERIOD ~ model, df=TRUE, value="V")
  item.results.pivot <- data.frame(item.results.pivot)
  rownames(item.results.pivot) <- NULL
  item.results.best <- Item.DB.GetResults(project.name=project.name, id=id, db.channel=db.channel, value=value, only.best=TRUE)
  item.results.best$item_id <- NULL

  item.residuals <- Item.DB.GetResiduals(project.name=project.name, id=id, db.channel=db.channel, value=value)
  item.summary <- Item.DB.GetSummary(project.name=project.name, id=id, db.channel=db.channel, value=value)
  item.summary.models <- Item.DB.GetSummaryModels(project.name=project.name, id=id, db.channel=db.channel, value=value)

  if (!is.null(item.summary) && nrow(item.summary) > 0) {
      item.models <- unique(item.summary.models$model)
      best.model <- as.character(item.summary[1,]$BestModel)
      suggested.model <- as.character(item.summary[1,]$SuggestedModel)
      param <- as.character(item.summary[1,]$Parameters)
  } else {
    best.model <- item.models <- suggested.model <- NULL
    param=""
  }
  if(length(param) == 0L) param=""
    
  result$param <- param
  result$best.model <- best.model
  result$models <- item.models
  result$suggested.model <- suggested.model
  result$results.pivot <- item.results.pivot
  result$residuals <- item.residuals
  result$summary <- item.summary
  result$summary.models <- item.summary.models


  ###############################################################################################################
  ## TAB Summary
  ###############################################################################################################

  if(!is.null(item.summary.models) && is.data.frame(item.summary.models) && nrow(item.summary.models) > 0) {
    i.summary.models <- item.summary.models
    i.summary.models$item_id <- NULL
    rownames(i.summary.models) <- NULL
    ## print(xtable(i.summary.models), type="html")
    ##T1 <- gvisTable(i.summary.models, options=list(width=700, height=250))
    ##b_T1 <- paste(capture.output(cat(T1$html$chart)), collapse="\n")
    b_T1 <- paste(capture.output(print(xtable(i.summary.models), type="html")), collapse="\n")
    result$summary.models.chart <- b_T1

    i.summary <- subset(item.summary, select=c(-item_id, -Parameters))
    rownames(i.summary) <- NULL
    ##summary.new <- data.frame(f=colnames(i.summary), as.data.frame(t(i.summary)))
    b_T2 <- paste(capture.output(print(xtable(t(i.summary)), type="html")), collapse="\n")
    ##T2 <- gvisTable(summary.new, options=list(width=700, height=510))
    ##b_T2 <- paste(capture.output(cat(T2$html$chart)), collapse="\n")
    result$summary.chart <- b_T2
  }

  ###############################################################################################################
  ## TAB Show best Model
  ###############################################################################################################

  best <- subset(item.results, model== best.model, select=c("PERIOD", "V"))
  BM <- gvisLineChart(data=best, options=options)
  b_BM <- paste(capture.output(cat(BM$html$chart)), collapse="\n")

  result$best.model.chart <- b_BM

  b_M2 <- ""
  df <- best
  p = sapply(as.character(df$PERIOD), function(x) y=unlist(strsplit(x, "-")))
  df$PERIOD1 <- p[1,]
  df$PERIOD2 <- p[2,]
  item.results.best.pivot <- as.data.frame(cast(df, PERIOD2 ~ PERIOD1, df=T, value="V"))
  item.results.best.pivot <- as.data.frame(cbind(month=rownames(item.results.best.pivot), item.results.best.pivot))
  BM2 <- gvisColumnChart(data=item.results.best.pivot, options=options)
  b_BM2 <- paste(capture.output(cat(BM2$html$chart)), collapse="\n")
  result$best.model.chart2 <- b_BM2

  ###############################################################################################################
  ## TAB Show All Models
  ###############################################################################################################

  AM <- gvisLineChart(data=item.results.pivot, options=options)
  b_AM <- paste(capture.output(cat(AM$html$chart)), collapse="\n")
  result$all.models.chart <- b_AM

  ###############################################################################################################
  ## TAB Results
  ###############################################################################################################

  b_TR <- paste(capture.output(print(xtable(item.results.pivot), type="html")), collapse="\n")
  ##TR <- gvisTable(item.results.pivot, options=list(width=700, height=500))
  ##b_TR <- paste(capture.output(cat(TR$html$chart)), collapse="\n")
  result$results.pivot.chart <- b_TR

  ###############################################################################################################
  ## TAB Residuals
  ###############################################################################################################

  if ((!is.null(item.residuals)) && is.data.frame(item.residuals) && nrow(item.residuals) > 0) {
    item.residuals$item_id <- NULL
    item.residuals.pivot <- cast(item.residuals, PERIOD ~ model, df=TRUE, value="V")
    item.residuals.pivot <- data.frame(item.residuals.pivot)
    ML <- gvisLineChart(data=item.residuals.pivot, options=options)
    b_ML <- paste(capture.output(cat(ML$html$chart)), collapse="\n")
    result$residuals.chart <- b_ML
  }

  result
}
