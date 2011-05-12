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

EvalItems <- function(project.name, id.range=NULL, id.list=c(), keys=NULL, values=NULL, param=NULL,
                      project.config=NULL, project.items=NULL, project.data=NULL, db.channel) {
  if (is.null(project.config))
    project.config <- ProjectGetConfig(project.name=project.name)
 
  if (is.null(project.items))
    project.items <- ProjectGetItems(project.name=project.name)

  if (is.null(project.data))
    project.data <- ProjectGetData(project.name=project.name)

  if (is.null(values))
    values <- GetValueNames(project.config=project.config)

  if (!is.null(id.range)) {
    list1 <- as.integer(id.range[1]):as.integer(id.range[2])
    id.list = append(list1, id.list)
  }

  for (id in id.list) {
    EvalItem(project.name=project.name, id=id, keys=keys, values=values, param=param,
             project.config=project.config, project.items=project.items,
             project.data=project.data, db.channel=db.channel)
  }
}

EvalItem <- function(project.name, id=NULL, keys=NULL, values, param=NULL,
                     project.config, project.items=NULL, project.data=NULL, db.channel) {

  if (is.null(project.data))
    project.data <- ProjectGetData(project.name=project.name)

  for (i in 1:length(values)) {
    value <- values[i]
    EvalData(project.name=project.name, id=id, keys=keys, value=value, param=param,
                 project.config=project.config, project.items=project.items,
                 project.data=project.data, db.channel=db.channel)
  }
}
EvalItemChildren <- function(project.name, id, keys=NULL, values, param=NULL,
                     project.config, project.items=NULL, project.data=NULL, db.channel) {

  if (is.null(project.items))
    project.items <- ProjectGetItems(project.name=project.name)
  
  if (is.null(project.data))
    project.data <- ProjectGetData(project.name=project.name)
  
  id.list <- ItemGetChildren(id=id, keys=keys, project.name=project.name, project.items=project.items)

  if (!is.null(id.list))
    EvalItems(project.name=project.name, id.list=id.list, values=values, param=param,
              project.config=project.config, project.items=project.items, project.data=project.data, db.channel=db.channel)
}

EvalData <- function(project.name, id=NULL, keys=NULL, item.data=NULL, value,
                         param=NULL, project.config, project.items=NULL, project.data=NULL, db.channel) {
  logger(INFO, "++++++++++++++++++++++++EvalData ++++++++++++++++++++++++")
  logger(INFO, paste("Project=", project.name, " Loading item ID=", id,
                     " KEYS=", paste(keys,collapse=","), " ",
                     value, "=", project.config$values[value],
                     sep=""))
  
  if (is.null(item.data))
    item.data <- ItemGetData(project.name=project.name, project.items=project.items, project.data=project.data, id=id, keys=keys, value=value)
  
  logger(INFO, paste("TS length=", nrow(item.data)))
  logger(DEBUG, item.data)
  
  logger(INFO, paste("period.start=", paste(project.config$period.start, collapse="-"),
                     " period.freq=", project.config$period.freq,
                     " period.end=", paste(project.config$period.end, collapse="-"),
                     sep=""))
  
  if (is.null(id)) {
    logger(INFO, "ID is null, assigning a new value")
    id <- GetNewID()
  } else {
    id <- as.integer(id)
  }
  
  ## param can be a string of parameters: in this case it must be converted to a list
  if (!is.null(param) & !is.list(param))
    param <- EvalParamString(param)
  
  param <- MergeParamWithDefault(project.config=project.config, param=param)

  logger(DEBUG, paste("Param= ", BuildParamString(param)))
  
  directory = ItemGetPath(project.name, id, value)
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  
  EvalFunction <- paste(project.config$eval.function,".EvalDataByValue(project.name=project.name, id=id, item.data=item.data,
    value=value, output.path=directory, param=param, project.config=project.config, db.channel=db.channel)", sep="")

  prediction <- eval(parse(text=EvalFunction))
  logger(INFO, "RESULTS:")
  logger(DEBUG, rownames(prediction))
  logger(INFO, prediction)
  t(prediction)
}



ItemGetData <- function(project.name, project.data=NULL, project.items=NULL, id=NULL, keys=NULL, value="V1", keys.na.rm=TRUE) {
 
  if (is.null(project.data))
    project.data <- ProjectGetData(project.name=project.name)

  if (is.null(keys)) {
    if (is.null(project.items))
      project.items <- ProjectGetItems(project.name=project.name)
    keys <- ItemGetKeys(id=id, project.name=project.name, project.items=project.items)
  }
#    filtered.data <- SubsetByID(data=project.data, id=id)
#  else
  filtered.data <- SubsetByKeys(data=project.data, keys=keys, keys.na.rm=keys.na.rm)

  if (is.null(filtered.data)) {
    logger(INFO, "filtered data is NULL")
    result = filtered.data
  } else if (nrow(filtered.data) > 0)
    result <- AggregateItemData(filtered.data, value=value)
  else {
    logger(INFO, "No rows: cannot aggregate data")
    result = filtered.data
  }
  result 
}

ItemGetIDs <- function(keys, project.name=NULL, project.items=NULL, keys.na.rm=FALSE) {
  if (is.null(project.items))
    project.items <- ProjectGetItems(project.name=project.name)
  
  records <- SubsetByKeys(data=project.items, keys=keys, keys.na.rm=keys.na.rm)
  tot <- nrow(records)
  if (tot == 0) {
    logger(WARN, paste("No id found for KEYS", keys, sep=' ', collapse=','))
    result = NA
  } else {
    result = records$id
  }
  result
}

ItemGetParent <- function(id, keys=NULL, project.name=NULL, project.items=NULL) {
  if (is.null(project.items))
    project.items <- ProjectGetItems(project.name=project.name)
  
  if (is.null(keys))
    keys <- ItemGetKeys(id, project.name=project.name, project.items=project.items)

  parent.key <- keys
  parent.key[length(keys)]=''
  
  result <- ItemGetIDs(parent.key, project.name=project.name, project.items=project.items, keys.na.rm=FALSE)
  if (!is.na(result))
    result <- result[1]

  result
}
ItemGetChildren <- function(id, keys=NULL, project.name=NULL, project.items=NULL) {
  if (is.null(project.items))
    project.items <- ProjectGetItems(project.name=project.name)
  
  if (is.null(keys))
    keys <- ItemGetKeys(id, project.name=project.name, project.items=project.items)
  
  ## TODO: now it could work only for keys with empty values at the end..."
  id.list <- ItemGetIDs(keys, project.name=project.name, project.items=project.items, keys.na.rm=TRUE)
  result <- id.list[id.list != id]
  if (length(result)==0) {
    logger(WARN, paste("No children for ID=",
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

ItemGetKeys <- function(id, project.name=NULL, project.items=NULL) {
  if (is.null(project.items))
    project.items <- ProjectGetItems(project.name=project.name)
  
  cmd <- "ds <- subset(project.items, id==__ID__, select=c(-id))"
  cmd <- gsub("__ID__", id, cmd)
  eval(parse(text = cmd))
  if (nrow(ds) >0)
    result <- as.matrix(ds)[1, ]
  else {
    logger(WARN, paste("No keys found for ID=", id))
    result <- NULL
  }
  result
}

ItemGetRelativePath <- function(id, value=NULL) { 
  path <- file.path(as.integer(id / 500), id)
  if( !is.null(value))
    path <- file.path(path, value)
  path
}

ItemGetPath <- function(project.name, id, value=NULL) {
  project.path <- ProjectGetPath(project.name)
  relative.path <- ItemGetRelativePath(id, value)
  paste(project.path, relative.path, sep="/")
}

ItemGetUrl <- function(project.name, id, value=NULL) {
  project.url <- ProjectGetUrl(project.name)
  relative.path <- ItemGetRelativePath(id, value)
  paste(project.url, relative.path, sep="/")
}

GetNewID <- function(from=strategico.config$id.dummies.from, to=strategico.config$id.dummies.to) {
  sample(from:to,1)
}
