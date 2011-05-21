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

Items.Eval <- function(project.name, id.range=NULL, id.list=c(), keys=NULL, values=NULL, param=NULL,
                      project.config=NULL, project.items=NULL, project.data=NULL, db.channel) {
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name=project.name)
 
  if (is.null(project.items))
    project.items <- Project.GetItems(project.name=project.name)

  if (is.null(project.data))
    project.data <- Project.GetData(project.name=project.name)

  if (is.null(values))
    values <- GetValueNames(project.config=project.config)

  if (!is.null(id.range)) {
    list1 <- as.integer(id.range[1]):as.integer(id.range[2])
    id.list = append(list1, id.list)
  }

  for (id in id.list) {
    Item.Eval(project.name=project.name, id=id, keys=keys, values=values, param=param,
             project.config=project.config, project.items=project.items,
             project.data=project.data, db.channel=db.channel)
  }
}

Item.EmptyFS <- function(project.name, id, value=NULL, recursive = TRUE) {
  if (is.na(id) | id < 0) {
    logger(INFO, paste("Invalid ID=", id))
  } else {
    project.path <- Project.GetPath(project.name)
    relative.path <- Item.GetRelativePath(id, value)
    item.path <- paste(project.path, relative.path, sep="/")
    logger(INFO, paste("Deleting Item path:", item.path))
    unlink(item.path, recursive=recursive)
  }
}

Item.Eval <- function(project.name, id=NULL, keys=NULL, values, param=NULL,
                     project.config, project.items=NULL, project.data=NULL, db.channel) {

  if (is.null(project.data))
    project.data <- Project.GetData(project.name=project.name)

  for (i in 1:length(values)) {
    Item.EvalData(project.name=project.name, id=id, keys=keys, value=values[i], param=param,
                 project.config=project.config, project.items=project.items,
                 project.data=project.data, db.channel=db.channel)
  }
}

Item.EvalChildren <- function(project.name, id, keys=NULL, values, param=NULL,
                     project.config, project.items=NULL, project.data=NULL, db.channel) {

  if (is.null(project.items))
    project.items <- Project.GetItems(project.name=project.name)
  
  if (is.null(project.data))
    project.data <- Project.GetData(project.name=project.name)
  
  id.list <- Item.GetChildren(id=id, keys=keys, project.name=project.name, project.items=project.items)

  if (!is.null(id.list))
    Items.Eval(project.name=project.name, id.list=id.list, values=values, param=param,
              project.config=project.config, project.items=project.items, project.data=project.data, db.channel=db.channel)
}

Item.EvalData <- function(project.name, id=NULL, keys=NULL, item.data=NULL, value,
                         param=NULL, project.config, project.items=NULL, project.data=NULL, db.channel) {
  logger(INFO, "++++++++++++++++++++++++Item.EvalData ++++++++++++++++++++++++")
  logger(INFO, paste("Project=", project.name, " Loading item ID=", id,
                     " KEYS=", paste(keys,collapse=","), " ",
                     value, "=", project.config$values[value],
                     sep=""))
  
  if (is.null(item.data))
    if ("data_db" %in% project.config$save)
          item.data <- Item.DB.GetData(project.name=project.name, project.items=project.items,
                                       project.config=project.config, id=id, keys=keys,
                                       value=value, db.channel=db.channel)
    else
      item.data <- Item.GetData(project.name=project.name, project.items=project.items,
                                project.data=project.data, project.config=project.config,
                                id=id, keys=keys, value=value)
  
  logger(INFO, paste("TS length=", nrow(item.data)))
  logger(DEBUG, item.data)
  n.char <- nchar(project.config$period.freq)
  logger(INFO, paste("period.start=", Period.ToString(project.config$period.start, n.char=n.char),
                     " period.freq=", project.config$period.freq,
                     " period.end=", Period.ToString(project.config$period.end, n.char=n.char),
                     sep=""))
  
  if (is.null(id)) {
    logger(INFO, "ID is null, assigning a new value")
    id <- Item.GetNewID()
  } else {
    id <- as.integer(id)
  }
  
  ## param can be a string of parameters: in this case it must be converted to a list
  if (!is.null(param) & !is.list(param))
    param <- Param.EvalString(param)
  
  param <- Param.MergeWithDefault(project.config=project.config, param=param)

  logger(DEBUG, paste("Param= ", Param.ToString(param)))
  
  directory = Item.GetPath(project.name, id, value)
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  
  EvalFunction <- paste(project.config$eval.function,".Item.EvalDataByValue(project.name=project.name, id=id, item.data=item.data,
    value=value, output.path=directory, param=param, project.config=project.config, db.channel=db.channel)", sep="")

  prediction <- eval(parse(text=EvalFunction))
  logger(INFO, "RESULTS:")
  logger(DEBUG, rownames(prediction))
  logger(INFO, prediction)
  t(prediction)
}



Item.GetData <- function(project.name, project.data=NULL, project.config=NULL, project.items=NULL, id=NULL, keys=NULL, value="V1",
                         keys.na.rm=TRUE, period.start=NULL, period.end=NULL) {
 
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name=project.name)
  
  if (is.null(project.data))
    project.data <- Project.GetData(project.name=project.name)
  
  if (is.null(keys)) {
    if (is.null(project.items))
      project.items <- Project.GetItems(project.name=project.name)
    keys <- Item.GetKeys(id=id, project.name=project.name, project.items=project.items)
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
  n.char <- nchar(project.config$period.freq)

  if (is.null(period.start)) period.start <- project.config$period.start
  if (is.null(period.end)) period.end <- project.config$period.end
  
  string.period.start <- Period.ToString(period.start, n.char=n.char)
  string.period.end <- Period.ToString(period.end, n.char=n.char)

  subset(result, rownames(result)  >=  string.period.start & rownames(result) <= string.period.end) 
}

Item.GetIDs <- function(keys, project.name=NULL, project.items=NULL, keys.na.rm=FALSE) {
  if (is.null(project.items))
    project.items <- Project.GetItems(project.name=project.name)
  
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

Item.GetParent <- function(id, keys=NULL, project.name=NULL, project.items=NULL) {
  if (is.null(project.items))
    project.items <- Project.GetItems(project.name=project.name)
  
  if (is.null(keys))
    keys <- Item.GetKeys(id, project.name=project.name, project.items=project.items)

  parent.key <- keys
  parent.key[length(keys)]=''
  
  result <- Item.GetIDs(parent.key, project.name=project.name, project.items=project.items, keys.na.rm=FALSE)
  if (!is.na(result))
    result <- result[1]

  result
}
Item.GetChildren <- function(id, keys=NULL, project.name=NULL, project.items=NULL) {
  if (is.null(project.items))
    project.items <- Project.GetItems(project.name=project.name)
  
  if (is.null(keys))
    keys <- Item.GetKeys(id, project.name=project.name, project.items=project.items)
  
  ## TODO: now it could work only for keys with empty values at the end..."
  id.list <- Item.GetIDs(keys, project.name=project.name, project.items=project.items, keys.na.rm=TRUE)
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

Item.GetKeys <- function(id, project.name=NULL, project.items=NULL) {
  if (is.null(project.items))
    project.items <- Project.GetItems(project.name=project.name)
  
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
