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
  paste("<a href=item.brew?project=",project.name, "&id=", id.list, "&value=",value,
    target,
        ">", id.list, "</a>", sep="")
}
