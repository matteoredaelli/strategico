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

DB.Connect <- function(db.name=strategico.config$db.name
                      ,db.user=strategico.config$db.user
                      ,db.pass=strategico.config$db.pass
                      ,db.host=strategico.config$db.host
                      ) {
  db.channel <- dbConnect(MySQL(max.con = 50),
                          user=db.user,
                          password=db.pass,
                          dbname=db.name,
                          host=db.host)

  ## TODO how to check a connection with RMySQL?
  ##if( db.channel == -1)
  ##  Quit("Cannot connect to DB")

  db.channel
}

DB.Close <- function(db.channel) {
  dbDisconnect(db.channel)
}

DB.ExportTable2Csv <- function(tablename, db.channel, output.file, sep=";", dec=",") {
  if(dbExistsTable(db.channel, tablename)) {
    sql_statement <- paste("select * from", tablename)
    records <- DB.RunSQLQuery(sql_statement=sql_statement, db.channel=db.channel)
    write.table(records, file=output.file, sep=sep, dec=dec, row.names=FALSE)
  } else {
    loginfo( paste("Table", tablename, "does not exist and cannot be exported to a csv file"))
  }
}

DB.EmptyTable <- function(tablename, db.channel) {
  sql_statement <- paste("truncate", tablename)
  try(DB.RunSQLQuery(sql_statement=sql_statement, db.channel=db.channel))
  logdebug( .Last.value) 
}

DB.DropTable <- function(tablename, db.channel) {
  DB.Drop(tablename, db.channel, object="table")
}

DB.DropView <- function(tablename, db.channel) {
  DB.Drop(tablename, db.channel, object="view")
}

DB.Drop <- function(tablename, db.channel, object="table") {
  logdebug( paste("Dropping", object, tablename, "...")) 
  sql_statement <- paste("drop", object, tablename)
  try(DB.RunSQLQuery(sql_statement=sql_statement, db.channel=db.channel))
  logdebug( .Last.value) 
}


DB.DeleteAndInsertData <- function(data, tablename, id.name="item_id", id=NULL, verbose=FALSE,
                                   append=TRUE, db.channel) {
  logdebug( paste("Saving data (deleting + inserting) to table", tablename))
  delete_sql <- paste("delete from", tablename)
  
  if(!is.null(id)) 
    delete_sql<- paste(delete_sql, "where", id.name, "=", id, sep=" ")

  ##logdebug( delete_sql)
  DB.RunSQLQuery(sql_statement=delete_sql, db.channel=db.channel)

  logdebug( paste("Saving data to table", tablename)) 
 
  dbWriteTable(db.channel, value=data.frame(data), name=tablename, row.names=F,
          append=append)
  logdebug( paste("Done saving data (deleting + inserting) to table", tablename))

}

DB.GetTableNameSummary <- function(project.name, value) {
  paste(project.name, "summary", value, sep="_")
}

DB.GetTableNameResiduals <- function(project.name, value) {
  paste(project.name, "residuals", value, sep="_")
}

DB.GetTableNameResults <- function(project.name, value) {
  paste(project.name, "results", value, sep="_")
}

DB.GetTableNameSummaryModels <- function(project.name, value) {
  paste(project.name, "summary_models", value, sep="_")
}

DB.GetTableNameProjectData <- function(project.name) {
  paste(project.name, "data_raw", sep="_")
}

DB.GetTableNameNormalizedData <- function(project.name, value) {
  paste(project.name, "data_norm", value, sep="_")
}

DB.GetTableNameProjectItems <- function(project.name) {
  paste(project.name, "items", sep="_")
}

DB.GetTableSize <- function(tablename, db.channel) {
  sql_statement <- paste("select count(*) from", tablename)
  records <- try(DB.RunSQLQuery(sql_statement=sql_statement, db.channel=db.channel))
  ## TODO: check if the table doen't exist
  if ( is.data.frame(records) )
    result <- as.integer(records[1][1])
  else {
    logerror( paste("cannot count rows of table", tablename))
    result <- 0
  }
  result
}

DB.RunSQLQuery <- function(sql_statement, db.channel) {
  for (statement in sql_statement) {
    logdebug( paste("Running SQL:", statement))
    result <- try(dbGetQuery(conn=db.channel, statement))
    logdebug( paste("Done running SQL:", statement))
  }
  result
}

Item.DB.GetNormalizedDataAndResults <- function(project.name, value, id, db.channel, only.best=TRUE) {
  i.results <-        Item.DB.GetResults(project.name=project.name, id=id, db.channel=db.channel, value=value, only.best=only.best)
  i.hist <-           Item.DB.GetNormalizedData(project.name=project.name, id=id, db.channel=db.channel, value=value)
  i.hist$item_id <- NULL
  i.results$item_id <- NULL
  
  model.names <- unique(i.results$model)
  nrow <- nrow(i.hist)
  repeted.models <- unlist(lapply(model.names, function(x) rep(x,nrow)))

  i.hist <- data.frame(model=repeted.models, i.hist)
  rbind(i.hist, i.results)
}

Item.DB.GetRecords <- function(project.name, key="item_id", id.list, tablename, db.channel) {
  id.string <- paste(as.vector(id.list), collapse="','", sep="")
  id.string <- paste("('",id.string, "')", sep="")
  filter <- paste(key, " in ", id.string, sep="")
  sql_statement <- paste("select * from", tablename, "where", filter, sep=" ")
  DB.RunSQLQuery(sql_statement=sql_statement, db.channel=db.channel)
}

Item.Db.SaveData <- function(id, data, tablename, db.channel) {
  loginfo( paste("Saving data for ID=", id, "to table", tablename))
  if (nrow(data)==0) {
    logwarn( paste("No data to be saved to", tablename)) 
  } else {
    data = cbind(item_id=id, data)
    data$PERIOD = rownames(data)
 
    DB.DeleteAndInsertData(data, tablename=tablename, id=id,
                           id.name="item_id", append=TRUE, db.channel=db.channel)
  }
}

Item.DB.GetNormalizedData <- function(project.name, value, id, db.channel) {
  tablename <- DB.GetTableNameNormalizedData(project.name, value=value)
  Item.DB.GetRecords(project.name, id=id, tablename=tablename, db.channel=db.channel)
}

Item.DB.GetResults <- function(project.name, value, id, db.channel, only.best=TRUE) {
  tablename <- DB.GetTableNameResults(project.name=project.name, value=value)
  if (only.best)
    tablename <- paste("v", tablename, sep="_")
  Item.DB.GetRecords(project.name, id=id, tablename=tablename, db.channel=db.channel)
}

Item.DB.GetResiduals <- function(project.name, value, id, db.channel) {
  tablename <- DB.GetTableNameResiduals(project.name, value=value)
  Item.DB.GetRecords(project.name, id=id, tablename=tablename, db.channel=db.channel)
}

Item.DB.GetSummary <- function(project.name, value, id, db.channel) {
  tablename <- DB.GetTableNameSummary(project.name, value=value)
  Item.DB.GetRecords(project.name, id=id, tablename=tablename, db.channel=db.channel)
}

Item.DB.GetVSummary <- function(project.name, value, id, db.channel) {
  tablename <- DB.GetTableNameSummary(project.name, value=value)
  tablename <- paste("v_", tablename, sep="")
  Item.DB.GetRecords(project.name, id=id, tablename=tablename, db.channel=db.channel)
}

Item.DB.GetSummaryModels <- function(project.name, value, id, db.channel) {
  tablename <- DB.GetTableNameSummaryModels(project.name, value=value)
  Item.DB.GetRecords(project.name, id=id, tablename=tablename, db.channel=db.channel)
}

Project.DBExportTables2Csv <- function(project.name, project.config=NULL, db.channel, sep=";", dec=",") {
  if(is.null(project.config)) {
    project.config <- Project.GetConfig(project.name, db.channel=db.channel)
  }
  project.path <- Project.GetPath(project.name)
  tables <- Project.GetTableNames(project.name=project.name, project.config=project.config)
  lapply(tables, function(x) DB.ExportTable2Csv(tablename=x,
                                             db.channel=db.channel,
                                             output.file=file.path(project.path, paste(x, ".csv", sep="")),
                                             sep=sep,
                                             dec=dec
                                             )
         )
}

Project.DBExportViews2Csv <- function(project.name, project.config=NULL, db.channel, sep=";", dec=",") {
  if(is.null(project.config)) {
    project.config <- Project.GetConfig(project.name, db.channel=db.channel)
  }
  project.path <- Project.GetPath(project.name)
  tables <- Project.GetViewNames(project.name=project.name, project.config=project.config)
  lapply(tables, function(x) DB.ExportTable2Csv(tablename=x,
                                             db.channel=db.channel,
                                             output.file=file.path(project.path, paste(x, ".csv", sep="")),
                                             sep=sep,
                                             dec=dec
                                             )
         )
}

Project.DB.AddOrUpdate.ununsed <- function(project.name, db.channel, config="", params="") {
  logdebug(paste("deleting record name=", project.name, "from strategico_projects table"))
  sql <- "delete from strategico_projects where name='__PROJECT_NAME__'"
  sql <- gsub("__PROJECT_NAME__", project.name, sql)
  DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel)

  logdebug(paste("adding record name=", project.name, "from strategico_projects table"))
  sql <- 'insert into strategico_projects(name, config, params) values("__PROJECT_NAME__", "__CONFIG__", "__PARAMS__")'
  logwarn(paste("config=", config))
  sql <- gsub("__PROJECT_NAME__", project.name, sql)
  sql <- gsub("__PARAMS__", params, sql)
  sql <- gsub("__CONFIG__", config, sql)
  DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel)
}
