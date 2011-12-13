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

library(RMySQL)

DB.Connect <- function(db.name=strategico.config$db.name
                      ,db.user=strategico.config$db.user
                      ,db.pass=strategico.config$db.pass
                      ,db.host=strategico.config$db.host
                      ) {
  db.channel <- dbConnect(MySQL(max.con = 1),
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
  sql_statement <- paste("select * from", tablename)
  records <- DB.RunSQLQuery(sql_statement=sql_statement, db.channel=db.channel)
  write.table(records, file=output.file, sep=sep, dec=dec, row.names=FALSE)
}

DB.EmptyTable <- function(tablename, db.channel) {
  sql_statement <- paste("truncate", tablename)
  try(DB.RunSQLQuery(sql_statement=sql_statement, db.channel=db.channel))
  logger(DEBUG, .Last.value) 
}

DB.DeleteAndInsertData <- function(data, tablename, id.name="item_id", id=NULL, verbose=FALSE,
                                   append=TRUE, db.channel) {
  logger(DEBUG, paste("Saving data (deleting + inserting) to table", tablename))
  delete_sql <- paste("delete from", tablename)
  
  if(!is.null(id)) 
    delete_sql<- paste(delete_sql, "where", id.name, "=", id, sep=" ")

  ##logger(DEBUG, delete_sql)
  DB.RunSQLQuery(sql_statement=delete_sql, db.channel=db.channel)

  logger(DEBUG, paste("Saving data to table", tablename)) 
 
  dbWriteTable(db.channel, value=data.frame(data), name=tablename, row.names=F,
          append=append)

}

DB.GetTableNameSummary <- function(project.name, value) {
  paste(project.name, "summary", value, sep="_")
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
  records <- DB.RunSQLQuery(sql_statement=sql_statement, db.channel=db.channel)
  ## TODO: check if the table doen't exist
  if ( is.data.frame(records) )
    result <- as.integer(records[1][1])
  else {
    logger(ERROR, paste("cannot count rows of table", tablename))
    result <- 0
  }
  result
}

DB.RunSQLQuery <- function(sql_statement, db.channel) {
  for (statement in sql_statement) {
    logger(DEBUG, paste("Running SQL:", statement))
    result <- dbGetQuery(conn=db.channel, statement)
    logger(DEBUG, paste("Done running SQL:", statement))
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

Item.DB.GetRecords <- function(project.name, key="item_id", id, tablename, db.channel) {
  filter <- paste(key, "=", id, sep="")
  sql_statement <- paste("select * from", tablename, "where", filter, sep=" ")
  DB.RunSQLQuery(sql_statement=sql_statement, db.channel=db.channel)
}

Item.Db.SaveData <- function(id, data, tablename, db.channel) {
  logger(INFO, paste("Saving data for ID=", id, "to table", tablename))
  if (nrow(data)==0) {
    logger(WARN, paste("No data to be saved to", tablename)) 
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

Item.DB.GetSummary <- function(project.name, value, id, db.channel) {
  tablename <- DB.GetTableNameSummary(project.name, value=value)
  Item.DB.GetRecords(project.name, id=id, tablename=tablename, db.channel=db.channel)
}

Item.DB.GetSummaryModels <- function(project.name, value, id, db.channel) {
  tablename <- DB.GetTableNameSummaryModels(project.name, value=value)
  Item.DB.GetRecords(project.name, id=id, tablename=tablename, db.channel=db.channel)
}

Items.DB.EvalFromSummary <- function(project.name, value, verbose=FALSE, project.config=NULL, db.channel) {
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name=project.name)

  tablename = DB.GetTableNameSummary(project.name, value)
  sql_statement <- paste("select * from ", tablename, " where Run=1", sep="")
  items <-DB.RunSQLQuery(sql_statement, db.channel=db.channel)

  tot <- nrow(items)
  if (tot == 0 )
    logger(WARN, "NO items found to be updated!")
  else {
    for( i in 1:tot) {
      item <- items[i,]
      logger(INFO, paste("Found ID=", items$item_id))
      logger(INFO, paste("Param String:", item$Parameters))
      
      param <- Param.EvalString(as.character(item$Parameters))
      Item.Eval(project.name=project.name, id=item$item_id, project.config=project.config,
               value=value, param=param, db.channel=db.channel
               )
    } #end for
  } #end if
}

Items.DB.SetBestModel <- function(project.name, value, id.list,
                                  model, db.channel) {
  str.id.list <- paste(id.list, collapse=",")
  logger(DEBUG, paste("Setting bestmodel=", model, " for id.list=", str.id.list, sep=""))
  tablename <- DB.GetTableNameSummary(project.name, value=value)
  str <- "update _TABLENAME_ set BestModel='_MODEL_' where id in (_IDLIST_)"
  str <- gsub("_TABLENAME_", tablename, str)
  str <- gsub("_MODEL_", model, str)
  str <- gsub("_IDLIST_", str.id.list, str)

  DB.RunSQLQuery(sql_statement=str, db.channel=db.channel)
}

Project.DBExportTables2Csv <- function(project.name, project.config=NULL, db.channel, sep=";", dec=",") {
  if(is.null(project.config)) {
    project.config <- Project.GetConfig(project.name)
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
    project.config <- Project.GetConfig(project.name)
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

Project.FixStructure <- function(project.name, values, db.channel) {
  ## TODO: generalize tablenames..
  ## Add unique index on keys in project_items table: needed for speed and consistency
  sql <- c("alter table sample_items MODIFY id integer",
           "alter table sample_results_V1 MODIFY item_id integer",
           "alter table sample_results_V1 MODIFY KEY1 varchar(40)",
           "alter table sample_results_V2 MODIFY item_id integer",
           "alter table sample_results_V1 MODIFY id integer",
           "alter table sample_results_V2 MODIFY id integer",
           "alter table sample_summary_V1 MODIFY id integer",
           "alter table sample_summary_V2 MODIFY id integer"
  ## ALTER TABLE europool_dev_items ADD UNIQUE ( KEY1, KEY2, KEY3, KEY4);
  ## ALTER TABLE europool_dev_results_V1 ADD INDEX ( item_id ) ;
)
  DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel)         
}
 
Project.GetStatisticsDB <- function(project.name, project.config=NULL, db.channel) {
  if(is.null(project.config)) {
    project.config <- Project.GetConfig(project.name)
  }

  tables <- Project.GetTableNames(project.name=project.name, project.config=project.config)
  rows <- unlist(lapply(tables, function(x) DB.GetTableSize(x,db.channel)))
  
  stats <- as.list(rows)
  names(stats) <- tables

  t.items <- DB.GetTableNameProjectItems(project.name)
  for (value in GetValueNames(project.config$values)) {
    ## adding % of predictions
    t.sum <- DB.GetTableNameSummary(project.name, value)
    stats[[paste("perc_predictions", value, sep= "_")]] <- 0
    try(stats[[paste("perc_predictions", value, sep= "_")]] <- stats[[t.sum]] / stats[[t.items]] * 100)
  }
  stats
}
