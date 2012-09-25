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
    project.config <- Project.GetConfig(project.name, db.channel=db.channel)

  if (is.null(values))
    values <- GetValueNames(project.config=project.config)

  for (id in id.list) {
    try(Item.Eval(project.name=project.name, id=id, keys=keys, values=values, param=param,
             period.start=period.start, period.end=period.end,
             project.config=project.config, db.channel=db.channel))
  }
}

Items.DB.EvalFromSummary <- function(project.name, value, verbose=FALSE, project.config=NULL, db.channel) {
  if (is.null(project.config))
    project.config <- Project.GetConfig(project.name, db.channel=db.channel)

  tablename = DB.GetTableNameSummary(project.name, value)
  sql_statement <- paste("select * from ", tablename, " where Run=1", sep="")
  items <-DB.RunSQLQuery(sql_statement, db.channel=db.channel)

  tot <- nrow(items)
  if (tot == 0 )
    logwarn( "NO items found to be updated!")
  else {
    for( i in 1:tot) {
      item <- items[i,]
      loginfo( paste("Found ID=", items$item_id))
      loginfo( paste("Param String:", item$Parameters))
      
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
  logdebug( paste("Setting bestmodel=", model, " for id.list=", str.id.list, sep=""))
  tablename <- DB.GetTableNameSummary(project.name, value=value)
  str <- "update _TABLENAME_ set BestModel='_MODEL_' where item_id in (_IDLIST_)"
  str <- gsub("_TABLENAME_", tablename, str)
  str <- gsub("_MODEL_", model, str)
  str <- gsub("_IDLIST_", str.id.list, str)

  DB.RunSQLQuery(sql_statement=str, db.channel=db.channel)
}

