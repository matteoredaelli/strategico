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

## Authors: M. Redaelli

## project name: strategico
## project website: http://code.google.com/p/strategico/
## created: 2011

library(methods)
library(getopt);

UsageAndQuit <- function(msg.err="\n") {
  self = commandArgs()[1];
  cat( paste("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++\n",
             "message: ", msg.err,
             "\n++++++++++++++++++++++++++++++++++++++++++++++++++++++\n"))
  
  options <- "
  --help | -h : show help and exit
  TODO (http://code.google.com/p/strategico/wiki/USAGE)
"
  cat(paste("\nUsage: ", self, options))
  q(status=1)
}

spec <- c(
          'cmd', 'C', 1, "character",
          'help', 'h', 0,  "logical",
          'project.name' , 'n', 1, "character",
          'id', 'i', 1, "character",
          'id.list', 'L', 1, "character",
          'id.range', 'R', 1, "character",
          'eval.param', 'p', 1, "character",
          'item.values', 'v', 1, "character",
          'file', 'F', 1, "character",
          'logging', 'l', 1, "character",
          'model', 'm', 1, "character",
          'mailto', 'M', 1, "character",
          'ahead', 'a', 1, "character",
          'runit', 'u', 0, "logical",
          'ts.freq', 'f', 1, "double",
          'ts.string', 't', 1, "character",
          'ts.periods', 'P', 1, "character",
          'ts.start', 'S', 1, "character",
          'version', 'V', 0, "logical"
          )

opt = getopt( matrix(spec,ncol=4,byrow=TRUE))

#########################################################################
## check Environment & source startegico_util.R
#########################################################################

strategico.path <-as.character(Sys.getenv("STRATEGICO_HOME"))

if (strategico.path == "") {
  strategico.path <- getwd()
  Sys.setenv(STRATEGICO_HOME=strategico.path)
  print(paste("Environment STRATEGICO_HOME is not set! Assuming", strategico.path))
}

source(file.path(strategico.path, "strategico_util.R"))

if (!is.null(opt$logging)) {
  setLevel(opt$logging)
}

#########################################################################
## version
#########################################################################

if (!is.null(opt$version)) {
  UsageAndQuit(SW.Description())
}

#########################################################################
## help
#########################################################################

if (!is.null(opt$help) ) {
  UsageAndQuit("You asked for a help...")
}

#########################################################################
## connecting to the database...
#########################################################################

db.channel <- DB.Connect()

#########################################################################
## cmd commands: runit, eval_items, eval_ts, eval_items_from_db
#########################################################################

if ( is.null(opt$cmd) ) 
  UsageAndQuit("Missing command!")

#########################################################################
## runit
#########################################################################

if (opt$cmd == "runit") {
  library('RUnit')
  
  project.name <- "sample"
  project.config <- Project.GetConfig(project.name, db.channel=db.channel)

  test.suite <- defineTestSuite("StrategicoTestSuite",
                              dirs = paste(GetStrategicoHome(), "tests", sep="/"),
                              testFileRegexp = 'test_.+\\.R$',
                              testFuncRegexp = '^test\\.+'
                              )
 
  test.result <- runTestSuite(test.suite)

  printTextProtocol(test.result)
  q(status=0);
}

#########################################################################
## Reading project.name
#########################################################################

if ( is.null(opt$project.name) )
  UsageAndQuit("Missing project name!")

#########################################################################
## create
#########################################################################

if (opt$cmd == "create") {

  Project.Create(project.name=opt$project.name, db.channel=db.channel)
  q(status=0)
}

#########################################################################
## Reading project.config
#########################################################################

if (!Project.Exists(opt$project.name))
  UsageAndQuit(sprintf("The project '%s' does nto exist! Bye", opt$project.name))

project.config <- Project.GetConfig(opt$project.name, db.channel=db.channel)

#########################################################################
## create.db
#########################################################################

if (opt$cmd == "create.db") {

  Project.CreateDB(project.name=opt$project.name, 
                         project.config=project.config, db.channel=db.channel)
  q(status=0)
}

#########################################################################
## statistics
#########################################################################

if (opt$cmd == "statistics") {
  stats <- Project.GetStatistics(project.name=opt$project.name, db.channel=db.channel)
  print(t(as.data.frame(stats)))
  q(status=0)
}

#########################################################################
## drop
#########################################################################
if (opt$cmd == "drop") {
  Project.DropDB(project.name=opt$project.name, db.channel=db.channel)
  Project.EmptyFS(project.name=opt$project.name)
  Strategico.Sendmail(to=opt$mailto, subject=opt$cmd, project.name=opt$project.name, body="Finished")
  q(status=0)
}

#########################################################################
## drop.db
#########################################################################
if (opt$cmd == "drop.db") {
  Project.DropDB(project.name=opt$project.name, db.channel=db.channel)
  q(status=0)
}

#########################################################################
## empty.db
#########################################################################
if (opt$cmd == "empty.db") {
  Project.EmptyDB(project.name=opt$project.name, db.channel=db.channel)
  q(status=0)
}

#########################################################################
## empty.fs
#########################################################################
if (opt$cmd == "empty.fs") {
  Project.EmptyFS(project.name=opt$project.name)
  q(status=0)
}

########################################################################
## export.csv
#########################################################################
if (opt$cmd == "export.csv") {
  Project.DBExportTables2Csv(project.name=opt$project.name, db.channel=db.channel)
  Project.DBExportViews2Csv(project.name=opt$project.name, db.channel=db.channel)
  Strategico.Sendmail(to=opt$mailto, subject=opt$cmd, project.name=opt$project.name, body="Finished")
  q(status=0)
}


#########################################################################
## CMD import.csv
#########################################################################

if (opt$cmd == "import.csv") {
  if(is.null(opt$file)) {
    opt$file <- Project.GetCSVFullPathFilename(opt$project.name)
    logwarn( paste("Missing file option: assuming file=", opt$file))
  }
  if(is.null(opt$ahead)) {
    opt$ahead <- 6
    logwarn( paste("Missing ahead option: assuming ahead=", opt$ahead))
  }
  if(is.null(opt$mailto)) {
    logwarn( "Missing mailto option: no mail will be sent")
  }
  Project.ImportFromCSV(project.name=opt$project.name, project.config=project.config, 
                        db.channel=db.channel, filename=opt$file,
                        mailto=opt$mailto, n.ahead=opt$ahead)
  Strategico.Sendmail(to=opt$mailto, subject=opt$cmd, project.name=opt$project.name, body="Finished")
  q(status=0)
  
}

#########################################################################
## normalizing values
#########################################################################

## item.values could be V1 or V1,V2
if (is.null(opt$item.values)) {
  opt$item.values <- GetValueNames(project.name=opt$project.name, project.config=project.config) 
  loginfo( paste("Missing item.values option: assuming item.values=", opt$item.value))
} else {
  opt$item.values <- unlist(strsplit(opt$item.values, ","))
}

#########################################################################
## CMD report.stats
#########################################################################

if (opt$cmd == "report.stats") {
  Project.BuildStatsHtmlPage(opt$project.name, db.channel=db.channel)
  q(status=0)
}

#########################################################################
## CMD report.suspicious
#########################################################################

if (opt$cmd == "report.suspicious") {
  
  if (is.null(opt$item.values))
    UsageAndQuit("Missing parameter item.values")
  
  for (v in opt$item.values) 
    Project.BuildSuspiciousItemsHtmlPage(opt$project.name, db.channel=db.channel, v)
  
  q(status=0)
}

#########################################################################
## Normalizing options
#########################################################################

logdebug( "checking parameter: eval.param")
param <- Param.EvalString(opt$eval.param)

logdebug( "checking parameters: id.list and id.range")
id.list <- c()

if (!is.null(opt$id.list))
  id.list <- unlist(strsplit(opt$id.list, ","), as.numeric)

if (!is.null(opt$id.range)) 
  id.list <- append(id.list, StrToRange(opt$id.range))


max.id <- Project.GetMaxID(opt$project.name, db.channel=db.channel)

if (is.null(id.list)) {
  loginfo( "Missing id.range and id.list: assuming all item IDs:")
  id.list <- 1:max.id
} else {
  id.list <- as.integer(id.list)
  ## removing IDs > max id   not good for ws.html
  ##wrong.id <- paste(id.list[id.list > max.id], collapse=", ")
  ##logwarn( paste("Skipping (if any) the following too high ids:", wrong.id))
  ##id.list <- id.list[id.list <= max.id]
}
#########################################################################
## eval_children
#########################################################################

if (opt$cmd == "eval.children") {

  if (is.null(opt$id.list))
    UsageAndQuit("Missing parameter id.list")

  for (id in opt$id.list)
    Item.EvalChildren(project.name=opt$project.name, id=id,
                      values=opt$item.values, param=param,
                      project.config=project.config, db.channel=db.channel)
  q(status=0)
}

#########################################################################
## eval_items
#########################################################################

if (opt$cmd == "eval.items") {

  Items.Eval(project.name=opt$project.name, 
             id.list=id.list, values=opt$item.values,
             param=param, project.config=project.config, db.channel=db.channel)
  Strategico.Sendmail(project.config=project.config, to=opt$mailto, subject=opt$cmd, project.name=opt$project.name, body="Finished")
  q(status=0)
}

#########################################################################
## eval_items_from_db
#########################################################################

if (opt$cmd == "eval.items.from.db") {

  if (is.null(opt$item.values))
    UsageAndQuit("Missing parameter item.values!")
  
  Items.DB.EvalFromSummary(project.name=opt$project.name, value=opt$item.values,
                           verbose=TRUE, project.config, db.channel=db.channel)
  Strategico.Sendmail(project.config=project.config, to=opt$mailto, subject=opt$cmd, project.name=opt$project.name, body="Finished")
  q(status=0)
}

#########################################################################
## eval_ts
#########################################################################

if (opt$cmd == "eval.ts") {

  if (is.null(opt$id))
    UsageAndQuit("Missing parameter id!")
  
  opt$id <- trunc(as.numeric(opt$id))
  if (is.na(opt$id))
    UsageAndQuit("Id parameter id is not an integer!")
  
  if (is.null(opt$ts.string))
    UsageAndQuit("Missing parameter ts.string!")
  
  if (is.null(opt$ts.start))
    UsageAndQuit("Missing parameter ts.start!")
  
  if (is.null(opt$ts.freq))
    UsageAndQuit("Missing parameter ts.freq!")

  EvalTSString(project.name=opt$project.name, id=opt$id, ts.string=opt$ts.string,
               ts.periods.string=opt$ts.periods,
               period.start.string=opt$ts.start,
               period.freq=opt$ts.freq, param=param, project.config=project.config, db.channel=db.channel
             )
  q(status=0)
}
#########################################################################
## CMD set.best.model
#########################################################################

if (opt$cmd == "set.best.model") {

  if (is.null(opt$id.list))
    UsageAndQuit("Missing parameter id.list")
  
  if (is.null(opt$item.values))
    UsageAndQuit("Missing parameter item.values")
  
  if (is.null(opt$model))
    UsageAndQuit("Missing parameter model")
  
  for (v in opt$item.values)
    Items.DB.SetBestModel(project.name=opt$project.name, id.list=id.list,
                       value=v, model=opt$model, db.channel=db.channel)
  q(status=0)
}

#########################################################################
## CMD: unknown command
#########################################################################

UsageAndQuit(paste("Unknown command", opt$cmd))



