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

library(methods)
library('getopt');

UsageAndQuit <- function(msg.err="\n") {
  self = commandArgs()[1];
  cat(msg.err)
  
  options <- "
  --help | -h : show help and exit
  TODO
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

if (strategico.path == "")
  UsageAndQuit("Environment STRATEGICO_HOME is nor set!")

source(file.path(strategico.path, "strategico_util.R"))	

#########################################################################
## version
#########################################################################

if (!is.null(opt$version)) {
  UsageAndQuit(Version())
}

#########################################################################
## help
#########################################################################

if (!is.null(opt$help) ) {
  UsageAndQuit("You asked for a help...")
}

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
## Reading project.config
#########################################################################

if ( is.null(opt$project.name) )
  UsageAndQuit("Missing project name!")

if ( !is.project(opt$project.name) )
  UsageAndQuit( paste("Unknown project name '", opt$project.name, "'", sep=""))

project.config <- GetProjectConfig(opt$project.name)

#########################################################################
## Opening DB connection
#########################################################################

# TODO: check if connection fails
db.channel <- DBConnect()

#########################################################################
## check missing options
#########################################################################

param <- EvalParamString(opt$eval.param)


#########################################################################
## eval_items
#########################################################################

if (opt$cmd == "eval_items") {

  if (is.null(opt$id.list) & is.null(opt$id.range))
    UsageAndQuit("Missing parameter id.list or id.range")

  if (!is.null(opt$id.list))
    opt$id.list <- unlist(strsplit(opt$id.list, ","), as.numeric)
  if (!is.null(opt$id.range))
    opt$id.range <- unlist(strsplit(opt$id.range, ":"), as.numeric)
      
##  if (is.null(opt$item.values))
##    UsageAndQuit("Missing parameter item.values!")

  ## item.values could be could be V1 or V1,V2
  if (!is.null(opt$item.values))
    opt$item.values <- unlist(strsplit(opt$item.values, ","))

  EvalItems(project.name=opt$project.name, 
            id.range=opt$id.range, id.list=opt$id.list,
            values=opt$values, param=param, project.config=project.config, db.channel=db.channel)
  q(status=0)
}

#########################################################################
## eval_items_from_db
#########################################################################

if (opt$cmd == "eval_items_from_db") {

  if (is.null(opt$item.values))
    UsageAndQuit("Missing parameter item.values!")
  
  EvalItemsFromDB(project.name=opt$project.name, value=opt$item.value,
                  verbose=TRUE, project.config, db.channel=db.channel)
  
  q(status=0)
}

#########################################################################
## eval_ts
#########################################################################

if (opt$cmd == "eval_ts") {

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
## statistics
#########################################################################

if (opt$cmd == "statistics") {
  stats <- GetProjectStatistics(project.name=opt$project.name, db.channel=db.channel)
  print(stats)
  q(status=0)
}

#########################################################################
## empty.db
#########################################################################
if (opt$cmd == "empty.db") {
  EmptyProjectTablesDB(project.name=opt$project.name, db.channel=db.channel)
  q(status=0)
}
########################################################################
## export.db.csv
#########################################################################
if (opt$cmd == "export.db.csv") {
  ExportProjectTables2Csv(project.name=opt$project.name, db.channel=db.channel)
  q(status=0)
}
#########################################################################
## import
#########################################################################

if (opt$cmd == "import") {
  ImportProjectData(project.name=opt$project.name, db.channel=db.channel)
  q(status=0)
  
} else {
  UsageAndQuit(paste("Unknown command", opt$cmd))
}



