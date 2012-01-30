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

UsageAndQuit <- function(msg.err="\n") {
  self = commandArgs()[1];
  cat( paste("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++\n",
             "message: ", msg.err,
             "\n++++++++++++++++++++++++++++++++++++++++++++++++++++++\n"))
  
  q(status=1)
}

#########################################################################
## check Environment & source startegico_util.R
#########################################################################

strategico.path <-as.character(Sys.getenv("STRATEGICO_HOME"))

if (strategico.path == "")
  UsageAndQuit("Environment STRATEGICO_HOME is not set!")

source(file.path(strategico.path, "strategico_util.R"))	

project.name <- "sample"
project.config <- Project.GetConfig(project.name)
value="V1"
id=1

db.channel <- DB.Connect()

