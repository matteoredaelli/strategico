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
## created: 2012

Projects.GetProjects <- function(db.channel) {
  sql <- "select name from strategico_projects order by name"
  logwarn("Retreiving projects list")
  DB.RunSQLQuery(sql_statement=sql, db.channel=db.channel) 
}

Projects.GetProjectsFS <- function(projects.home = strategico.config$projects.home) {
  logdebug( paste("Projects in", projects.home, ":"))
  projects <- dir(projects.home)
  logdebug( paste(projects, collapse=", "))
  projects
}

