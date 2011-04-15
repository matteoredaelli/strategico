## This program is free software: you can redistribute it and/or modify
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

source("eval_ltp.R")

test.EvalItemValue <- function() {
  project.path <- "projects/sample"
  c <- GetProjectConfig("projects/sample/project.config")

  e1     <- EvalItemValue(project.path, keys=c("IT"), value="VALUE1")
  e1.bis <- EvalItemFromProjectData(project.path, keys=c("IT"), value="VALUE1")
  checkEquals(
              e1,
              e1.bis
              )
  checkEquals(
              c(5192, 1345, 5824, 1977, 6456, 2609, 7088, 3241),
              as.vector(e1[1,])
              )
  checkEquals(
              length( colnames(e1)),
              c$param$n.ahead
              )
  checkEquals(
              c("2011-1", "2011-2", "2012-1", "2012-2", "2013-1", "2013-2", "2014-1", "2014-2"),
              colnames(e1)
              )
  
  e11     <- EvalItemValue(project.path, keys=c("IT"), value="VALUE2")
  e11.bis <- EvalItemFromProjectData(project.path, keys=c("IT"), value="VALUE2")
  checkEquals(
              e11,
              e11.bis
              )
  checkEquals(
              c(13553, 12228, 16187, 20338, 20887, 20824, 25595, 28372),
              as.vector(e11[1,])
              )

  e2     <- EvalItemValue(project.path, keys=c("IT", "CAR"), value="VALUE1")
  e2.bis <- EvalItemFromProjectData(project.path, c("IT", "CAR"), value="VALUE1")
  checkEquals(
              e2,
              e2.bis
              )
  checkEquals(
              c(5278, 1378, 5996, 2096, 6714, 2814, 7432, 3532),
              as.vector(e2[1,])
              )
  checkEquals(
              c("2011-1", "2011-2", "2012-1", "2012-2", "2013-1", "2013-2", "2014-1", "2014-2"),
              colnames(e2)
              )

  e22     <- EvalItemValue(project.path, keys=c("IT", "CAR"), value="VALUE2")
  e22.bis <- EvalItemFromProjectData(project.path, c("IT", "CAR"), value="VALUE2")
  checkEquals(
              e22,
              e22.bis
              )
  checkEquals(
              c(6847, 7388, 6895, 7436, 6942, 7483, 6990, 7531),
              as.vector(e22[1,])
              )
  checkEquals(
              c("2011-1", "2011-2", "2012-1", "2012-2", "2013-1", "2013-2", "2014-1", "2014-2"),
              colnames(e22)
              )
  
  ## aggiungere qui' delle chiamate dirette alla funzione ltp()
  ## item.data = ...
  ## model = ltp(...)
  ## e fare qualche checkEquals tra i valori di model
}

test.EvalItemFromProjectData <- function() {
  project.path <- "projects/sample"
  
  e1 <- EvalItemFromProjectData(project.path, keys=c("","CAR"), value="VALUE1")
  checkEquals(
              c(4054, 1915, 3882, 2437, 3949, 2802, 4118, 3100),
              as.vector(e1[1,])
              )
}
  
