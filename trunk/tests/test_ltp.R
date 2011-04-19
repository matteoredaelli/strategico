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

project.path <- "projects/sample"
CONFIG <- GetProjectConfig(paste(project.path, "project.config", sep="/"))

test.EvalItemValue <- function() {

  e1     <- EvalItemValue(project.path, keys=c("IT"), value="VALUE1", CONFIG=CONFIG)
  e1.bis <- EvalItemFromProjectData(project.path, keys=c("IT"), value="VALUE1", CONFIG=CONFIG)
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
              CONFIG$param$n.ahead
              )
  checkEquals(
              c("2011-1", "2011-2", "2012-1", "2012-2", "2013-1", "2013-2", "2014-1", "2014-2"),
              colnames(e1)
              )
  
  e11     <- EvalItemValue(project.path, keys=c("IT"), value="VALUE2", CONFIG=CONFIG)
  e11.bis <- EvalItemFromProjectData(project.path, keys=c("IT"), value="VALUE2", CONFIG=CONFIG)
  checkEquals(
              e11,
              e11.bis
              )
  checkEquals(
              c(13553, 12228, 16187, 20338, 20887, 20824, 25595, 28372),
              as.vector(e11[1,])
              )

  e2     <- EvalItemValue(project.path, keys=c("IT", "CAR"), value="VALUE1", CONFIG=CONFIG)
  e2.bis <- EvalItemFromProjectData(project.path, c("IT", "CAR"), value="VALUE1", CONFIG=CONFIG)
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

  e22     <- EvalItemValue(project.path, keys=c("IT", "CAR"), value="VALUE2", CONFIG=CONFIG)
  e22.bis <- EvalItemFromProjectData(project.path, c("IT", "CAR"), value="VALUE2", CONFIG=CONFIG)
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
}

test.EvalItemValue.param <- function() {

  param.string <- "n.ahead=3"
  param <- EvalParamString(param.string)
  
  ## check passing parameters
  
  e3     <- EvalItemValue(project.path, keys=c("DE", "CAR", "ALFA"),
                          value="VALUE2", CONFIG=CONFIG, param=param)
  checkEquals(
              c(3998, 3766, 4094),
              as.vector(e3[1,])
              )

  param.string <- "n.ahead=3,try.models=c('mean','trend','lm')"
  param <- EvalParamString(param.string)
  e4     <- EvalItemValue(project.path, keys=c("DE", "CAR", "BMW"),
                          value="VALUE2", CONFIG=CONFIG, param=param)
  checkEquals(
              c(2026, 9098, 2328),
              as.vector(e4[1,])
              )
  ## aggiungere qui' delle chiamate dirette alla funzione ltp()
  ## item.data = ...
  ## model = ltp(...)
  ## e fare qualche checkEquals tra i valori di model
}

test.EvalItemFromProjectData <- function() {

  e1 <- EvalItemFromProjectData(project.path, keys=c("","CAR"), value="VALUE1", CONFIG=CONFIG)
  checkEquals(
              c(4054, 1915, 3882, 2437, 3949, 2802, 4118, 3100),
              as.vector(e1[1,])
              )
}
  
test.EvalTSString <- function() {

  e1 <- EvalTSString(project.path, keys="TEST-1",
                     ts.string="10,7.6,9.2,8.67,9,3.6,9.0,5.9,6.9,6.5,8.1,9,8,7,6,7,8,6",
                     period.start="2001-1", period.freq=3, CONFIG=CONFIG)
  checkEquals(
              c(8, 7, 6, 7, 7, 6, 7, 6),
              as.vector(e1[1,])
              )

  checkEquals(
              c("2007-1", "2007-2", "2007-3", "2008-1", "2008-2", "2008-3", "2009-1", "2009-2"),
              colnames(e1)
              )
}
