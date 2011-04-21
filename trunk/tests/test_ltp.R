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
  ## NO DATA time series
  e0     <- EvalItemValue(project.path, keys=c("ES","MOTO","DUCATI"), value="VALUE1", CONFIG=CONFIG)
  e0.bis <- EvalItemFromProjectData(project.path, keys=c("ES","MOTO","DUCATI"), value="VALUE1", CONFIG=CONFIG)
  checkEquals(
              e0,
              e0.bis
              )
  checkEquals(
              c(0, 0, 0, 0, 0, 0, 0, 0),
              as.vector(e0[1,])
              )
  checkEquals(
              length( colnames(e0)),
              CONFIG$param$n.ahead
              )
  
  checkEquals(
              c("2011-1", "2011-2", "2012-1", "2012-2", "2013-1", "2013-2", "2014-1", "2014-2"),
              colnames(e0)
              )
  
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
  ts.string="10.00, 7.60, 9.20, 8.67, 9.00, 3.60, 9.00, 5.90, 6.90, 6.50, 8.10, 9.00, 8.00, 7.00, 6.00, 7.00, 8.00, 6.00"
  #ts.string <- "10,7.6,9.2,8.67,9,3.6,9.0,5.9,6.9,6.5,8.1,9,8,7,6,7,8,6"
  e1 <- EvalTSString(project.path, keys="TEST-TS1",
                     ts.string=ts.string,
                     period.start="2001-1", period.freq=3, CONFIG=CONFIG)
  
  e1.bis <- EvalTSString(project.path, keys="TEST-TS1-BIS",
                         ts.string=ts.string,
                         ts.periods.string="2001-1,2001-2,2001-3,2002-1,2002-2,2002-3,2003-1,2003-2,2003-3,2004-1,2004-2,2004-3,2005-1,2005-2,2005-3,2006-1,2006-2,2006-3",
                         period.start="2001-1", period.freq=3, CONFIG=CONFIG)
  
  checkEquals(
              e1,
              e1.bis
              )

  checkEquals(
              c(8, 7, 6, 7, 7, 6, 7, 6),
              as.vector(e1[1,])
              )

  checkEquals(
              c("2007-1", "2007-2", "2007-3", "2008-1", "2008-2", "2008-3", "2009-1", "2009-2"),
              colnames(e1)
              )

  ## testing a TS with some missing periods
  
  e2 <- EvalTSString(project.path, keys="TEST-TS2  ",
                     ts.string=ts.string,
                     ts.periods.string="2001-1,2001-2,2002-1,2002-2,2002-3,2003-1,2003-3,2004-1,2004-2,2005-1,2005-2,2005-3,2006-1,2006-3,2007-1,2007-2,2007-3,2008-1",
                     period.start="2001-1", period.freq=3, CONFIG=CONFIG)
  checkEquals(
              c("2008-2", "2008-3", "2009-1", "2009-2", "2009-3", "2010-1", "2010-2", "2010-3"),
              colnames(e2)
              )
  checkEquals(
              c(5, 6, 7, 5, 6, 7, 5, 6),
              as.vector(e2[1,])
              )
}

