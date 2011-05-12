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

test.035.ItemGetKeys <- function() {
  project.items <- ProjectGetItems(project.name)
  k1 <-     ItemGetKeys(project.name=project.name, id=5)
  k1.bis <- ItemGetKeys(project.items=project.items, id=5)
  
  checkEquals(
              k1,
              k1.bis
              )
  ## checkEquals(
  ##            c("ES", "MOTO", "DUCATI")
  ##            k1[1,]
  ##            ) 
}

test.035.ItemGetIDs <- function() {
  project.items <- ProjectGetItems(project.name)
  
  checkEquals(
              c(2),
              ItemGetIDs(keys=c("IT","CAR","ALFA"), project.items=project.items, keys.na.rm=FALSE)
              )
  checkEquals(
              c(10),
              ItemGetIDs(keys=c("IT","CAR",""), project.items=project.items, keys.na.rm=FALSE)
              )
  checkEquals(
              c(16),
              ItemGetIDs(keys=c("IT","",""), project.items=project.items, keys.na.rm=FALSE)
              )
  checkEquals(
              c(1, 2, 8, 10, 14, 16),
              ItemGetIDs(keys=c("IT","",""), project.items=project.items, keys.na.rm=TRUE)
              )
}

test.ItemGetData <- function() {
  project.data <- ProjectGetData(project.name)

  i0 <-     ItemGetData(project.name=project.name, project.data=project.data, keys=c("ES","MOTO","DUCATI"), value="V1")
  i0.bis <- ItemGetData(project.name=project.name, project.data=project.data, id=5, value="V1")

  checkEquals(
              i0,
              i0.bis
              )
  checkEquals(
              "2010-2",
              rownames(i0)
              )
  checkEquals(
              c(0),
              i0[1,]
              )
  
  i1 <- ItemGetData(project.name=project.name, project.data=project.data, keys=c("IT","CAR",""), value="V1")
  checkEquals(
              c(644.6, 646, 868, 501.2, 620, 290.3, 560, 680, 624.6, 311, 820, 250.6, 640, 440.6, 4560, 660),
              i1$V1
              )
  checkEquals(
              c("2003-1","2003-2","2004-1","2004-2","2005-1","2005-2","2006-1","2006-2","2007-1","2007-2","2008-1","2008-2","2009-1","2009-2","2010-1","2010-2"),
              rownames(i1)
              )

  i2 <- ItemGetData(project.name=project.name, project.data=project.data, keys=c("", "MOTO","DUCATI"), value="V1")
  checkEquals(
              c(33, 5, 44, 36, 80, 0, 56, 0, 80, 43, 22, 24, 53, 44),
              i2$V1
              )
  checkEquals(
              c("2001-2", "2002-2", "2003-1", "2003-2", "2004-1"),
              rownames(i2)[1:5]
              )
}

test.ItemGetRelativePath <- function() {
  checkEquals("0/1",
              ItemGetRelativePath(1)
              )
  checkEquals("0/1/V1",
              ItemGetRelativePath(1, "V1")
              )
}


test.ItemGetParent <- function() {
  checkEquals(
              10,
              ItemGetParent(id=1, project.name=project.name)
              )
  checkEquals(
              10,
              ItemGetParent(keys=c('IT', 'CAR', 'ALFA'), project.name=project.name)
              )
}

test.ItemGetChildren <- function() {
  checkEquals(
              c(3,4),
              ItemGetChildren(id=11, project.name=project.name)
              )
  checkEquals(
              c(),
              ItemGetChildren(id=1, project.name=project.name)
              )
}

test.ItemGetPath <- function() {
  checkEquals(
              paste(ProjectGetPath(project.name), ItemGetRelativePath(1, "V1"), sep="/"),
              ItemGetPath(project.name, 1, "V1")
              )
  checkEquals(
              "/var/www/strategico/projects/sample/0/1/V1",
              ItemGetPath(project.name, 1, "V1")
              )
}

test.ItemGetUrl <- function() {
  checkEquals(
              paste(ProjectGetUrl(project.name), ItemGetRelativePath(1, "V1"), sep="/"),
              ItemGetUrl(project.name, 1, "V1")
              )
  checkEquals(
              "http://localhost/strategico/projects/sample/0/1/V1",
              ItemGetUrl(project.name, 1, "V1")
              )
}
