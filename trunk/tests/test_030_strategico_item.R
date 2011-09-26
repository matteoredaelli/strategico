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

test.035.Item.GetKeys <- function() {
  project.items <- Project.GetItems(project.name)
  k1 <-     Item.GetKeys(project.name=project.name, id=5)
  k1.bis <- Item.GetKeys(project.items=project.items, id=5)
  
  checkEquals(
              k1,
              k1.bis
              )
  ## checkEquals(
  ##            c("ES", "MOTO", "DUCATI")
  ##            k1[1,]
  ##            ) 
}


test.Item.GetData <- function() {
  project.data <- Project.GetData(project.name)

  i0 <-     Item.GetData(project.name=project.name, project.data=project.data, keys=c("ES","MOTO","DUCATI"), value="V1")
  i0.bis <- Item.GetData(project.name=project.name, project.data=project.data, id=5, value="V1")

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

  keys <- c("IT","CAR","")
  i1 <- Item.GetData(project.name=project.name, project.data=project.data, keys=keys, value="V1")
  checkEquals(
              c(644.6, 646, 868, 501.2, 620, 290.3, 560, 680, 624.6, 311, 820, 250.6, 640, 440.6, 4560, 660),
              i1$V
              )
  checkEquals(
              c("2003-1","2003-2","2004-1","2004-2","2005-1","2005-2","2006-1","2006-2",
                "2007-1","2007-2","2008-1","2008-2","2009-1","2009-2","2010-1","2010-2"),
              rownames(i1)
              )
  i1.db <- Item.DB.GetData(project.name=project.name, keys=keys, value="V1", db.channel=db.channel)
  checkEquals(
              rep(TRUE, length(rownames(i1))),
              rownames(i1) == rownames(i1.db)
              )
  checkEquals(
              rep(TRUE, length(i1$V)),
              i1$V == i1.db$V
              )

  keys=c("", "MOTO","DUCATI")
  i2 <- Item.GetData(project.name=project.name, project.data=project.data, keys=keys, value="V1", period.end=c(2010,2))
  i2.db <- Item.DB.GetData(project.name=project.name, keys=keys, value="V1", db.channel=db.channel)
  
  checkEquals(
              c(33, 5, 44, 36, 80, 0, 56, 0, 80, 43, 22, 24, 53),
              i2$V
              )
  checkEquals(
              c("2001-2", "2002-2", "2003-1", "2003-2", "2004-1"),
              rownames(i2)[1:5]
              )
  checkEquals(
              rep(TRUE, length(rownames(i2))),
              rownames(i2) == rownames(i2.db)
              )
  checkEquals(
              rep(TRUE, length(i2$V)),
              i2$V == i2.db$V
              )
}

test.Item.GetRelativePath <- function() {
  checkEquals("0/1",
              Item.GetRelativePath(1)
              )
  checkEquals("0/1/V1",
              Item.GetRelativePath(1, "V1")
              )
}


test.Item.GetParent <- function() {
  checkEquals(
              10,
              Item.GetParent(id=1, project.name=project.name, db.channel=db.channel)
              )
  checkEquals(
              10,
              Item.GetParent(keys=c('IT', 'CAR', 'ALFA'), project.name=project.name, db.channel=db.channel)
              )
}

test.Item.GetChildren <- function() {
  checkEquals(
              c(3,4),
              Item.GetChildren(id=11, project.name=project.name, db.channel=db.channel)
              )
  checkEquals(
              c(),
              Item.GetChildren(id=1, project.name=project.name, db.channel=db.channel)
              )
}

test.Item.GetPath <- function() {
  checkEquals(
              paste(Project.GetPath(project.name), Item.GetRelativePath(1, "V1"), sep="/"),
              Item.GetPath(project.name, 1, "V1")
              )
  checkEquals(
              "/var/www/strategico/projects/sample/0/1/V1",
              Item.GetPath(project.name, 1, "V1")
              )
}

test.Item.GetUrl <- function() {
  checkEquals(
              paste(Project.GetUrl(project.name), Item.GetRelativePath(1, "V1"), sep="/"),
              Item.GetUrl(project.name, 1, "V1")
              )
  checkEquals(
              "http://localhost/strategico/projects/sample/0/1/V1",
              Item.GetUrl(project.name, 1, "V1")
              )
}
