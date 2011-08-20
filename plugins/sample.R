# This program is fre esoftware: you can redistribute it and/or modify
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

## project name: strategico
## project website: http://code.google.com/p/strategico/
## authors: M. redaelli
## created: 2011

sample.importItemsData <- function(project.name) {
  fullname <- paste("tests", "external_data.csv", sep="/")
  Project.ImportDataFromCSV(project.name, filename=fullname,
                           KEY=c("KEY1","KEY2","KEY3"),
                           timesKeys=c("PERIOD"),
                           V=c("V1","V2"))
}
