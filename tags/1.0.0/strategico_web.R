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

## Authors:  M. Redaelli

BuildHtmlElement_input <- function(name, value) {
    str <- '<input name="_NAME_" type="text" value="_V_" />'
    str <- gsub("_NAME_", name, str)
    str <- gsub("_V_", value, str)
    str
}

BuildHtmlElement_select <- function(name, value, list.values) {
    str <- 'select name="_NAME_" >_OPTIONS_</select>'
    str <- str.gsub("_name_", name, str)
    str <- str.gsub("_V_", value, str)
    ##TODO
}

BuildHtmlKeyElement <- function(name, value, list.values=NULL) {
  if (is.null(list.values)) {
    str <- BuildHtmlElement_inputn(name, value)
  } else {
    str <- BuildHtmlElement_select(name, value, list.values)
  }
  str
}

