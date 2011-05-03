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

BuildHtmlElement_input <- function(label, name, value) {
  str <- '_LABEL_ <input name="_NAME_" type="text" value="_V_" />'
  str <- gsub("_LABEL_", label, str)
  str <- gsub("_NAME_", name, str)
  str <- gsub("_V_", value, str)
  str
}

BuildHtmlElement_select <- function(label, name, list.values) {
  opt_template<- '<option value="_V_">_V_</option>'
  optlist <- ""
  for (v in list.values) {
    opt <- gsub("_V_", v, opt_template)
    optlist <- paste(optlist, opt, sep=" ")
  }
  str <- '_LABEL_ <select name="_NAME_" >_OPTIONS_</select>'
  str <- gsub("_LABEL_", label, str)
  str <- gsub("_NAME_", name, str)
  str <- gsub("_OPTIONS_", optlist, str)
  str
}

BuildHtmlKeyElement <- function(label, name, value, list.values=NULL) {
  if (is.null(list.values)) {
    str <- BuildHtmlElement_input(label, name, value)
  } else {
    str <- BuildHtmlElement_select(label, name, value, list.values)
  }
  str
}

BuildFormElement_project <- function(label="Project") {
  BuildHtmlElement_select(label, "project", GetProjectsList())
}

GetTemplatesHome <- function() {
  file.path(GetStrategicoHome(), "web_templates")
}

ShowTemplate <- function(template.name) {
  template.file <- paste(template.name, '.brew', sep='')
  brew(file.path(GetTemplatesHome(), template.file))
}
