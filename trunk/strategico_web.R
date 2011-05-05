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

BuildHtmlElement_input <- function(label="", name, default, type="text") {
  str <- '_LABEL_ <input name="_NAME_" type="_TYPE_" value="_V_" />'
  str <- gsub("_LABEL_", label, str)
  str <- gsub("_NAME_", name, str)
  str <- gsub("_V_", default, str)
  str <- gsub("_TYPE_", type, str)
  str
}

BuildHtmlElement_select <- function(label, name, list.values, default=NULL) {
  opt_template<- '<option value="_V_">_V_</option>'
  opt_selected_template<- '<option value="_V_" SELECTED>_V_</option>'
  optlist <- ""

  if (is.null(default))
    default <- list.values[1]
  
  for (v in list.values) {
    if (v==default)
      opt <- gsub("_V_", v, opt_selected_template)
    else
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

BuildFormElement_project <- function(label="Project", default=NULL) {
  BuildHtmlElement_select(label=label, name="project",list.values= GetProjectsList(), default=default)
}

BuildFormElement_keys <- function(project.name=NULL, project.config=NULL, project.items=NULL, defaults, sep=" ") {
  if (is.null(project.config))
    project.config <- GetProjectConfig(project.name=project.name)
  if (is.null(project.items))
    project.items <- GetProjectItems(project.name=project.name)
  
  list.values <- GetUniqueKeyValues(project.name=project.name, project.items=project.items, project.config=project.config)
  keys <- GetKeyNames(keys=NULL, project.name=project.name, project.config=project.config)
  result <- ""
  for (k in 1:length(keys)) 
    result <- paste(result,
                    BuildHtmlElement_select(project.config$keys[k], keys[k], list.values[[k]], defaults[k]),
                    sep=sep)
  result
}

BuildFormElement_value <- function(project.name=NULL, project.config=NULL, project.items=NULL, default=NULL) {
  if (is.null(project.config))
    project.config <- GetProjectConfig(project.name=project.name)
  if (is.null(project.items))
    project.items <- GetProjectItems(project.name=project.name)
  
  list.values <- GetValueNames(values=project.config$values, project.name=project.name, project.config=project.config)
  BuildHtmlElement_select("Value", "value", list.values, default=default)
}

GetTemplatesHome <- function() {
  file.path(GetStrategicoHome(), "web_templates")
}

ShowTemplate <- function(template.name) {
  template.file <- paste(template.name, '.brew', sep='')
  brew(file.path(GetTemplatesHome(), template.file))
}
