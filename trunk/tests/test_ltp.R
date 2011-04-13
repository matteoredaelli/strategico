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
  e1 <- EvalItemValue("projects/sample", keys=c("IT"), value="VALUE1")
  
  checkEquals(
              c(5278, 1378, 5996, 2096, 6714, 2814, 7432, 3532),
              as.vector(e1[1,])
              )
}
