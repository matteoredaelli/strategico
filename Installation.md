# Requirements #

Strategico requires the library AST that at the moment works only in Linux/Unix


You need to install
  * the [R](http://www.r-project.org) (release >= 2.11) statistical software

and the following R libraries (see [CRAN](http://cran.r-project.org/web/packages/) for a full list of R libraries):
  * [getopt](http://cran.r-project.org/web/packages/getopt)
  * [logging](http://cran.r-project.org/web/packages/logging)
  * [reshape](http://cran.r-project.org/web/packages/reshape/)
  * [hwriter](http://cran.r-project.org/web/packages/hwriter/index.html)
  * [RMySQL library](http://cran.r-project.org/web/packages/RMySQL/index.html)
  * [Forecast library](http://cran.r-project.org/web/packages/forecast/index.html) that requires fracdiff and tseries
  * [R2HTML library](http://cran.r-project.org/web/packages/R2HTML/index.html)
  * [runit](http://cran.r-project.org/web/packages/RUnit/index.html)
  * [xtable library](http://cran.r-project.org/web/packages/xtable/index.html)
  * [brew](http://cran.r-project.org/web/packages/brew/index.html)
  * [sendmailR](http://cran.r-project.org/web/packages/sendmailR/index.html)

You can install them with teh following R command
```
  install.packages(c("getopt", "logging", "hwriter", "reshape", "tseries", "forecast", "RUnit", "xtable", "R2HTML","fracdiff", "brew", "sendmailR", "googleVis", "RMySQL"), dependencies = TRUE)
```

For Debian and Ubuntu some of the libraries are available via apt-get command:
```
  apt-get install r-cran-xtable r-cran-runit r-cran-getopt r-cran-xtable r-cran-mysql r-cran-tseries
```


# Installation #

Download strategico.zip from [Download tab](http://code.google.com/p/strategico/downloads/list) and unzip it into a folder like /apps/strategico (Linux) or C:\strategico


# Configuration #

## Environment ##

Set a variable STRATEGICO\_HOME in your environment

Under Linux you could add

```
 export STRATEGICO_HOME=/opt/strategico
```

in your /etc/profile or $HOME/.bashrc

## Database ##

Enter in mysql console with

> mysql -u root

```
 CREATE DATABASE strategico CHARACTER SET utf8 COLLATE utf8_general_ci;

 grant all on strategico.* to r@localhost identified by 'r';
 grant all on strategico.* to r@'%' identified by 'r';
 flush privileges;
```
