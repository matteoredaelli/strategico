# Introduction #

## PROJECT ##

A project is a collection of time series with the following format

  * KEY1
  * KEY2
  * ..
  * KEYN
  * PERIOD
  * VALUE1
  * VALUE2
  * ..
  * VALUEN

## KEYS and ITEM ##

The list of values of KEY1,KEY2,...,KEYN identifies an **item** of the project.

## PERIOD ##

Period is a string with two numbers separated by "," Usually the first number is the year and second one the month or the semester or the quarter ... For instance
  * 2001,1
  * 2001,2
  * 2002,1
  * 2002,2
  * ...

## VALUES ##

For the each **item** you can have one or more time series (for instance the sales of your company and sales of competitors).