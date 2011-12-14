-- phpMyAdmin SQL Dump
-- version 3.3.9
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: Sep 01, 2011 at 05:56 PM
-- Server version: 5.0.67
-- PHP Version: 5.2.14

DROP TABLE IF EXISTS `projectname_results_V1`;
CREATE TABLE IF NOT EXISTS `projectname_results_V1` (
  `item_id` int(11) not NULL,
  `model` varchar(20) not NULL,
  `PERIOD` varchar(20) not NULL,
  `V` double default NULL,
  PRIMARY KEY  (`item_id`, `model`, `PERIOD`)
) ENGINE=MyIsam DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `projectname_data_norm_V1`;
CREATE TABLE IF NOT EXISTS `projectname_data_norm_V1` (
  `item_id` int(11) NOT NULL,
  `PERIOD` varchar(20) NOT NULL default '',
  `V` double default NULL,
  PRIMARY KEY  (`item_id`, `PERIOD`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

--
-- Table structure for table `projectname_summary_models_V1`
--

DROP TABLE IF EXISTS `projectname_summary_models_V1`;
CREATE TABLE IF NOT EXISTS `projectname_summary_models_V1` (
  `item_id` int(11) NOT NULL,
  `model` varchar(20) NOT NULL,
  `formula` varchar(50) default NULL,
  `R2` varchar(20) default NULL,
  `AIC` varchar(20) default NULL,
  `IC_width` varchar(20) default NULL,
  `maxJump` varchar(20) default NULL,
  `VarCoeff` varchar(20) default NULL,
  PRIMARY KEY  (`item_id`,`model`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `projectname_summary_V1`
--

DROP TABLE IF EXISTS `projectname_summary_V1`;
CREATE TABLE IF NOT EXISTS `projectname_summary_V1` (
  `item_id` int(11) NOT NULL,
  `BestModel` varchar(20) default NULL,
  `Points` int(11) default NULL,
  `NotZeroPoints` int(11) default NULL,
  `LastNotEqualValues` double default NULL,
  `MeanPredicted` double default NULL,
  `MeanValues` double default NULL,
  `MeanPredictedRatioMeanValues` double default NULL,
  `SdPredictedRatioSdValues` double default NULL,
  `BestAICNoOutRangeExclude` varchar(20) default NULL,
  `BestICNoOutRangeExclude` varchar(20) default NULL,
  `Timestamp` double default NULL,
  `SuggestedModel` varchar(20) default NULL,
  `TotModels` int(11) default NULL,
  `Parameters` varchar(1000) default NULL,
  `ReturnCode` int(11) default NULL,
  `Run` int(11) default NULL,
  PRIMARY KEY  (`item_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

create or replace view v_projectname_results_V1 as
select
  i.*,
  r.PERIOD,
  r.V
from
  projectname_items i inner join 
  projectname_summary_V1 s on (i.item_id = s.item_id) inner join 
  projectname_results_V1 r on (s.item_id = r.item_id and s.SuggestedModel = r.model)
order by
  i.item_id,
  r.PERIOD
;


create or replace view v_projectname_summary_V1 as
select
  i.KEY1,
  i.KEY2,
  i.KEY3,
  s.BestModel,
  s.Points,
  s.LastNotEqualValues,
  s.MeanPredicted,
  s.MeanValues,
  s.MeanPredictedRatioMeanValues,
  s.SdPredictedRatioSdValues,
  s.BestAICNoOutRangeExclude,
  s.BestICNoOutRangeExclude,
  s.SuggestedModel,
  sm.*
from
  projectname_items i inner join 
  projectname_summary_V1 s on (i.item_id = s.item_id) inner join 
  projectname_summary_models_V1 sm on (s.item_id = sm.item_id and s.BestModel = sm.model)
order by
  i.item_id
;

