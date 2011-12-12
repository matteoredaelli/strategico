-- phpMyAdmin SQL Dump
-- version 3.3.9
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: Sep 01, 2011 at 05:56 PM
-- Server version: 5.0.67
-- PHP Version: 5.2.14

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Database: `strategico`
--
-- --------------------------------------------------------

--
-- Table structure for table `sample_data_raw`
--


DROP TABLE IF EXISTS `sample_data_raw`;
CREATE TABLE IF NOT EXISTS `sample_data_raw` (
  `KEY1` varchar(20) NOT NULL default '',
  `KEY2` varchar(20) NOT NULL default '',
  `KEY3` varchar(20) NOT NULL default '',
  `PERIOD` varchar(20) NOT NULL default '',
  `V1` double default NULL,
  `V2` double default NULL,
  PRIMARY KEY  (`KEY1`,`KEY2`,`KEY3`,`PERIOD`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `sample_results_V1`;
CREATE TABLE IF NOT EXISTS `sample_results_V1` (
  `item_id` int(11) not NULL,
  `model` varchar(20) not NULL,
  `PERIOD` varchar(20) not NULL,
  `V` double default NULL,
  PRIMARY KEY  (`item_id`, `model`, `PERIOD`)
) ENGINE=MyIsam DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `sample_results_V2`;
CREATE TABLE IF NOT EXISTS `sample_results_V2` (
  `item_id` int(11) not NULL,
  `model` varchar(20) not NULL,
  `PERIOD` varchar(20) not NULL,
  `V` double default NULL,
  PRIMARY KEY  (`item_id`, `model`, `PERIOD`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `sample_data_norm_V1`;
CREATE TABLE IF NOT EXISTS `sample_data_norm_V1` (
  `item_id` int(11) NOT NULL,
  `PERIOD` varchar(20) NOT NULL default '',
  `V` double default NULL,
  PRIMARY KEY  (`item_id`, `PERIOD`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

DROP TABLE IF EXISTS `sample_data_norm_V2`;
CREATE TABLE IF NOT EXISTS `sample_data_norm_V2` (
  `item_id` int(11) NOT NULL,
  `PERIOD` varchar(20) NOT NULL default '',
  `V` double default NULL,
  PRIMARY KEY  (`item_id`, `PERIOD`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `sample_items`
--

DROP TABLE IF EXISTS `sample_items`;
CREATE TABLE IF NOT EXISTS `sample_items` (
  `item_id` int(11) NOT NULL,
  `KEY1` varchar(20) default NULL,
  `KEY2` varchar(20) default NULL,
  `KEY3` varchar(20) default NULL,
  PRIMARY KEY  (`item_id`),
  UNIQUE KEY `KEY1` (`KEY1`,`KEY2`,`KEY3`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `sample_summary_models_V1`
--

DROP TABLE IF EXISTS `sample_summary_models_V1`;
CREATE TABLE IF NOT EXISTS `sample_summary_models_V1` (
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
-- Table structure for table `sample_summary_models_V2`
--

DROP TABLE IF EXISTS `sample_summary_models_V2`;
CREATE TABLE IF NOT EXISTS `sample_summary_models_V2` (
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
-- Table structure for table `sample_summary_V1`
--

DROP TABLE IF EXISTS `sample_summary_V1`;
CREATE TABLE IF NOT EXISTS `sample_summary_V1` (
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

-- --------------------------------------------------------

--
-- Table structure for table `sample_summary_V2`
--

DROP TABLE IF EXISTS `sample_summary_V2`;
CREATE TABLE IF NOT EXISTS `sample_summary_V2` (
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

create or replace view v_sample_results_V1 as
select
  i.*,
  r.PERIOD,
  r.V
from
  sample_items i inner join 
  sample_summary_V1 s on (i.item_id = s.item_id) inner join 
  sample_results_V1 r on (s.item_id = r.item_id and s.BestModel = r.model)
order by
  i.item_id,
  r.PERIOD
;

create or replace view v_sample_results_V2 as
select
  i.*,
  r.PERIOD,
  r.V
from
  sample_items i inner join
  sample_summary_V2 s on (i.item_id = s.item_id) inner join
  sample_results_V2 r on (s.item_id = r.item_id and s.BestModel = r.model)
order by
  i.item_id,
  r.PERIOD
;

