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
-- Table structure for table `sample_data_norm_V1`
--

CREATE TABLE IF NOT EXISTS `sample_data_norm_V1` (
  `item_id` int(11) NOT NULL default '0',
  `PERIOD` varchar(20) NOT NULL default '',
  `V` double default NULL,
  PRIMARY KEY  (`item_id`,`PERIOD`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `sample_data_norm_V2`
--

CREATE TABLE IF NOT EXISTS `sample_data_norm_V2` (
  `item_id` int(11) NOT NULL default '0',
  `PERIOD` varchar(20) NOT NULL default '',
  `V` double default NULL,
  PRIMARY KEY  (`item_id`,`PERIOD`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `sample_data_raw`
--

CREATE TABLE IF NOT EXISTS `sample_data_raw` (
  `KEY1` varchar(20) NOT NULL default '',
  `KEY2` varchar(20) NOT NULL default '',
  `KEY3` varchar(20) NOT NULL default '',
  `PERIOD` varchar(20) NOT NULL default '',
  `V1` int(11) default NULL,
  `V2` int(11) default NULL,
  PRIMARY KEY  (`KEY1`,`KEY2`,`KEY3`,`PERIOD`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `sample_items`
--

CREATE TABLE IF NOT EXISTS `sample_items` (
  `id` varchar(20) NOT NULL,
  `KEY1` varchar(20) default NULL,
  `KEY2` varchar(20) default NULL,
  `KEY3` varchar(20) default NULL,
  PRIMARY KEY  (`id`),
  UNIQUE KEY `KEY1` (`KEY1`,`KEY2`,`KEY3`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `sample_results_V1`
--

CREATE TABLE IF NOT EXISTS `sample_results_V1` (
  `item_id` varchar(20) NOT NULL default '',
  `model` varchar(20) NOT NULL default '',
  `PERIOD` varchar(20) NOT NULL default '',
  `V` varchar(20) default NULL,
  PRIMARY KEY  (`item_id`,`model`,`PERIOD`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `sample_results_V2`
--

CREATE TABLE IF NOT EXISTS `sample_results_V2` (
  `item_id` varchar(20) NOT NULL default '',
  `model` varchar(20) NOT NULL default '',
  `PERIOD` varchar(20) NOT NULL default '',
  `V` varchar(20) default NULL,
  PRIMARY KEY  (`item_id`,`model`,`PERIOD`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `sample_summary_models_V1`
--

CREATE TABLE IF NOT EXISTS `sample_summary_models_V1` (
  `item_id` int(11) NOT NULL default '0',
  `model` varchar(20) NOT NULL default '',
  `formula` varchar(20) default NULL,
  `R2` varchar(20) default NULL,
  `AIC` varchar(20) default NULL,
  `ICwidth` varchar(20) default NULL,
  `maxJump` varchar(20) default NULL,
  `VarCoeff` varchar(20) default NULL,
  PRIMARY KEY  (`item_id`,`model`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `sample_summary_models_V2`
--

CREATE TABLE IF NOT EXISTS `sample_summary_models_V2` (
  `item_id` int(11) NOT NULL default '0',
  `model` varchar(20) NOT NULL default '',
  `formula` varchar(20) default NULL,
  `R2` varchar(20) default NULL,
  `AIC` varchar(20) default NULL,
  `ICwidth` varchar(20) default NULL,
  `maxJump` varchar(20) default NULL,
  `VarCoeff` varchar(20) default NULL,
  PRIMARY KEY  (`item_id`,`model`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `sample_summary_V1`
--

CREATE TABLE IF NOT EXISTS `sample_summary_V1` (
  `id` varchar(20) NOT NULL,
  `BestModel` varchar(20) default NULL,
  `ICwidth` double default NULL,
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
  `ReturnCode` double default NULL,
  `Run` double default NULL,
  PRIMARY KEY  (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `sample_summary_V2`
--

CREATE TABLE IF NOT EXISTS `sample_summary_V2` (
  `id` varchar(20) NOT NULL,
  `BestModel` varchar(20) default NULL,
  `ICwidth` double default NULL,
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
  `ReturnCode` double default NULL,
  `Run` double default NULL,
  PRIMARY KEY  (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
