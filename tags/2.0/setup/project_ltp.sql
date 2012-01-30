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
-- Table structure for table `projectname_data_raw`
--


DROP TABLE IF EXISTS `projectname_data_raw`;
CREATE TABLE IF NOT EXISTS `projectname_data_raw` (
  `KEY1` varchar(20) NOT NULL default '',
  `KEY2` varchar(20) NOT NULL default '',
  `KEY3` varchar(20) NOT NULL default '',
  `PERIOD` varchar(20) NOT NULL default '',
  `V1` double default NULL,
  `V2` double default NULL,
  PRIMARY KEY  (`KEY1`,`KEY2`,`KEY3`,`PERIOD`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

--
-- Table structure for table `projectname_items`
--

DROP TABLE IF EXISTS `projectname_items`;
CREATE TABLE IF NOT EXISTS `projectname_items` (
  `item_id` int(11) NOT NULL,
  `KEY1` varchar(20) default NULL,
  `KEY2` varchar(20) default NULL,
  `KEY3` varchar(20) default NULL,
  PRIMARY KEY  (`item_id`),
  UNIQUE KEY `KEY1` (`KEY1`,`KEY2`,`KEY3`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
