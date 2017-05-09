#' ---
#' title: "Falls Among Older Women in Primary Care"
#' author: "Alex F. Bokov"
#' date: "05/09/2017"
#' output: html_document
#' ---
#' 
#' ## Load libraries
require(survival); library(magrittr); require("dplyr");
require('data.table'); require("ggplot2"); require('MASS'); 
require('readr');
#' ## Set variables, 1st pass
session <- 'session.rdata';
inputdata <- 'falls.csv'; datadict <- 'df_dynsql.csv';
#' ## Load data
if(session %in% list.files()) load(session);
if(!exists('dd')) dd <- read_csv(datadict,na='');
if(!exists('d0')) d0 <- read_csv(inputdata,na='');
#' ## Set variables, 2nd pass
demcols <- names(d0)[1:7];
#' Clean up data
