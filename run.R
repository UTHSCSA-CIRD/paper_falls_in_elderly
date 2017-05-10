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
require('Hmisc'); require('readr');
#' ...and local functions...
source('functions.R');
#' ## Set variables, 1st pass
session <- 'session.rdata';
inputdata <- 'falls.csv'; datadict <- 'df_dynsql.csv';
rebuild <- c();
cols2drop <- c('start_date','birth_date','sex_cd','v063_VTMN_D_info');
demcols <- c('patient_num','race_cd','language_cd','age_at_visit_days')
#' ## Load data
if(session %in% list.files()) load(session);
if(!exists('dd')) {
  rebuild <- c(rebuild,'dd');
  dd <- read_csv(datadict,na='');
}
if(!exists('d0')) {
  rebuild <- c(rebuild,'d0');
  d0 <- read_csv(inputdata,na='');
}
#' ## Set variables, 2nd pass
demcols <- c(demcols,subset(dd,rule=='ethnicity')$colname);
#' ## Clean up data
if('d0' %in% rebuild) {
  # Remove impossible START_DATEs (2 of them)
  d0 <- subset(d0,age_at_visit_days > 0);
  # Drop non-informative columns
  d0[,cols2drop] <- NULL;
  # English-speaker or not
  d0$language_cd <- cl_bintail(d0$language_cd,1);
  # Hispanic, Non-Hispanic, or other
  d0$v042_Ethnct <- cl_bintail(d0$v042_Ethnct,2);
  # Race: white, black, asian, other
  d0$race_cd <- cl_bintail(d0$race_cd,4,2);
  # replace all diagnoses with T/F values
  d0[,subset(dd,rule=='diag')$colname]<-sapply(d0[,subset(dd,rule=='diag')$colname],function(xx) !is.na(xx));
  # Flag aberrant values
  ifelse(grepl('\'TNP\'',d0$v064_VTMN_TTL_1990_1_info)
         ,NA,d0$v064_VTMN_TTL_1990_1_info) %>% 
    ifelse(grepl('\'L\'',.),'L',.) %>% 
    ifelse(grepl('\'H\'',.),'H',.) %>% 
    ifelse(is.na(.),ifelse(is.na(d0$v064_VTMN_TTL_1990_1_num),NA,'OK'),.) -> 
    d0$v064_VTMN_TTL_1990_1_info;
  save.image(session);
}
#' ## Exploration of possibly combinable variables
#' The active diagnoses
d0[,grep('_inactive',subset(dd,rule=='diag')$colname,val=T,invert = T)] %>%
  as.matrix()+0 %>% varclus(similarity='bothpos') -> vc1;
plot(vc1$hclust);
#' The inactive diagnoses
d0[,grep('_inactive',subset(dd,rule=='diag')$colname,val=T)] %>%
  as.matrix()+0 %>% varclus(similarity='bothpos') -> vc2;
plot(vc2$hclust);
