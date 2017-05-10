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
if(!exists('dd')) {
  rebuild <- c(rebuild,'dd');
  dd <- read_csv(datadict,na='');
  dd$present <- T;
}
cols2drop <- c('start_date','birth_date','sex_cd','v063_VTMN_D_info');
#' We are going to delete the inactive diagnoses for now, under the following 
#' reasoning: if the diagnosis is historical, it isn't a dynamic reflection of
#' the patient's state. If it is deleted or resolved, then it won't persist in
#' future visits anyway. Leaving the fall codes in for now, though, pending a 
#' closer look.
cols2drop <- c(cols2drop,
               subset(dd,rule='diag')$colname %>% 
                 grep('_inactive',.,val=T) %>% 
                 grep('_trpng_stmblng_|_ACCDNTL_FLLS_',.,inv=T,val=T));
demcols <- c('patient_num','race_cd','language_cd','age_at_visit_days')
#' ## Load data
if(session %in% list.files()) load(session);
if(!exists('d0')) {
  rebuild <- c(rebuild,'d0');
  d0 <- read_csv(inputdata,na='');
}
#' ## Set variables, 2nd pass
demcols <- c(demcols,subset(dd,rule=='ethnicity')$colname);
syn_diag_active <- list(
  c('v065_trpng_stmblng','v041_ACCDNTL_FLLS'),
  c('v048_rsprtr_unspcfd','v014_rsprtr_unspcfd'),
  c('v049_Act_brnchts','v015_brnchts_brnchlts','v016_Act_brnchts'),
  c('v030_invlvng_rsprtr','v031_Cgh'),
  c('v050_Vsmtr_rhnts','v017_rhnts_unspcfd'),
  c('v039_lprtn_lpdms','v002_Dsrdrs_mtblsm'),
  c('v051_Gstr_esphgl','v018_Dss_esphgs','v019_dsrdrs_esphgs'),
  c('v054_dsrdrs_urnr','v022_infctn_spcfd'),
  c('v059_R_Plr','v033_Smptms_invlvng'),
  c('v052_Dvrtclr_intstn','v020_Dvrtcl_cln'),
  c('v057_invlvng_dgstv','v032_invlvng_dgstv'),
  c('v053_K_Cnstptn','v021_Cnstptn'),
  c('v040_elctrlt_acd_bs','v003_elctrlt_acd_bs'),
  c('v043_Unspcfd_dmnt','v005_Dmnt_unspcfd'),
  c('v046_Alzhmr_s_ds','v009_Alzhmr_s_ds','v008_crbrl_dgnrtns'),
  c('v062_Smptms_cncrng','v026_mtblsm_dvlpmnt','v027_Abnrml_undrwght'),
  c('v047_G_Slp_dsrdrs','v024_Slp_dstrbncs'),
  c('v044_dprsv_dsrdr','v007_Dprsv_clsfd'),
  c('v045_anxt_dsrdrs','v006_dsctv_smtfrm'),
  c('v038_Vtmn_dfcnc','v001_Vtmn_dfcnc'),
  c('v037_Dfcnc_vtmns','v000_Dfcnc_cmpnts'),
  c('v061_Mls_and_ftg','v025_Mls_and_ftg')
);
#' ## Clean up data
if('d0' %in% rebuild) {
  # Remove impossible START_DATEs (2 of them)
  d0 <- subset(d0,age_at_visit_days > 0);
  # Drop non-informative columns
  d0[,cols2drop] <- NULL;
  dd[dd$colname%in%cols2drop,'present'] <- F;
  # English-speaker or not
  d0$language_cd <- cl_bintail(d0$language_cd,1);
  # Hispanic, Non-Hispanic, or other
  d0$v042_Ethnct <- cl_bintail(d0$v042_Ethnct,2);
  # Race: white, black, asian, other
  d0$race_cd <- cl_bintail(d0$race_cd,4,2);
  # replace all diagnoses with T/F values
  d0[,subset(dd,rule=='diag'&present)$colname]<-sapply(d0[,subset(dd,rule=='diag'&present)$colname],function(xx) !is.na(xx));
  # Flag aberrant values
  ifelse(grepl('\'TNP\'',d0$v064_VTMN_TTL_1990_1_info)
         ,NA,d0$v064_VTMN_TTL_1990_1_info) %>% 
    ifelse(grepl('\'L\'',.),'L',.) %>% 
    ifelse(grepl('\'H\'',.),'H',.) %>% 
    ifelse(is.na(.),ifelse(is.na(d0$v064_VTMN_TTL_1990_1_num),NA,'OK'),.) -> 
    d0$v064_VTMN_TTL_1990_1_info;
  for(ii in syn_diag_active) {
    d0[,ii[1]]<-apply(d0[,ii],1,any);
    d0[,ii[-1]]<-NULL;
    dd[dd$colname%in%ii[-1],'present'] <- F;
    };
  save.image(session);
}
#' ## Exploration of possibly combinable variables
#' The active diagnoses
subset(dd,rule=='diag'&present)$colname %>%
  intersect(names(d0)) %>% `[`(d0,,.) %>% as.matrix(.)+0 -> .foo
  varclus(.foo,similarity='bothpos') -> vc1;
plot(vc1$hclust);
