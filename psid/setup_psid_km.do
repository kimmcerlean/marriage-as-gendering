********************************************************************************
* Project: Marriage as a Gendering Institution
* Owner: Kimberly McErlean
* Started: September 2024
* File: setup_psid_km.do
********************************************************************************

********************************************************************************
* Description
********************************************************************************
* this file sets up the macros for the folder structure

* My home directory varies based on which computing environment I am in

if `"`c(hostname)'"' == "LAPTOP-TP2VHI6B" global homedir `"C:/Users/mcerl/OneDrive - Istituto Universitario Europeo/projects/Marriage as Gendering"' // Kim's Personal Computer
// if `"`c(hostname)'"' == "LAPTOP-TP2VHI6B" global homedir `"G:/Other computers/My Laptop/Documents"' // Kim's Personal Computer
if `"`c(hostname)'"' == "PPRC-STATS-P01" global homedir `"T:/Research Projects/Marriage as Gendering"' // PRC Stats Server
if `"`c(hostname)'"' == "60018D" global homedir `"C:/Users/kmcerlea/OneDrive - Istituto Universitario Europeo/projects"' // EUI 

* This is the base directory with the setup files.
* It is the directory you should change into before executing any files
global code "$homedir/github/marriage-as-gendering/psid"

* This locations of folders containing the original data files
global PSID "$homedir/data/PSID"

* created data files
global created_data "$homedir/created data/psid"

* temporary processing files
global temp "$homedir/temp data/psid"

* results
global results "$homedir/results/psid" 

ssc install psmatch2, replace
ssc install xtfeis, replace

set seed 8675309
set scheme cleanplots