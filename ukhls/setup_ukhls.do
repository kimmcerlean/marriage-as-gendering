set maxvar 10000

// to get your hostname to add your own macros to file, type: display "`c(hostname)'"

* Set home directory based on computing environment. 
if `"`c(hostname)'"' == "LAPTOP-TP2VHI6B" global homedir `"C:/Users/mcerl/OneDrive - Istituto Universitario Europeo"' // One Drive on Kim's PC
if `"`c(hostname)'"' == "PPRC-STATS-P01" global homedir `"T:"' // PRC Stats Server
if `"`c(hostname)'"' == "60018D" global homedir `"C:/Users/kmcerlea/OneDrive - Istituto Universitario Europeo"' // One Drive on EUI Computer
// if `"`c(hostname)'"' == "{hostname}" global homedir

* This is where your code is. It is the directory you should change into before executing any files
if `"`c(hostname)'"' == "LAPTOP-TP2VHI6B" global code "G:/Other computers/My Laptop/Documents/GitHub/marriage-as-gendering/ukhls"
if `"`c(hostname)'"' == "60018D" global code "\\bfsrv2\home$\kmcerlea\PersonalData\Documents\GitHub\marriage-as-gendering\ukhls"
// if `"`c(hostname)'"' == "{hostname}" global code

* This locations of folders containing the original data files
global UKHLS "$homedir/datasets/UKHLS/UKDA-6614-stata/stata/stata13_se"
global UKHLS_mh "$homedir/datasets/UKHLS/UKDA-8473-stata/stata"

* created data files
global created_data "$homedir/projects/Marriage as Gendering/created data/ukhls"

* temporary data processing files
global temp "$homedir/projects/Marriage as Gendering/temp data/ukhls"

* File for results
global results "$homedir/projects/Marriage as Gendering/results/ukhls"

cd "$code"

set scheme cleanplots