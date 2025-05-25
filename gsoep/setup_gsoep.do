clear all
set maxvar 10000

// to get your hostname to add your own macros to file, type: display "`c(hostname)'"

* Set home directory based on computing environment. 
if `"`c(hostname)'"' == "LAPTOP-TP2VHI6B" global homedir `"C:/Users/mcerl/OneDrive - Istituto Universitario Europeo"' // One Drive on Kim's PC
if `"`c(hostname)'"' == "PPRC-STATS-P01" global homedir `"T:"' // PRC Stats Server
if `"`c(hostname)'"' == "60018D" global homedir `"C:/Users/kmcerlea/OneDrive - Istituto Universitario Europeo"' // One Drive on EUI Computer
// if `"`c(hostname)'"' == "{hostname}" global homedir

* This is where your code is. It is the directory you should change into before executing any files
if `"`c(hostname)'"' == "LAPTOP-TP2VHI6B" global code "G:/Other computers/My Laptop/Documents/GitHub/marriage-as-gendering/gsoep"
if `"`c(hostname)'"' == "60018D" global code "\\bfsrv2\home$\kmcerlea\PersonalData\Documents\GitHub\marriage-as-gendering\gsoep"
// if `"`c(hostname)'"' == "{hostname}" global code

* This locations of folders containing the original data files
global GSOEP "$homedir/datasets/GSOEP/Stata"

* created data files
global created_data "$homedir/projects/Marriage as Gendering/created data/gsoep"

* temporary data processing files
global temp "$homedir/projects/Marriage as Gendering/temp data/gsoep"

* File for results
global results "$homedir/projects/Marriage as Gendering/results/gsoep"

cd "$code"

set scheme cleanplots