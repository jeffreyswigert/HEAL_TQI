/*******************************************************************************
This performs Therapist Type analysis 

For "Therapist Quality Inequality: Evidence from an Online Therapy Platform"

Coded by: Jacob Mortensen, Mitchell Zufelt, Jeff Rowley & Dr. Jeff Swigert

INPUTS: 
		- vam_analysis_sample.dta

OUTPUT:
		-
********************************************************************************/
clear
set more off

cd "/Volumes/Extreme SSD/Library/Therapist_Value_Added_Local/" 
global data "Data"
global analysis "Analysis"
global output "Output"
cap log close 
log using "$output/VAM_prelim_Jeff.txt", replace
use "$data/vam_analysis_sample_all_vars.dta", clear

*use "E:\vam_analysis_sample.dta"

keep if om_scale_id == 4


