# compara-carreras

ComparaCarreras.org (CompareMajors, in Spanish) is an interactive tool for prospective college students to view and compare key statistics about graduates of different academic majors in the labor market. Its goals are to help young people make evidence-based decisions and to consider an economic perspective when choosing an academic major in college. The site presents a profile for university programs in Mexico, with data such as their earnings, tuiton costs,employment rates and return on investment, and allows easy comparisons between different majors. Compara Carreras is a project I led for several years while working as a researcher at IMCO, a public policy thinktank in Mexico. 


This repository contains the R scripts to transform the project's source data and the final results in csv and xlsx files. The csv files were used by the project's database managers to update each year's results and the xlsx file was used to communicate the results to a wider audience. 

The script 00_ComparaCarreras_2022 is the main script for all the analysis, as all others scripts run different steps within it, and it creates a function called ComparaCarreras. A description for the arguments that the ComparaCarreras function takes can be found inside its scripts. In order to replicate this project, you need to download the sociodemographic microdata table of the ENOE, a household survey done by INEGI, from this institute's official site.


