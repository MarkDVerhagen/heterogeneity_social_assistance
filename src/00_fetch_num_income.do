use RINPERSOON percsm_2017 using "G:\Maatwerk\STAPELINGSMONITOR\2017\geconverteerde data\200930 Stapelingsmonitor SZW 2017 V3.dta"

export delimited using "H:\data\numeric_income\2017\rin_num_income.csv", replace

use RINPERSOON percsm_2018 using "G:\Maatwerk\STAPELINGSMONITOR\2018\geconverteerde data\200930 Stapelingsmonitor SZW 2018 V1.dta", clear

export delimited using "H:\data\numeric_income\2018\rin_num_income.csv", replace