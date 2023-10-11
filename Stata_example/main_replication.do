
clear 
*GLOBALS
*set the path to "YOUR PATH"
local path="YOURPATH"
		
		global raw  "`path'/Data/Raw Data/"
		global data "`path'/Data/Clean Data/"
		global main "`path'/Results/Main Text/"
		global appendix "`path'/Results/Appendix/" 
		global code "`path'/Code/" 

do "${code}config.do"
// do "${code}datacleaning.do"
// do "${code}analysis.do"

do "${code}datacleaning_replication.do"
do "${code}analysis_replication.do"