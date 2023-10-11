clear
set more off
u "${data}choice_experiments.dta"

*As pre-registered, drop Mturk participant with inconsistent alpha value
drop if study!=1 & alphavaluefinal==. /*(study1 are profs and there's no MPL')*/

global covariates "wave2 wave3 professionalscloudresearch incentiveshigh##incentiveleft age female"	
global covariates2 "professionalsfree seeincentivecostly seequalitycostly wave2 wave3 professionalscloudresearch incentiveshigh incentiveleft incentiveshigh_incentiveleft age female"

eststo:reg choicebefore i.treatment $covariates if Highx10==0 & Highx100==0, vce(hc3)
margins i.treatment, atmeans saving("${main}Choice_adjusted_demand", replace)

*return list
matrix coeffs=r(table)
matrix list coeffs

g preddemand=coeffs[1,1] if treatment==0
g sepreddemand=coeffs[2,1] if treatment==0
forval i=2(1)4{
	local j=`i'-1
replace preddemand=coeffs[1,`i'] if treatment==`j'
replace sepreddemand=coeffs[2,`i'] if treatment==`j'
}
g ubpreddemand=preddemand+1.96*sepreddemand
g lbpreddemand=preddemand-1.96*sepreddemand

g meancommit=1

******************************************************************************************************************************************
******************************************************************************************************************************************
** Tables
******************************************************************************************************************************************
******************************************************************************************************************************************

*TABLE 1: PREFERENCES FOR INFORMATION ORDER (with Corrected Selfishness)
est clear
eststo:reg choicebefore $covariates2 if Highx10==0 & Highx100==0, vce(hc3)
eststo:reg choicebefore $covariates2 stdalpha if Highx10==0 & Highx100==0 & professionals==0, vce(hc3)
eststo:reg choicebefore $covariates2 stdalpha selfishseeincentivecostly selfishseequalitycostly if Highx10==0 & Highx100==0, vce(hc3)
test selfishseeincentivecostly + stdalpha = 0
test selfishseequalitycostly + stdalpha = 0

local panel "\begin{table}[h!]" "\centering" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\fontsize{10}{11}\selectfont" "\caption{Preference for Information Order (with Corrected \emph{Selfishness})}" "\begin{tabular}{l*{3}{c}} \hline" 
local dv "$\ \ \ \ \ \ \ \ \ \ \ \ \ \ $    &\multicolumn{3}{c}{\textbf{Prefer to See Incentive First}} \\\hline & & & \\"

esttab using  "${main}Choice_Demand_Corrected.tex", ///
 se r2 replace nolines cells(b(fmt(3)) se(par fmt(3))) ///
coeflabel (professionalsfree "Choice Free -- Professionals    $  \ \ \ \ \ \ \  $" seeincentivecostly "See Incentive First Costly          " seequalitycostly "Assess Quality First Costly" wave2 "Wave 2" wave3 "Wave 3" ///
female "Female" age "Age" stdalpha "Selfishness" professionalscloudresearch "Professionals $\times$ Cloudresearch" selfishseeincentivecostly "See Incentive First Costly $\times$ Selfishness       " selfishseequalitycostly "Access Quality First Costly $\times$ Selfishness            " ///
incentiveshigh "Probabilistic Incentive Mturk" incentiveleft "Order" incentiveshigh_incentiveleft "Probabilistic Incentive $\times$ Order") ///
order (seeincentivecostly seequalitycostly  professionalsfree stdalpha selfishseeincentivecostly selfishseequalitycostly female age wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch ) ///
collabels(none)  ///
drop(wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch) ///
nomtitle label substitute(" 0.000 " " " " (.) " " ") ///
nonotes ///
prehead("`panel'") posthead("`dv'") postfoot("\hline" "\end{tabular}%" "\captionsetup{width=0.8\textwidth}" "\caption*{\footnotesize \textit{Notes:} This table displays the estimated coefficients from linear probability models on the preference to see the incentive first. See Incentive First Costly and Assess Quality First Costly are indicator variables that take value 1 in the respective treatment, 0 otherwise. Selfishness was elicited at the end of the experiment, using a multiple price list (MPL) with 5 decisions. The variable is a standardized measure of the number of times the advisor chose to recommend the incentivized product in the MPL task. The regression models in columns (2) and (3) include individual controls for the advisor's gender and age, each wave of the experiment, whether incentives were probabilistic, the position of the products on the screen and the interaction between these two variables. Robust standard errors (HC3) in parentheses.}" "\label{tab:demand_corrected}" "\end{table}")

esttab using  "${main}Choice_Demand_Corrected_p.tex", ///
 se r2 replace nolines cells(b(fmt(3)) se(par fmt(3)) p(par(`"["' `"]"'))) ///
coeflabel (professionalsfree "Choice Free -- Professionals    $  \ \ \ \ \ \ \  $" seeincentivecostly "See Incentive First Costly          " seequalitycostly "Assess Quality First Costly" wave2 "Wave 2" wave3 "Wave 3" ///
female "Female" age "Age" stdalpha "Selfishness" professionalscloudresearch "Professionals $\times$ Cloudresearch" selfishseeincentivecostly "See Incentive First Costly $\times$ Selfishness       " selfishseequalitycostly "Access Quality First Costly $\times$ Selfishness            " ///
incentiveshigh "Probabilistic Incentive Mturk" incentiveleft "Order" incentiveshigh_incentiveleft "Probabilistic Incentive $\times$ Order") ///
order (seeincentivecostly seequalitycostly  professionalsfree stdalpha selfishseeincentivecostly selfishseequalitycostly female age wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch ) ///
collabels(none)  ///
drop(wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch) ///
nomtitle label substitute(" 0.000 " " " " (.) " " ") ///
nonotes ///
prehead("`panel'") posthead("`dv'") postfoot("\hline" "\end{tabular}%" "\captionsetup{width=0.8\textwidth}" "\caption*{\footnotesize \textit{Notes:} This table displays the estimated coefficients from linear probability models on the preference to see the incentive first. See Incentive First Costly and Assess Quality First Costly are indicator variables that take value 1 in the respective treatment, 0 otherwise. Selfishness was elicited at the end of the experiment, using a multiple price list (MPL) with 5 decisions. The variable is a standardized measure of the number of times the advisor chose to recommend the incentivized product in the MPL task. The regression models in columns (2) and (3) include individual controls for the advisor's gender and age, each wave of the experiment, whether incentives were probabilistic, the position of the products on the screen and the interaction between these two variables. Robust standard errors (HC3) in parentheses.}" "\label{tab:demand_corrected_p}" "\end{table}")

*TABLE 2: PREFERENCES FOR INFORMATION ORDER (Replace Selfishness with 5 Dummies)
est clear
eststo:reg choicebefore $covariates2 if Highx10==0 & Highx100==0, vce(hc3)
eststo:reg choicebefore $covariates2 ib5.alphavaluefinal if Highx10==0 & Highx100==0 & professionals==0, vce(hc3)
eststo:reg choicebefore $covariates2 ib5.alphavaluefinal##seeincentivecostly ib5.alphavaluefinal##seequalitycostly if Highx10==0 & Highx100==0, vce(hc3)

test i1.alphavaluefinal == i4.alphavaluefinal
test i2.alphavaluefinal == i3.alphavaluefinal

local panel "\begin{table}[h!]" "\centering" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\fontsize{10}{11}\selectfont" "\caption{Preference for Information Order (Replace \emph{Selfishness} with 5 Dummies)}" "\begin{tabular}{l*{3}{c}} \hline" 
local dv "$\ \ \ \ \ \ \ \ \ \ \ \ \ \ $    &\multicolumn{3}{c}{\textbf{Prefer to See Incentive First}} \\\hline & & & \\"

esttab using  "${main}Choice_Demand_FiveFactors.tex", ///
 se r2 replace nolines cells(b(fmt(3)) se(par fmt(3))) ///
coeflabel (professionalsfree "Choice Free -- Professionals    $  \ \ \ \ \ \ \  $" seeincentivecostly "See Incentive First Costly" seequalitycostly "Assess Quality First Costly" wave2 "Wave 2" wave3 "Wave 3" ///
female "Female" age "Age" professionalscloudresearch "Professionals $\times$ Cloudresearch" selfishseeincentivecostly "See Incentive First Costly $\times$ Selfishness" selfishseequalitycostly "Access Quality First Costly $\times$ Selfishness" ///
incentiveshigh "Probabilistic Incentive Mturk" incentiveleft "Order" incentiveshigh_incentiveleft "Probabilistic Incentive $\times$ Order") ///
order (seeincentivecostly seequalitycostly  professionalsfree stdalpha selfishseeincentivecostly selfishseequalitycostly female age wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch ) ///
collabels(none)  ///
drop(wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch) ///
nomtitle label substitute(" 0.000 " " " " (.) " " ") ///
nonotes ///
prehead("`panel'") posthead("`dv'") postfoot("\hline" "\end{tabular}%" "\captionsetup{width=0.8\textwidth}" "\caption*{\footnotesize \textit{Notes:} This table displays the estimated coefficients from linear probability models on the preference to see the incentive first. See Incentive First Costly and Assess Quality First Costly are indicator variables that take value 1 in the respective treatment, 0 otherwise. Selfishness was elicited at the end of the experiment, using a multiple price list (MPL) with 5 decisions. Five dummy variables for alphavalue correspond to the number of times the advisor chose to recommend the incentivized product in the MPL task. The regression models in columns (2) and (3) include individual controls for the advisor's gender and age, each wave of the experiment, whether incentives were probabilistic, the position of the products on the screen and the interaction between these two variables. Robust standard errors (HC3) in parentheses.}" "\label{tab:demand_Dummies}" "\end{table}")

esttab using  "${main}Choice_Demand_FiveFactors_p.tex", ///
 se r2 replace nolines cells(b(fmt(3)) se(par fmt(3)) p(par(`"["' `"]"'))) ///
coeflabel (professionalsfree "Choice Free -- Professionals    $  \ \ \ \ \ \ \  $" seeincentivecostly "See Incentive First Costly" seequalitycostly "Assess Quality First Costly" wave2 "Wave 2" wave3 "Wave 3" ///
female "Female" age "Age" professionalscloudresearch "Professionals $\times$ Cloudresearch" selfishseeincentivecostly "See Incentive First Costly $\times$ Selfishness" selfishseequalitycostly "Access Quality First Costly $\times$ Selfishness" ///
incentiveshigh "Probabilistic Incentive Mturk" incentiveleft "Order" incentiveshigh_incentiveleft "Probabilistic Incentive $\times$ Order") ///
order (seeincentivecostly seequalitycostly  professionalsfree stdalpha selfishseeincentivecostly selfishseequalitycostly female age wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch ) ///
collabels(none)  ///
drop(wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch) ///
nomtitle label substitute(" 0.000 " " " " (.) " " ") ///
nonotes ///
prehead("`panel'") posthead("`dv'") postfoot("\hline" "\end{tabular}%" "\captionsetup{width=0.8\textwidth}" "\caption*{\footnotesize \textit{Notes:} This table displays the estimated coefficients from linear probability models on the preference to see the incentive first. See Incentive First Costly and Assess Quality First Costly are indicator variables that take value 1 in the respective treatment, 0 otherwise. Selfishness was elicited at the end of the experiment, using a multiple price list (MPL) with 5 decisions. Five dummy variables for alphavalue correspond to the number of times the advisor chose to recommend the incentivized product in the MPL task. The regression models in columns (2) and (3) include individual controls for the advisor's gender and age, each wave of the experiment, whether incentives were probabilistic, the position of the products on the screen and the interaction between these two variables. Robust standard errors (HC3) in parentheses.}" "\label{tab:demand_Dummies_p}" "\end{table}")

*TABLE 3: PREFERENCES FOR INFORMATION ORDER (with New Definition for Selfishness)
preserve
tab alphavalue
replace alphavalue = alphavalue - 2
replace alphavalue = 0 if alphavalue < 0
tab alphavalue
egen stdalpha_adjusted = std(alphavalue)

est clear
eststo:reg choicebefore $covariates2 if Highx10==0 & Highx100==0, vce(hc3)
eststo:reg choicebefore $covariates2 stdalpha_adjusted if Highx10==0 & Highx100==0 & professionals==0, vce(hc3)
eststo:reg choicebefore $covariates2 stdalpha_adjusted selfishseeincentivecostly selfishseequalitycostly if Highx10==0 & Highx100==0, vce(hc3)

local panel "\begin{table}[h!]" "\centering" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\fontsize{10}{11}\selectfont" "\caption{Preference for Information Order (Redefine \textit{Selfishness} on Q1 to Q3)}" "\begin{tabular}{l*{3}{c}} \hline" 
local dv "$\ \ \ \ \ \ \ \ \ \ \ \ \ \ $    &\multicolumn{3}{c}{\textbf{Prefer to See Incentive First}} \\\hline & & & \\"

esttab using  "${main}Choice_Demand_New_Definition.tex", ///
 se r2 replace nolines cells(b(fmt(3)) se(par fmt(3))) ///
coeflabel (professionalsfree "Choice Free -- Professionals    $  \ \ \ \ \ \ \  $" seeincentivecostly "See Incentive First Costly          " seequalitycostly "Assess Quality First Costly" wave2 "Wave 2" wave3 "Wave 3" ///
female "Female" age "Age" stdalpha_adjusted "Selfishness" professionalscloudresearch "Professionals $\times$ Cloudresearch" selfishseeincentivecostly "See Incentive First Costly $\times$ Selfishness       " selfishseequalitycostly "Access Quality First Costly $\times$ Selfishness            " ///
incentiveshigh "Probabilistic Incentive Mturk" incentiveleft "Order" incentiveshigh_incentiveleft "Probabilistic Incentive $\times$ Order") ///
order (seeincentivecostly seequalitycostly  professionalsfree stdalpha selfishseeincentivecostly selfishseequalitycostly female age wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch ) ///
collabels(none)  ///
drop(wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch) ///
nomtitle label substitute(" 0.000 " " " " (.) " " ") ///
nonotes ///
prehead("`panel'") posthead("`dv'") postfoot("\hline" "\end{tabular}%" "\captionsetup{width=0.8\textwidth}" "\caption*{\footnotesize \textit{Notes:} This table displays the estimated coefficients from linear probability models on the preference to see the incentive first. See Incentive First Costly and Assess Quality First Costly are indicator variables that take value 1 in the respective treatment, 0 otherwise. Selfishness was elicited at the end of the experiment, using a multiple price list (MPL) with only 3 decisions, excluding Q4 and Q5. The variable is a standardized measure of the number of times the advisor chose to recommend the incentivized product in the MPL task. The regression models in columns (2) and (3) include individual controls for the advisor's gender and age, each wave of the experiment, whether incentives were probabilistic, the position of the products on the screen and the interaction between these two variables. Robust standard errors (HC3) in parentheses.}" "\label{tab:demand_3Q}" "\end{table}")

esttab using  "${main}Choice_Demand_New_Definition_p.tex", ///
 se r2 replace nolines cells(b(fmt(3)) se(par fmt(3)) p(par(`"["' `"]"'))) ///
coeflabel (professionalsfree "Choice Free -- Professionals    $  \ \ \ \ \ \ \  $" seeincentivecostly "See Incentive First Costly          " seequalitycostly "Assess Quality First Costly" wave2 "Wave 2" wave3 "Wave 3" ///
female "Female" age "Age" stdalpha_adjusted "Selfishness" professionalscloudresearch "Professionals $\times$ Cloudresearch" selfishseeincentivecostly "See Incentive First Costly $\times$ Selfishness       " selfishseequalitycostly "Access Quality First Costly $\times$ Selfishness            " ///
incentiveshigh "Probabilistic Incentive Mturk" incentiveleft "Order" incentiveshigh_incentiveleft "Probabilistic Incentive $\times$ Order") ///
order (seeincentivecostly seequalitycostly  professionalsfree stdalpha selfishseeincentivecostly selfishseequalitycostly female age wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch ) ///
collabels(none)  ///
drop(wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch) ///
nomtitle label substitute(" 0.000 " " " " (.) " " ") ///
nonotes ///
prehead("`panel'") posthead("`dv'") postfoot("\hline" "\end{tabular}%" "\captionsetup{width=0.8\textwidth}" "\caption*{\footnotesize \textit{Notes:} This table displays the estimated coefficients from linear probability models on the preference to see the incentive first. See Incentive First Costly and Assess Quality First Costly are indicator variables that take value 1 in the respective treatment, 0 otherwise. Selfishness was elicited at the end of the experiment, using a multiple price list (MPL) with only 3 decisions, excluding Q4 and Q5. The variable is a standardized measure of the number of times the advisor chose to recommend the incentivized product in the MPL task. The regression models in columns (2) and (3) include individual controls for the advisor's gender and age, each wave of the experiment, whether incentives were probabilistic, the position of the products on the screen and the interaction between these two variables. Robust standard errors (HC3) in parentheses.}" "\label{tab:demand_3Q_p}" "\end{table}")
restore

*TABLE 4: PREFERENCES FOR INFORMATION ORDER (with Corrected Selfishness; Drop alphavalue < 2)
preserve
drop if alphavaluefinal <= 1
est clear
eststo:reg choicebefore $covariates2 if Highx10==0 & Highx100==0, vce(hc3)
eststo:reg choicebefore $covariates2 stdalpha if Highx10==0 & Highx100==0 & professionals==0, vce(hc3)
eststo:reg choicebefore $covariates2 stdalpha selfishseeincentivecostly selfishseequalitycostly if Highx10==0 & Highx100==0, vce(hc3)

local panel "\begin{table}[h!]" "\centering" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\fontsize{10}{11}\selectfont" "\caption{Preference for Information Order (with Corrected \emph{Selfishness}; Drop \\ \texttt{alphavalue} < 2)}" "\begin{tabular}{l*{3}{c}} \hline" 
local dv "$\ \ \ \ \ \ \ \ \ \ \ \ \ \ $    &\multicolumn{3}{c}{\textbf{Prefer to See Incentive First}} \\\hline & & & \\"

esttab using  "${main}Choice_Demand_Corrected_Drop.tex", ///
 se r2 replace nolines cells(b(fmt(3)) se(par fmt(3))) ///
coeflabel (professionalsfree "Choice Free -- Professionals    $  \ \ \ \ \ \ \  $" seeincentivecostly "See Incentive First Costly          " seequalitycostly "Assess Quality First Costly" wave2 "Wave 2" wave3 "Wave 3" ///
female "Female" age "Age" stdalpha "Selfishness" professionalscloudresearch "Professionals $\times$ Cloudresearch" selfishseeincentivecostly "See Incentive First Costly $\times$ Selfishness       " selfishseequalitycostly "Access Quality First Costly $\times$ Selfishness            " ///
incentiveshigh "Probabilistic Incentive Mturk" incentiveleft "Order" incentiveshigh_incentiveleft "Probabilistic Incentive $\times$ Order") ///
order (seeincentivecostly seequalitycostly  professionalsfree stdalpha selfishseeincentivecostly selfishseequalitycostly female age wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch ) ///
collabels(none)  ///
drop(wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch) ///
nomtitle label substitute(" 0.000 " " " " (.) " " ") ///
nonotes ///
prehead("`panel'") posthead("`dv'") postfoot("\hline" "\end{tabular}%" "\captionsetup{width=0.8\textwidth}" "\caption*{\footnotesize \textit{Notes:} We drop the samples with alphavalue = 0 or alphavalue = 1 in the regression. This table displays the estimated coefficients from linear probability models on the preference to see the incentive first. See Incentive First Costly and Assess Quality First Costly are indicator variables that take value 1 in the respective treatment, 0 otherwise. Selfishness was elicited at the end of the experiment, using a multiple price list (MPL) with 5 decisions. The variable is a standardized measure of the number of times the advisor chose to recommend the incentivized product in the MPL task. The regression models in columns (2) and (3) include individual controls for the advisor's gender and age, each wave of the experiment, whether incentives were probabilistic, the position of the products on the screen and the interaction between these two variables. Robust standard errors (HC3) in parentheses.}" "\label{tab:demand_Corrected_Subsample}" "\end{table}")

esttab using  "${main}Choice_Demand_Corrected_Drop_p.tex", ///
 se r2 replace nolines cells(b(fmt(3)) se(par fmt(3)) p(par(`"["' `"]"'))) ///
coeflabel (professionalsfree "Choice Free -- Professionals    $  \ \ \ \ \ \ \  $" seeincentivecostly "See Incentive First Costly          " seequalitycostly "Assess Quality First Costly" wave2 "Wave 2" wave3 "Wave 3" ///
female "Female" age "Age" stdalpha "Selfishness" professionalscloudresearch "Professionals $\times$ Cloudresearch" selfishseeincentivecostly "See Incentive First Costly $\times$ Selfishness       " selfishseequalitycostly "Access Quality First Costly $\times$ Selfishness            " ///
incentiveshigh "Probabilistic Incentive Mturk" incentiveleft "Order" incentiveshigh_incentiveleft "Probabilistic Incentive $\times$ Order") ///
order (seeincentivecostly seequalitycostly  professionalsfree stdalpha selfishseeincentivecostly selfishseequalitycostly female age wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch ) ///
collabels(none)  ///
drop(wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch) ///
nomtitle label substitute(" 0.000 " " " " (.) " " ") ///
nonotes ///
prehead("`panel'") posthead("`dv'") postfoot("\hline" "\end{tabular}%" "\captionsetup{width=0.8\textwidth}" "\caption*{\footnotesize \textit{Notes:} We drop the samples with alphavalue = 0 or alphavalue = 1 in the regression. This table displays the estimated coefficients from linear probability models on the preference to see the incentive first. See Incentive First Costly and Assess Quality First Costly are indicator variables that take value 1 in the respective treatment, 0 otherwise. Selfishness was elicited at the end of the experiment, using a multiple price list (MPL) with 5 decisions. The variable is a standardized measure of the number of times the advisor chose to recommend the incentivized product in the MPL task. The regression models in columns (2) and (3) include individual controls for the advisor's gender and age, each wave of the experiment, whether incentives were probabilistic, the position of the products on the screen and the interaction between these two variables. Robust standard errors (HC3) in parentheses.}" "\label{tab:demand_Corrected_Subsample_p}" "\end{table}")
restore

*TABLE 5: PREFERENCES FOR INFORMATION ORDER (Replace Selfishness with 5 Dummies; Drop alphavalue < 2)
preserve
drop if alphavaluefinal <= 1
est clear
eststo:reg choicebefore $covariates2 if Highx10==0 & Highx100==0, vce(hc3)
eststo:reg choicebefore $covariates2 ib5.alphavaluefinal if Highx10==0 & Highx100==0 & professionals==0, vce(hc3)
eststo:reg choicebefore $covariates2 ib5.alphavaluefinal##seeincentivecostly ib5.alphavaluefinal##seequalitycostly if Highx10==0 & Highx100==0, vce(hc3)

local panel "\begin{table}[h!]" "\centering" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\fontsize{10}{11}\selectfont" "\caption{Preference for Information Order (Replace \emph{Selfishness} with 5 Dummies; Drop \texttt{alphavalue} < 2)}" "\begin{tabular}{l*{3}{c}} \hline" 
local dv "$\ \ \ \ \ \ \ \ \ \ \ \ \ \ $    &\multicolumn{3}{c}{\textbf{Prefer to See Incentive First}} \\\hline & & & \\"

esttab using  "${main}Choice_Demand_FiveFactors_Drop.tex", ///
 se r2 replace nolines cells(b(fmt(3)) se(par fmt(3))) ///
coeflabel (professionalsfree "Choice Free -- Professionals    $  \ \ \ \ \ \ \  $" seeincentivecostly "See Incentive First Costly" seequalitycostly "Assess Quality First Costly" wave2 "Wave 2" wave3 "Wave 3" ///
female "Female" age "Age" professionalscloudresearch "Professionals $\times$ Cloudresearch" selfishseeincentivecostly "See Incentive First Costly $\times$ Selfishness" selfishseequalitycostly "Access Quality First Costly $\times$ Selfishness" ///
incentiveshigh "Probabilistic Incentive Mturk" incentiveleft "Order" incentiveshigh_incentiveleft "Probabilistic Incentive $\times$ Order") ///
order (seeincentivecostly seequalitycostly  professionalsfree stdalpha selfishseeincentivecostly selfishseequalitycostly female age wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch ) ///
collabels(none)  ///
drop(wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch) ///
nomtitle label substitute(" 0.000 " " " " (.) " " ") ///
nonotes ///
prehead("`panel'") posthead("`dv'") postfoot("\hline" "\end{tabular}%" "\captionsetup{width=0.8\textwidth}" "\caption*{\footnotesize \textit{Notes:} We drop the samples with alphavalue = 0 or alphavalue = 1 in the regression. This table displays the estimated coefficients from linear probability models on the preference to see the incentive first. See Incentive First Costly and Assess Quality First Costly are indicator variables that take value 1 in the respective treatment, 0 otherwise. Selfishness was elicited at the end of the experiment, using a multiple price list (MPL) with 5 decisions. Three dummy variables for alphavalue correspond to the number of times the advisor chose to recommend the incentivized product in the MPL task. The regression models in columns (2) and (3) include individual controls for the advisor's gender and age, each wave of the experiment, whether incentives were probabilistic, the position of the products on the screen and the interaction between these two variables. Robust standard errors (HC3) in parentheses.}" "\label{tab:demand_Dummies_Subsample}" "\end{table}")

esttab using  "${main}Choice_Demand_FiveFactors_Drop_p.tex", ///
 se r2 replace nolines cells(b(fmt(3)) se(par fmt(3)) p(par(`"["' `"]"'))) ///
coeflabel (professionalsfree "Choice Free -- Professionals    $  \ \ \ \ \ \ \  $" seeincentivecostly "See Incentive First Costly" seequalitycostly "Assess Quality First Costly" wave2 "Wave 2" wave3 "Wave 3" ///
female "Female" age "Age" professionalscloudresearch "Professionals $\times$ Cloudresearch" selfishseeincentivecostly "See Incentive First Costly $\times$ Selfishness" selfishseequalitycostly "Access Quality First Costly $\times$ Selfishness" ///
incentiveshigh "Probabilistic Incentive Mturk" incentiveleft "Order" incentiveshigh_incentiveleft "Probabilistic Incentive $\times$ Order") ///
order (seeincentivecostly seequalitycostly  professionalsfree stdalpha selfishseeincentivecostly selfishseequalitycostly female age wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch ) ///
collabels(none)  ///
drop(wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch) ///
nomtitle label substitute(" 0.000 " " " " (.) " " ") ///
nonotes ///
prehead("`panel'") posthead("`dv'") postfoot("\hline" "\end{tabular}%" "\captionsetup{width=0.8\textwidth}" "\caption*{\footnotesize \textit{Notes:} We drop the samples with alphavalue = 0 or alphavalue = 1 in the regression. This table displays the estimated coefficients from linear probability models on the preference to see the incentive first. See Incentive First Costly and Assess Quality First Costly are indicator variables that take value 1 in the respective treatment, 0 otherwise. Selfishness was elicited at the end of the experiment, using a multiple price list (MPL) with 5 decisions. Three dummy variables for alphavalue correspond to the number of times the advisor chose to recommend the incentivized product in the MPL task. The regression models in columns (2) and (3) include individual controls for the advisor's gender and age, each wave of the experiment, whether incentives were probabilistic, the position of the products on the screen and the interaction between these two variables. Robust standard errors (HC3) in parentheses.}" "\label{tab:demand_Dummies_Subsample_p}" "\end{table}")
restore

*TABLE 8: PREFERENCES FOR INFORMATION ORDER (Include Incentivized Product B)
est clear
eststo:reg choicebefore incentiveB $covariates2 if Highx10==0 & Highx100==0, vce(hc3)
eststo:reg choicebefore incentiveB $covariates2 stdalpha if Highx10==0 & Highx100==0 & professionals==0, vce(hc3)
eststo:reg choicebefore incentiveB $covariates2 stdalpha selfishseeincentivecostly selfishseequalitycostly if Highx10==0 & Highx100==0, vce(hc3)

local panel "\begin{table}[h!]" "\centering" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\fontsize{10}{11}\selectfont" "\caption{Preference for Information Order}" "\begin{tabular}{l*{3}{c}} \hline" 
local dv "$\ \ \ \ \ \ \ \ \ \ \ \ \ \ $    &\multicolumn{3}{c}{\textbf{Prefer to See Incentive First}} \\\hline & & & \\"

esttab using  "${main}Choice_Demand_IncentiveB.tex", ///
 se r2 replace nolines cells(b(fmt(3)) se(par fmt(3))) ///
coeflabel (professionalsfree "Choice Free -- Professionals    $  \ \ \ \ \ \ \  $" seeincentivecostly "See Incentive First Costly          " seequalitycostly "Assess Quality First Costly" wave2 "Wave 2" wave3 "Wave 3" ///
female "Female" age "Age" stdalpha "Selfishness" professionalscloudresearch "Professionals $\times$ Cloudresearch" selfishseeincentivecostly "See Incentive First Costly $\times$ Selfishness       " selfishseequalitycostly "Access Quality First Costly $\times$ Selfishness            " ///
incentiveshigh "Probabilistic Incentive Mturk" incentiveleft "Order" incentiveshigh_incentiveleft "Probabilistic Incentive $\times$ Order" incentiveB "Incentive for B") ///
order (seeincentivecostly seequalitycostly  professionalsfree stdalpha selfishseeincentivecostly selfishseequalitycostly female age wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch ) ///
collabels(none)  ///
drop(wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch) ///
nomtitle label substitute(" 0.000 " " " " (.) " " ") ///
nonotes ///
prehead("`panel'") posthead("`dv'") postfoot("\hline" "\end{tabular}%" "\captionsetup{width=0.8\textwidth}" "\caption*{\footnotesize \textit{Notes:} This table displays the estimated coefficients from linear probability models on the preference to see the incentive first. See Incentive First Costly and Assess Quality First Costly are indicator variables that take value 1 in the respective treatment, 0 otherwise. Selfishness was elicited at the end of the experiment, using a multiple price list (MPL) with 5 decisions. The variable is a standardized measure of the number of times the advisor chose to recommend the incentivized product in the MPL task. The regression models in columns (2) and (3) include individual controls for the advisor's gender and age, each wave of the experiment, whether incentives were probabilistic, the position of the products on the screen, the interaction between these two variables, and whether the incentivized product is B. Robust standard errors (HC3) in parentheses.}" "\label{tab:demand_IncentiveB}" "\end{table}")

esttab using  "${main}Choice_Demand_IncentiveB_p.tex", ///
 se r2 replace nolines cells(b(fmt(3)) se(par fmt(3)) p(par(`"["' `"]"'))) ///
coeflabel (professionalsfree "Choice Free -- Professionals    $  \ \ \ \ \ \ \  $" seeincentivecostly "See Incentive First Costly          " seequalitycostly "Assess Quality First Costly" wave2 "Wave 2" wave3 "Wave 3" ///
female "Female" age "Age" stdalpha "Selfishness" professionalscloudresearch "Professionals $\times$ Cloudresearch" selfishseeincentivecostly "See Incentive First Costly $\times$ Selfishness       " selfishseequalitycostly "Access Quality First Costly $\times$ Selfishness            " ///
incentiveshigh "Probabilistic Incentive Mturk" incentiveleft "Order" incentiveshigh_incentiveleft "Probabilistic Incentive $\times$ Order" incentiveB "Incentive for B") ///
order (seeincentivecostly seequalitycostly  professionalsfree stdalpha selfishseeincentivecostly selfishseequalitycostly female age wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch ) ///
collabels(none)  ///
drop(wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch) ///
nomtitle label substitute(" 0.000 " " " " (.) " " ") ///
nonotes ///
prehead("`panel'") posthead("`dv'") postfoot("\hline" "\end{tabular}%" "\captionsetup{width=0.8\textwidth}" "\caption*{\footnotesize \textit{Notes:} This table displays the estimated coefficients from linear probability models on the preference to see the incentive first. See Incentive First Costly and Assess Quality First Costly are indicator variables that take value 1 in the respective treatment, 0 otherwise. Selfishness was elicited at the end of the experiment, using a multiple price list (MPL) with 5 decisions. The variable is a standardized measure of the number of times the advisor chose to recommend the incentivized product in the MPL task. The regression models in columns (2) and (3) include individual controls for the advisor's gender and age, each wave of the experiment, whether incentives were probabilistic, the position of the products on the screen, the interaction between these two variables, and whether the incentivized product is B. Robust standard errors (HC3) in parentheses.}" "\label{tab:demand_IncentiveB_p}" "\end{table}")

***************
* BLINDING **
***************

*TABLE 6: Preference for Information Order (Selfishness Replace By Blinding)
gen blindseeincentivecostly=chooseblindness*incentivecostly
gen blindseequalitycostly=chooseblindness*qualitycostly

est clear
eststo:reg choicebefore $covariates2 if Highx10==0 & Highx100==0, vce(hc3)
eststo:reg choicebefore $covariates2 chooseblindness if Highx10==0 & Highx100==0 & professionals==0, vce(hc3)
eststo:reg choicebefore $covariates2 chooseblindness blindseeincentivecostly blindseequalitycostly if Highx10==0 & Highx100==0, vce(hc3)

local panel "\begin{table}[h!]" "\centering" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\fontsize{10}{11}\selectfont" "\caption{Preference for Information Order (Wave 3 Only; Replace \emph{Selfishness} with \emph{Blinding})}" "\begin{tabular}{l*{3}{c}} \hline" 
local dv "$\ \ \ \ \ \ \ \ \ \ \ \ \ \ $    &\multicolumn{3}{c}{\textbf{Prefer to See Incentive First}} \\\hline & & & \\"

esttab using  "${main}Choice_Demand_blindness2.tex", ///
 se r2 replace nolines cells(b(fmt(3)) se(par fmt(3))) ///
coeflabel (professionalsfree "Choice Free -- Professionals    $  \ \ \ \ \ \ \  $" seeincentivecostly "See Incentive First Costly          " seequalitycostly "Assess Quality First Costly" wave2 "Wave 2" wave3 "Wave 3" ///
female "Female" age "Age" chooseblindness "Blinding" professionalscloudresearch "Professionals $\times$ Cloudresearch" blindseeincentivecostly "See Incentive First Costly $\times$ Blindness            " blindseequalitycostly "See Quality First Costly $\times$ Blindness            " ///
incentiveshigh "Probabilistic Incentive Mturk" incentiveleft "Order" incentiveshigh_incentiveleft "Probabilistic Incentive $\times$ Order") ///
order (seeincentivecostly seequalitycostly  professionalsfree chooseblindness blindseeincentivecostly blindseequalitycostly female age wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch ) ///
collabels(none)  ///
drop(wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch) ///
nomtitle label substitute(" 0.000 " " " " (.) " " ") ///
nonotes ///
prehead("`panel'") posthead("`dv'") postfoot("\hline" "\end{tabular}%" "\captionsetup{width=0.8\textwidth}" "\caption*{\footnotesize \textit{Notes:} This table displays the estimated coefficients from linear probability models on the preference to see the incentive first. See Incentive First Costly and Assess Quality First Costly are indicator variables that take value 1 in the respective treatment, 0 otherwise. Selfishness was elicited at the end of the experiment, using a multiple price list (MPL) with 5 decisions. The variable is a standardized measure of the number of times the advisor chose to recommend the incentivized product in the MPL task. The regression models in columns (2) and (3) include individual controls for the advisor's gender and age, each wave of the experiment, whether incentives were probabilistic, the position of the products on the screen and the interaction between these two variables. Robust standard errors (HC3) in parentheses.}" "\label{tab:demand_blinding}" "\end{table}")

esttab using  "${main}Choice_Demand_blindness2_p.tex", ///
 se r2 replace nolines cells(b(fmt(3)) se(par fmt(3)) p(par(`"["' `"]"'))) ///
coeflabel (professionalsfree "Choice Free -- Professionals    $  \ \ \ \ \ \ \  $" seeincentivecostly "See Incentive First Costly          " seequalitycostly "Assess Quality First Costly" wave2 "Wave 2" wave3 "Wave 3" ///
female "Female" age "Age" chooseblindness "Blinding" professionalscloudresearch "Professionals $\times$ Cloudresearch" blindseeincentivecostly "See Incentive First Costly $\times$ Blindness            " blindseequalitycostly "See Quality First Costly $\times$ Blindness            " ///
incentiveshigh "Probabilistic Incentive Mturk" incentiveleft "Order" incentiveshigh_incentiveleft "Probabilistic Incentive $\times$ Order") ///
order (seeincentivecostly seequalitycostly  professionalsfree chooseblindness blindseeincentivecostly blindseequalitycostly female age wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch ) ///
collabels(none)  ///
drop(wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch) ///
nomtitle label substitute(" 0.000 " " " " (.) " " ") ///
nonotes ///
prehead("`panel'") posthead("`dv'") postfoot("\hline" "\end{tabular}%" "\captionsetup{width=0.8\textwidth}" "\caption*{\footnotesize \textit{Notes:} This table displays the estimated coefficients from linear probability models on the preference to see the incentive first. See Incentive First Costly and Assess Quality First Costly are indicator variables that take value 1 in the respective treatment, 0 otherwise. Selfishness was elicited at the end of the experiment, using a multiple price list (MPL) with 5 decisions. The variable is a standardized measure of the number of times the advisor chose to recommend the incentivized product in the MPL task. The regression models in columns (2) and (3) include individual controls for the advisor's gender and age, each wave of the experiment, whether incentives were probabilistic, the position of the products on the screen and the interaction between these two variables. Robust standard errors (HC3) in parentheses.}" "\label{tab:demand_blinding_p}" "\end{table}")

test chooseblindness + blindseeincentivecostly  ==0
test chooseblindness + blindseequalitycostly  ==0

*TABLE 7: Preference for Information Order (Wave 3 Only)
preserve
keep if wave3 ==1
est clear
eststo:reg choicebefore $covariates2 if Highx10==0 & Highx100==0, vce(hc3)
eststo:reg choicebefore $covariates2 stdalpha if Highx10==0 & Highx100==0 & professionals==0, vce(hc3)
eststo:reg choicebefore $covariates2 stdalpha selfishseeincentivecostly selfishseequalitycostly if Highx10==0 & Highx100==0, vce(hc3)
restore
local panel "\begin{table}[h!]" "\centering" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\fontsize{10}{11}\selectfont" "\caption{Preference for Information Order (Wave 3 Only)}" "\begin{tabular}{l*{3}{c}} \hline" 
local dv "$\ \ \ \ \ \ \ \ \ \ \ \ \ \ $    &\multicolumn{3}{c}{\textbf{Prefer to See Incentive First}} \\\hline & & & \\"

esttab using  "${main}Choice_Demand_wave3.tex", ///
 se r2 replace nolines cells(b(fmt(3)) se(par fmt(3))) ///
coeflabel (seeincentivecostly "See Incentive First Costly          " seequalitycostly "Assess Quality First Costly" wave2 "Wave 2" wave3 "Wave 3" ///
female "Female" age "Age" stdalpha "Selfishness" professionalscloudresearch "Professionals $\times$ Cloudresearch" selfishseeincentivecostly "See Incentive First Costly $\times$ Selfishness       " selfishseequalitycostly "See Quality First Costly $\times$ Selfishness            " ///
incentiveshigh "Probabilistic Incentive Mturk" incentiveleft "Order" incentiveshigh_incentiveleft "Probabilistic Incentive $\times$ Order") ///
order (seeincentivecostly seequalitycostly stdalpha selfishseeincentivecostly selfishseequalitycostly female age wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch ) ///
collabels(none)  ///
drop(professionalsfree wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch) ///
nomtitle label substitute(" 0.000 " " " " (.) " " ") ///
nonotes ///
prehead("`panel'") posthead("`dv'") postfoot("\hline" "\end{tabular}%" "\captionsetup{width=0.8\textwidth}" "\caption*{\footnotesize \textit{Notes:} This table displays the estimated coefficients from linear probability models on the preference to see the incentive first. See Incentive First Costly and Assess Quality First Costly are indicator variables that take value 1 in the respective treatment, 0 otherwise. Selfishness was elicited at the end of the experiment, using a multiple price list (MPL) with 5 decisions. The variable is a standardized measure of the number of times the advisor chose to recommend the incentivized product in the MPL task. The regression models in columns (2) and (3) include individual controls for the advisor's gender and age, each wave of the experiment, whether incentives were probabilistic, the position of the products on the screen and the interaction between these two variables. Robust standard errors (HC3) in parentheses.}" "\label{tab:demand_wave3}" "\end{table}")

esttab using  "${main}Choice_Demand_wave3_p.tex", ///
 se r2 replace nolines cells(b(fmt(3)) se(par fmt(3)) p(par(`"["' `"]"'))) ///
coeflabel (seeincentivecostly "See Incentive First Costly          " seequalitycostly "Assess Quality First Costly" wave2 "Wave 2" wave3 "Wave 3" ///
female "Female" age "Age" stdalpha "Selfishness" professionalscloudresearch "Professionals $\times$ Cloudresearch" selfishseeincentivecostly "See Incentive First Costly $\times$ Selfishness       " selfishseequalitycostly "See Quality First Costly $\times$ Selfishness            " ///
incentiveshigh "Probabilistic Incentive Mturk" incentiveleft "Order" incentiveshigh_incentiveleft "Probabilistic Incentive $\times$ Order") ///
order (seeincentivecostly seequalitycostly stdalpha selfishseeincentivecostly selfishseequalitycostly female age wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch ) ///
collabels(none)  ///
drop(professionalsfree wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch) ///
nomtitle label substitute(" 0.000 " " " " (.) " " ") ///
nonotes ///
prehead("`panel'") posthead("`dv'") postfoot("\hline" "\end{tabular}%" "\captionsetup{width=0.8\textwidth}" "\caption*{\footnotesize \textit{Notes:} This table displays the estimated coefficients from linear probability models on the preference to see the incentive first. See Incentive First Costly and Assess Quality First Costly are indicator variables that take value 1 in the respective treatment, 0 otherwise. Selfishness was elicited at the end of the experiment, using a multiple price list (MPL) with 5 decisions. The variable is a standardized measure of the number of times the advisor chose to recommend the incentivized product in the MPL task. The regression models in columns (2) and (3) include individual controls for the advisor's gender and age, each wave of the experiment, whether incentives were probabilistic, the position of the products on the screen and the interaction between these two variables. Robust standard errors (HC3) in parentheses.}" "\label{tab:demand_wave3_p}" "\end{table}")

test stdalpha + selfishseeincentivecostly  ==0
test stdalpha + selfishseequalitycostly  ==0


***************
* Posterior **
***************
gen signalred = (signalblue == 0)
gen incentiveA_signalred = incentiveA*signalred
gen choicebefore_signalred = choicebefore*signalred
corr incentiveA signalred

*Table 9
est clear
eststo: reg belief incentiveA signalred incentiveA_signalred $covariates2 if Highx10==0 & Highx100==0 & getbefore == 1, vce(hc3)
test incentiveA + incentiveA_signalred == 0
eststo: reg belief incentiveA signalred incentiveA_signalred choicebefore choicebefore_signalred $covariates2 if Highx10==0 & Highx100==0 & getbefore == 1, vce(hc3)
eststo: reg belief incentiveA signalred incentiveA_signalred $covariates2 if Highx10==0 & Highx100==0 & getbefore == 0, vce(hc3)
eststo: reg belief incentiveA signalred incentiveA_signalred choicebefore choicebefore_signalred $covariates2 if Highx10==0 & Highx100==0 & getbefore == 0, vce(hc3)


local panel "\begin{table}[h!]" "\centering" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\fontsize{9}{10}\selectfont" "\caption{Belief Updating: Correct Choice}" "\begin{tabular}{l*{4}{c}}\hline" 
local dv "&\multicolumn{4}{c}{\textbf{Belief}} \\"
local pref "\multicolumn{1}{r}{\textit{Assignment:}}&\multicolumn{2}{c}{Assigned to see incentive first}&\multicolumn{2}{c}{Assigned to see quality first}\\    "
local groups "\multicolumn{1}{r}{\textit{Grouping}} &\multicolumn{2}{c}{Assigned to }&\multicolumn{2}{c}{Assigned to }  \\ \multicolumn{1}{r}{\textit{of Data:}} &\multicolumn{2}{c}{See Incentive First}&\multicolumn{2}{c}{Assess Quality First}  \\\hline        &            &     &            &  \\"

esttab using  "${main}posterior.tex", se r2 replace cells(b(fmt(3)) se(par fmt(3))) ///
coeflabel (incentiveA "Incentive for A" signalred "Low Quality Signal" ///
incentiveA_signalred "Incentive for A $\times$ Low Quality Signal" professionalsfree "Choice Free--Professionals" seeincentivecostly "See Incentive First Costly" seequalitycostly "Assess Quality First Costly" wave2 "Wave 2" wave3 "Wave 3" notgetyourchoice "Not Assigned Preference" ///
female "Female" age "Age" selfish "Selfish" professionalscloudresearch "Professionals $\times$ Cloudresearch" selfishseeincentivecostly "Selfish $\times$ See Incentive First Costly" selfishseequalitycostly "Selfish $\times$ See Quality First Costly" ///
incentiveshigh "Probabilistic Incentive Mturk" incentiveleft "Order" incentiveshigh_incentiveleft "Probabilistic Incentive $\times$ Order" ///
choicebefore "Prefer to See Incentive First" ///
choicebefore_signalred "Prefer to See Incentive First $\times$ Low Quality Signal" ///
choicebeforenotgetyourchoice "Prefer to See Incentive First $\times$ Not Assigned Pref." ///
notgetyourchoicenoconflict "No Conflict $\times$ Not Assigned Preference") ///
order (incentiveA signalred incentiveA_signalred  seeincentivecostly seequalitycostly  professionalsfree female age wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch ) ///
title("Belief Updating: Posterior Belief") ///
drop(professionalsfree wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch) ///
mlabels(none) label collabels(none) substitute(" 0.000 " " " " (.) " " ") ///
nonotes ///
posthead("`dv'" "`groups'") prehead("`panel'") nolines postfoot("\hline" "\end{tabular}%" "\captionsetup{width=0.8\textwidth}" "\caption*{\footnotesize \textit{Notes:} This table displays the estimated coefficients from linear probability models on the advisor's beliefs that the quality of product B is low measured via their choice of one out of 10 possible belief bins (ranging from 0 to 100, in steps of 10). Column (1) and (2) focuses on individuals who are assigned to see the incentive first, while column (3) and (4) focuses on individuals who are not assigned to access the quality signal first. Incentive for A is an indicator of Product A being incentivized. Low Quality Signal is the indicator of the quality signal being a red ball (\\$0). Prefer to See Incentive First is an indicator of the advisor's preference. Choice Free-Professionals (omitted in the table), See Incentive First Costly and Assess Quality First Costly are indicator variables that take value 1 in the respective treatment, 0 otherwise. All regression models include controls for each wave of the experiment, whether incentives were probabilistic, the position of the products on the screen and the interaction between these two variables. Robust standard errors (HC3) in parentheses.}" "\label{tab:posteriorbelief}" "\end{table}")

esttab using  "${main}posterior_p.tex", se r2 replace cells(b(fmt(3)) se(par fmt(3)) p(par(`"["' `"]"'))) ///
coeflabel (incentiveA "Incentive for A" signalred "Low Quality Signal" ///
incentiveA_signalred "Incentive for A $\times$ Low Quality Signal" professionalsfree "Choice Free--Professionals" seeincentivecostly "See Incentive First Costly" seequalitycostly "Assess Quality First Costly" wave2 "Wave 2" wave3 "Wave 3" notgetyourchoice "Not Assigned Preference" ///
female "Female" age "Age" selfish "Selfish" professionalscloudresearch "Professionals $\times$ Cloudresearch" selfishseeincentivecostly "Selfish $\times$ See Incentive First Costly" selfishseequalitycostly "Selfish $\times$ See Quality First Costly" ///
incentiveshigh "Probabilistic Incentive Mturk" incentiveleft "Order" incentiveshigh_incentiveleft "Probabilistic Incentive $\times$ Order" ///
choicebefore "Prefer to See Incentive First" ///
choicebefore_signalred "Prefer to See Incentive First $\times$ Low Quality Signal" ///
choicebeforenotgetyourchoice "Prefer to See Incentive First $\times$ Not Assigned Pref." ///
notgetyourchoicenoconflict "No Conflict $\times$ Not Assigned Preference") ///
order (incentiveA signalred incentiveA_signalred  seeincentivecostly seequalitycostly  professionalsfree female age wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch ) ///
title("Belief Updating: Posterior Belief") ///
drop(professionalsfree wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch) ///
mlabels(none) label collabels(none) substitute(" 0.000 " " " " (.) " " ") ///
nonotes ///
posthead("`dv'" "`groups'") prehead("`panel'") nolines postfoot("\hline" "\end{tabular}%" "\captionsetup{width=0.8\textwidth}" "\caption*{\footnotesize \textit{Notes:} This table displays the estimated coefficients from linear probability models on the advisor's beliefs that the quality of product B is low measured via their choice of one out of 10 possible belief bins (ranging from 0 to 100, in steps of 10). Column (1) and (2) focuses on individuals who are assigned to see the incentive first, while column (3) and (4) focuses on individuals who are not assigned to access the quality signal first. Incentive for A is an indicator of Product A being incentivized. Low Quality Signal is the indicator of the quality signal being a red ball (\\$0). Prefer to See Incentive First is an indicator of the advisor's preference. Choice Free-Professionals (omitted in the table), See Incentive First Costly and Assess Quality First Costly are indicator variables that take value 1 in the respective treatment, 0 otherwise. All regression models include controls for each wave of the experiment, whether incentives were probabilistic, the position of the products on the screen and the interaction between these two variables. Robust standard errors (HC3) in parentheses.}" "\label{tab:posteriorbelief_p}" "\end{table}")

*Table 10
est clear
eststo: reg logitbelief incentiveA signalred incentiveA_signalred $covariates2 if Highx10==0 & Highx100==0 & getbefore == 1, vce(hc3)
eststo: reg logitbelief incentiveA signalred incentiveA_signalred choicebefore choicebefore_signalred $covariates2 if Highx10==0 & Highx100==0 & getbefore == 1, vce(hc3)
eststo: reg logitbelief incentiveA signalred incentiveA_signalred $covariates2 if Highx10==0 & Highx100==0 & getbefore == 0, vce(hc3)
eststo: reg logitbelief incentiveA signalred incentiveA_signalred choicebefore choicebefore_signalred $covariates2 if Highx10==0 & Highx100==0 & getbefore == 0, vce(hc3)


local panel "\begin{table}[h!]" "\centering" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\fontsize{9}{10}\selectfont" "\caption{Belief Updating: Correct Choice}" "\begin{tabular}{l*{4}{c}}\hline" 
local dv "&\multicolumn{4}{c}{\textbf{Posterior Belief}} \\"
local pref "\multicolumn{1}{r}{\textit{Assignment:}}&\multicolumn{2}{c}{Assigned to see incentive first}&\multicolumn{2}{c}{Assigned to see quality first}\\    "
local groups "\multicolumn{1}{r}{\textit{Data:}} &\multicolumn{2}{c}{Assigned to see incentive first}&\multicolumn{2}{c}{Assigned to see quality first}  \\\hline        &            &     &            &  \\"

esttab using "${main}posterior_logit.tex", se r2 replace cells(b(fmt(3)) se(par fmt(3))) ///
coeflabel (incentiveA "Incentive for A" signalred "Low Quality Signal" ///
incentiveA_signalred "Incentive for A $\times$ Low Quality Signal" professionalsfree "Choice Free--Professionals" seeincentivecostly "See Incentive First Costly" seequalitycostly "Assess Quality First Costly" wave2 "Wave 2" wave3 "Wave 3" notgetyourchoice "Not Assigned Preference" ///
female "Female" age "Age" selfish "Selfish" professionalscloudresearch "Professionals $\times$ Cloudresearch" selfishseeincentivecostly "Selfish $\times$ See Incentive First Costly" selfishseequalitycostly "Selfish $\times$ See Quality First Costly" ///
incentiveshigh "Probabilistic Incentive Mturk" incentiveleft "Order" incentiveshigh_incentiveleft "Probabilistic Incentive $\times$ Order" ///
choicebefore "Prefer to See Incentive First" ///
choicebefore_signalred "Prefer to See Incentive First $\times$ Low Quality Signal" ///
choicebeforenotgetyourchoice "Prefer to See Incentive First $\times$ Not Assigned Pref." ///
notgetyourchoicenoconflict "No Conflict $\times$ Not Assigned Preference") ///
order (incentiveA signalred incentiveA_signalred  seeincentivecostly seequalitycostly  professionalsfree female age wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch ) ///
title("Belief Updating: Posterior Belief") ///
drop(professionalsfree wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch) ///
mlabels(none) label collabels(none) substitute(" 0.000 " " " " (.) " " ") ///
nonotes ///
posthead("`dv'" "`groups'") prehead("`panel'") nolines postfoot("\hline" "\end{tabular}%" "\captionsetup{width=0.8\textwidth}" "\caption*{\footnotesize \textit{Notes:} This table displays the estimated coefficients from linear probability models on the advisor's beliefs that the quality of product B is low measured via their choice of one out of 10 possible belief bins (ranging from 0 to 100, in steps of 10). Column (1) and (2) focuses on individuals who are assigned to see the incentive first, while column (3) and (4) focuses on individuals who are not assigned to access the quality signal first. Incentive for A is an indicator of Product A being incentivized. Low Quality Signal is the indicator of the quality signal being a red ball (\\$0). Prefer to See Incentive First is an indicator of the advisor's preference. Choice Free-Professionals (omitted in the table), See Incentive First Costly and Assess Quality First Costly are indicator variables that take value 1 in the respective treatment, 0 otherwise. All regression models include controls for each wave of the experiment, whether incentives were probabilistic, the position of the products on the screen and the interaction between these two variables. Robust standard errors (HC3) in parentheses.}" "\label{tab:posteriorbelieflog}" "\end{table}")

esttab using "${main}posterior_logit_p.tex", se r2 replace cells(b(fmt(3)) se(par fmt(3)) p(par(`"["' `"]"'))) ///
coeflabel (incentiveA "Incentive for A" signalred "Low Quality Signal" ///
incentiveA_signalred "Incentive for A $\times$ Low Quality Signal" professionalsfree "Choice Free--Professionals" seeincentivecostly "See Incentive First Costly" seequalitycostly "Assess Quality First Costly" wave2 "Wave 2" wave3 "Wave 3" notgetyourchoice "Not Assigned Preference" ///
female "Female" age "Age" selfish "Selfish" professionalscloudresearch "Professionals $\times$ Cloudresearch" selfishseeincentivecostly "Selfish $\times$ See Incentive First Costly" selfishseequalitycostly "Selfish $\times$ See Quality First Costly" ///
incentiveshigh "Probabilistic Incentive Mturk" incentiveleft "Order" incentiveshigh_incentiveleft "Probabilistic Incentive $\times$ Order" ///
choicebefore "Prefer to See Incentive First" ///
choicebefore_signalred "Prefer to See Incentive First $\times$ Low Quality Signal" ///
choicebeforenotgetyourchoice "Prefer to See Incentive First $\times$ Not Assigned Pref." ///
notgetyourchoicenoconflict "No Conflict $\times$ Not Assigned Preference") ///
order (incentiveA signalred incentiveA_signalred  seeincentivecostly seequalitycostly  professionalsfree female age wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch ) ///
title("Belief Updating: Posterior Belief") ///
drop(professionalsfree wave2 wave3 incentiveshigh incentiveleft incentiveshigh_incentiveleft professionalscloudresearch) ///
mlabels(none) label collabels(none) substitute(" 0.000 " " " " (.) " " ") ///
nonotes ///
posthead("`dv'" "`groups'") prehead("`panel'") nolines postfoot("\hline" "\end{tabular}%" "\captionsetup{width=0.8\textwidth}" "\caption*{\footnotesize \textit{Notes:} This table displays the estimated coefficients from linear probability models on the advisor's beliefs that the quality of product B is low measured via their choice of one out of 10 possible belief bins (ranging from 0 to 100, in steps of 10). Column (1) and (2) focuses on individuals who are assigned to see the incentive first, while column (3) and (4) focuses on individuals who are not assigned to access the quality signal first. Incentive for A is an indicator of Product A being incentivized. Low Quality Signal is the indicator of the quality signal being a red ball (\\$0). Prefer to See Incentive First is an indicator of the advisor's preference. Choice Free-Professionals (omitted in the table), See Incentive First Costly and Assess Quality First Costly are indicator variables that take value 1 in the respective treatment, 0 otherwise. All regression models include controls for each wave of the experiment, whether incentives were probabilistic, the position of the products on the screen and the interaction between these two variables. Robust standard errors (HC3) in parentheses.}" "\label{tab:posteriorbelieflog_p}" "\end{table}")



******************************************************************************************************************************************
******************************************************************************************************************************************
** Figure 5
******************************************************************************************************************************************
******************************************************************************************************************************************

	
*STACKED FIGURE
bys choicebefore: egen blind=mean(avoid_incentiveinfo) if Highx10==0 & Highx100==0 & age!=. & female!=. & wave3==1
bys choicebefore: egen blindsd=sd(avoid_incentiveinfo) if Highx10==0 & Highx100==0 & age!=. & female!=. & wave3==1
bys choicebefore: egen blindn=count(avoid_incentiveinfo) if Highx10==0 & Highx100==0 & age!=. & female!=. & wave3==1

/*
summ avoid_incentiveinfo if Highx10==0 & Highx100==0, d
return list
*/

g loblind=blind-1.96*(blindsd/sqrt(blindn))
g hiblind=blind+1.96*(blindsd/sqrt(blindn))


tab blind

twoway (bar meancommit choiceafter, lcolor(black) fcolor(white) barwidth(0.3) lwidth(thin)) ///
		(bar blind choiceafter, lcolor(black) color(gs8) barwidth(0.3) lwidth(thin)) ///
		(rcap loblind hiblind choiceafter, color(gs12)) ///
		, graphr(c(white)) xlabel(0 "Prefer to See Incentive First" 1 "Prefer to Assess Quality First") ///
		xscale(r(-0.4 1.4)) legend(order(1 "Prefer Not to Blind Incentive Information" ///
		2 "Prefer Blinding Incentive Information") rows(2)) ylabel(0(0.2)1) ///
		ytitle("Advisor Preference to Blind" " ") ///
		xtitle(" " "Advisor Preference in Choice Experiment") ///
		text(0.2 0 "0.31") text(0.2 1 "0.54")  ///
		text(0.8 0 "0.69") text(0.8 1 "0.46")
graph export "${main}Choice_Blinding.png", replace

	 
******************************************************************************************************************************************
******************************************************************************************************************************************
** Figure 6
******************************************************************************************************************************************
******************************************************************************************************************************************

u "${data}choice_experiments.dta", clear
*As pre-registered, drop Mturk participant with inconsistent alpha value
drop if study!=1 & alphavaluefinal==. /*(study1 are profs and there's no MPL')*/
gen incentiveBgetbefore=incentiveB*getbefore

**** FIGURE ***** MAIN TEXT

cap drop predrecommend_* ubpredrecommend_* lbpredrecommend_* sepredrecommend_*
g predrecommend_dqf_gqf=.
g sepredrecommend_dqf_gqf=.
g predrecommend_dqf_gif=.
g sepredrecommend_dqf_gif=.
g predrecommend_dif_gqf=.
g sepredrecommend_dif_gqf=.
g predrecommend_dif_gif=.
g sepredrecommend_dif_gif=.

*demand incentive first 
est clear
eststo:reg recommendincentive i.treatment##i.getbefore incentiveB  $covariates2 ///
if choicebefore==1  ///
& conflict==1 & Highx10==0 & Highx100==0, vce(hc3) 
forval i=0(1)3{
margins i.getbefore if treatment==`i', atmeans saving("${main}adjusted_recommendations", replace)
matrix coeffs=r(table)
replace predrecommend_dif_gqf=coeffs[1,1] if treatment==`i' & choicebefore==1 & getbefore==0
replace sepredrecommend_dif_gqf=coeffs[2,1] if treatment==`i' & choicebefore==1 & getbefore==0
replace predrecommend_dif_gif=coeffs[1,2] if treatment==`i' & choicebefore==1 & getbefore==1
replace sepredrecommend_dif_gif=coeffs[2,2] if treatment==`i' & choicebefore==1 & getbefore==1
}	
g ubpredrecommend_dif_gif=predrecommend_dif_gif+1.96*sepredrecommend_dif_gif
g lbpredrecommend_dif_gif=predrecommend_dif_gif-1.96*sepredrecommend_dif_gif
g ubpredrecommend_dif_gqf=predrecommend_dif_gqf+1.96*sepredrecommend_dif_gqf
g lbpredrecommend_dif_gqf=predrecommend_dif_gqf-1.96*sepredrecommend_dif_gqf


*demand quality first 
est clear
eststo:reg recommendincentive i.treatment##i.getbefore incentiveB  $covariates2 ///
if choicebefore==0  ///
& conflict==1 & Highx10==0 & Highx100==0, vce(hc3)  
forval i=0(1)3{
margins i.getbefore if treatment==`i', atmeans saving("${main}adjusted_recommendations", replace)
matrix coeffs=r(table)
replace predrecommend_dqf_gqf=coeffs[1,1] if treatment==`i' & choicebefore==0 & getbefore==0
replace sepredrecommend_dqf_gqf=coeffs[2,1] if treatment==`i' & choicebefore==0 & getbefore==0
replace predrecommend_dqf_gif=coeffs[1,2] if treatment==`i' & choicebefore==0 & getbefore==1
replace sepredrecommend_dqf_gif=coeffs[2,2] if treatment==`i' & choicebefore==0 & getbefore==1
}	
g ubpredrecommend_dqf_gif=predrecommend_dqf_gif+1.96*sepredrecommend_dqf_gif 
g lbpredrecommend_dqf_gif=predrecommend_dqf_gif-1.96*sepredrecommend_dqf_gif
g ubpredrecommend_dqf_gqf=predrecommend_dqf_gqf+1.96*sepredrecommend_dqf_gqf
g lbpredrecommend_dqf_gqf=predrecommend_dqf_gqf-1.96*sepredrecommend_dqf_gqf


cap drop treatnumgraph2
g treatnumgraph2=2 if  choicebefore==1 & getbefore==1 & condition=="ChoiceFree" & professionals==0
replace treatnumgraph2=2.1 if choicebefore==1 & getbefore==0 & condition=="ChoiceFree" & professionals==0
replace treatnumgraph2=2.2 if  choicebefore==0 & getbefore==1 & condition=="ChoiceFree" & professionals==0
replace treatnumgraph2=2.3 if  choicebefore==0 & getbefore==0 & condition=="ChoiceFree" & professionals==0


replace treatnumgraph2=1 if  choicebefore==1 & getbefore==1 & condition=="ChoiceFree_Professionals" 
replace treatnumgraph2=1.1 if choicebefore==1 & getbefore==0 & condition=="ChoiceFree_Professionals" 
replace treatnumgraph2=1.2 if  choicebefore==0 & getbefore==1 & condition=="ChoiceFree_Professionals" 
replace treatnumgraph2=1.3 if  choicebefore==0 & getbefore==0 & condition=="ChoiceFree_Professionals" 

replace treatnumgraph2=4 if  choicebefore==1 & getbefore==1 & condition=="PayAfter"
replace treatnumgraph2=4.1 if  choicebefore==1 & getbefore==0 & condition=="PayAfter"
replace treatnumgraph2=4.2 if  choicebefore==0 & getbefore==1 & condition=="PayAfter"
replace treatnumgraph2=4.3 if  choicebefore==0 & getbefore==0 & condition=="PayAfter"

replace treatnumgraph2=3 if  choicebefore==1 & getbefore==1 & condition=="PayBefore"
replace treatnumgraph2=3.1 if  choicebefore==1 & getbefore==0 & condition=="PayBefore"
replace treatnumgraph2=3.2 if  choicebefore==0 & getbefore==1 & condition=="PayBefore"
replace treatnumgraph2=3.3 if  choicebefore==0 & getbefore==0 & condition=="PayBefore"

*Analytical Code for Figure 6	
*Conflict panel for long version of figure 		
twoway 	(scatteri 1 0.7 1 1.7, bcolor(gs15) recast(area)) ///
		(scatteri 1 2.7 1 3.7, bcolor(gs15) recast(area)) ///
		(rcap lbpredrecommend_dif_gif ubpredrecommend_dif_gif treatnumgraph2, lcolor(red) lwidth(thin)) ///
		(rcap lbpredrecommend_dqf_gqf ubpredrecommend_dqf_gqf treatnumgraph2, lcolor(black) lwidth(thin)) ///
		(rcap lbpredrecommend_dif_gqf ubpredrecommend_dif_gqf treatnumgraph2, lcolor(red*0.3) lwidth(thin)) ///
		(rcap lbpredrecommend_dqf_gif ubpredrecommend_dqf_gif treatnumgraph2, lcolor(black*0.3) lwidth(thin)) ///
		(scatter predrecommend_dif_gif treatnumgraph2 if ///
		condition=="ChoiceFree_Professionals" & professionals==1 , mcolor(red) msize(*0.75) ms(T)) ///
		(scatter predrecommend_dif_gqf treatnumgraph2 if ///
		condition=="ChoiceFree_Professionals" & professionals==1 & choicebefore==1 & getbefore==0, mfcolor(white) msize(*0.8) mlcolor(red*0.5) ms(S)) ///
		(scatter predrecommend_dqf_gif treatnumgraph2 if ///
		condition=="ChoiceFree_Professionals" & professionals==1 & choicebefore==0 & getbefore==1,  mfcolor(white) mlcolor(black*0.5) msize(*0.8) ms(T)) ///
		(scatter predrecommend_dqf_gqf treatnumgraph2 if ///
		condition=="ChoiceFree_Professionals" & professionals==1 & choicebefore==0 & getbefore==0, mcolor(black) msize(*0.8) ms(S)) ///
		(scatter predrecommend_dif_gif treatnumgraph2 if ///
		condition=="ChoiceFree" , mcolor(red) msize(*0.75) ms(T)) ///
		(scatter predrecommend_dif_gqf treatnumgraph2 if ///
		condition=="ChoiceFree" , mfcolor(white) msize(*0.8) mlcolor(red*0.5) ms(S)) ///
		(scatter predrecommend_dqf_gif treatnumgraph2 if ///
		condition=="ChoiceFree", mfcolor(white)  mlcolor(black*0.3) msize(*0.8) ms(T)) ///
		(scatter predrecommend_dqf_gqf treatnumgraph2 if ///
		condition=="ChoiceFree", msize(*0.8) mcolor(black) ms(S)) ///
		(scatter predrecommend_dif_gif treatnumgraph2 if condition=="PayAfter", mcolor(red) msize(*0.75) ms(T)) ///
		(scatter predrecommend_dif_gqf treatnumgraph2 if condition=="PayAfter", mfcolor(white) msize(*0.8) mlcolor(red*0.5)  ms(S)) ///
		(scatter predrecommend_dqf_gif treatnumgraph2 if condition=="PayAfter", mfcolor(white) mlcolor(black*0.5) msize(*0.8) ms(T)) ///
		(scatter predrecommend_dqf_gqf treatnumgraph2 if condition=="PayAfter", mcolor(black)  msize(*0.8)  ms(S)) ///
		(scatter predrecommend_dif_gif treatnumgraph2 if condition=="PayBefore", mcolor(red) msize(*0.75) ms(T)) ///
		(scatter predrecommend_dif_gqf treatnumgraph2 if condition=="PayBefore", mfcolor(white) msize(*0.8) mlcolor(red*0.5)  ms(S)) ///
		(scatter predrecommend_dqf_gif treatnumgraph2 if condition=="PayBefore", mfcolor(white) mlcolor(black*0.5) msize(*0.8) ms(T)) ///
		(scatter predrecommend_dqf_gqf treatnumgraph2 if condition=="PayBefore", msize(*0.8) mcolor(black) ms(S)) ///
		, graphr(c(white)) plotr(c(white)) ///
		ylabel(0.3(0.1)1) yscale(r(0.3 1)) 	///
		xtitle(" ") ///
		xlabel(none) ///                  
		xscale(r(0.7 4.6)) ///
		legend(order( - "{bf:Advisor Prefers to}" "{bf:See Incentive First}" ///
				7 8 - "{bf:Advisor Prefers to}" "{bf:Assess Quality First}" 9 10) ///
		lab(7 " Assigned to See Incentive First") ///
		lab(8 " Assigned to Assess Quality First") ///
		lab(10 " Assigned to Assess Quality First") ///
		lab(9 " Assigned to See Incentive First") ///
		rows(3) colfirst size(*0.7)) ///
		text(0.38 1.15 "{bf: Choice Free}", color(black) size(*0.8)) ///
		text(0.35 1.15 "{bf: - Professionals}", color(black) size(*0.8)) ///		
		text(0.38 2.15 "{bf: Choice}", color(black) size(*0.8)) ///
		 text(0.35 2.15 "{bf: Free }", color(black) size(*0.8)) ///
		text(0.38 4.15 "{bf: Quality First}", color(black) size(*0.8)) ///
		text(0.35 4.15 "{bf: Costly}", color(black) size(*0.8)) ///
		text(0.38 3.15 "{bf: Incentive First}", color(black) size(*0.8)) ///
		text(0.35 3.15 "{bf: Costly}", color(black) size(*0.8)) ///
		ytitle("{bf:Incentivized product recommendation}" " " ) 
		
		graph export "${main}Choice_recommendations_conflict_pred_correct.pdf", replace
