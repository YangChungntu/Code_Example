/*-------------------------------*/	
*NO CHOICE 
/*-------------------------------*/	
set more off
clear
import delimited "${raw}NoChoice_deidentified.csv", varnames(1)
drop if status==1
tab distributionchannel
drop if distributionchannel=="preview"
tab finished
keep if finished==1

sort newid
quietly by newid:  gen dup = cond(_N==1,0,_n)
tab dup
drop if dup!=0

rename q32 attention

g id=_n
g attentionpassed=attention==3
keep if attentionpassed==1


*Treatments
g treatment=1 if condition=="BeforeA"
replace treatment=2 if condition=="AfterA"
replace treatment=3 if condition=="BeforeB"
replace treatment=4 if condition=="AfterB"

label def treat 1 "Before A" 2 "After A" 3 "Before B" 4 "After B"
label val treatment treat 



*Beliefs
g beliefbin=q45 
g beliefnum=q43

*Recommend b
g recommendb=(recommendationa==2 | recommendationb==2)

*Signal
g signalblue=(fl_211_do=="Signal_blue2" | fl_221_do=="Signal_blue2" | fl_229_do=="Signal_blue2" | fl_127_do=="Signal_blue2")

*Conflict
g conflict=1 if treatment<3 & signalblue==1
replace conflict=1 if treatment>2 & signalblue==0
replace conflict=0 if conflict==.

*Commission
g incentiveA=(condition=="AfterA" | condition=="BeforeA")
g incentiveB=1-incentiveA

*Recommend incentivized
g recommendincentivized=recommendb if treatment>2
replace recommendincentivized=1-recommendb if treatment<3

*Before-After
g before=(treatment==1 | treatment==3)
g after=1-before

*Prosociality 		
forval i=2(1)5{
recode q8`i' (1=0) (2=1)
}

rename q82 alpha1
rename q83 alpha2
rename q84 alpha3
rename q85 alpha4
rename q176 alpha5

recode alpha5 (1=0) (2=1)

egen alphamiss=rowmiss(alpha1 alpha2 alpha3 alpha4) 
tab alphamiss

*alpha1-5 equal 1 is recommend the incentivized option 

*RESTRICTING RESPONDENTS TO THOSE WITH COMPLETE ALPHA CHOICES
keep if alphamiss==0

*Prosociality switch 
reshape long alpha, i(id) j(number)
	
sort id number		
g previouschoice=alpha[_n-1] if number>1		
g steptob=(alpha-previouschoice) if number>1
bys id: egen sumsteptob=total(steptob) 

*Alpha value final = number of times you go with the incentive to recommend Y
g alphavalue=0 if sumsteptob==0 & alpha==0 & number==5 /*always choosing X, no incentive*/
replace alphavalue=5 if sumsteptob==0 & alpha==1 & number==5 /*always choosing Y, incentive*/
*Alpha value is the question number when the steptob is 1 for those who switch
replace alphavalue=5-(number-1) if sumsteptob==1 & steptob==1 

g inconsistentchoice=(steptob==-1)
bys id: egen inconsistentid=total(inconsistentchoice) 

bys id: egen alphavaluefinal=total(alphavalue) 
replace alphavaluefinal=. if inconsistentid>0

tab alphavaluefinal, m
cap drop alphavalue inconsistentchoice previouschoice steptob
reshape wide alpha, i(id) j(number)

*Average alpha
tab alphavaluefinal if treatment!=., m

rename recommendincentivized recommendincentive
g female=(gender==2)
replace age="31" if age=="3i"
destring age, replace
recode englishnative (2=0)

g nodifficulty=(difficulty==2)
replace nodifficulty=. if difficulty==.

label var female "Female"
label var age "Age"

gen seeincentivefirst=(condition=="BeforeA" | condition=="BeforeB")

gen selfish=0
replace selfish=1 if alphavaluefinal>3
replace selfish=. if alphavaluefinal==.
gen missingalpha=0
replace missingalpha=1 if alphavaluefinal==.

gen noconflict=1-conflict
g seeincentivefirst_noconflict=seeincentivefirst*noconflict 	


g recommendincentive_before=recommendincentive if seeincentivefirst==1
g recommendincentive_after=recommendincentive if seeincentivefirst==0	


egen stdalpha=std(alphavaluefinal)


g choice="No Choice"


save "${data}nochoice.dta", replace		



/*------------------------------*/
*CHOICE EXPERIMENT
/*------------------------------*/
*** CLEANING DEIDENTIFIED DATA
clear
import delimited "${raw}Choice1_deidentified.csv", varnames(1)

drop if status==1
*drop if workerid==""
tab distributionchannel
drop if distributionchannel=="preview"
tab finished
keep if finished==1


sort newid
quietly by newid:  gen dup = cond(_N==1,0,_n)
tab dup
drop if dup!=0 


rename q32 attention

g id=_n+10000
g attentionpassed=attention==3
keep if attentionpassed==1

***gen IncentiveB
g incentiveB=.
replace incentiveB=1 if condition=="choiceFreeB"
replace incentiveB=1 if condition=="choicePayB"
replace incentiveB=0 if condition=="choiceFreeA"
replace incentiveB=0 if condition=="choicePayA"

g incentiveA=.
replace incentiveA=1 if condition=="choiceFreeA"
replace incentiveA=1 if condition=="choicePayA"
replace incentiveA=0 if condition=="choiceFreeB"
replace incentiveA=0 if condition=="choicePayB"

**Commission
gen commission="commissionB" if incentiveB==1
replace commission="commissionA" if incentiveB==0

*Choice before
g choicebefore=(choicefreeq==1 | choicepay==1)

*Recommendation 
g recommendb=(recommendationa==2 | recommendationb==2)
replace recommendb=. if recommendationa==. & recommendationb==.
g recommenda=1-recommendb	

g recommendincentive=recommendb if commission=="commissionB"
replace recommendincentive=recommenda if commission=="commissionA"	


*Beliefs
g beliefbin=q45 
g belief=q43 
replace belief=q43*100 if q43<1

*Signal
g signalblue=(fl_298_do=="Signal_blue2" ///
		| fl_304_do=="Signal_blue2" ///
		| fl_211_do=="Signal_blue2" ///
		| fl_221_do=="Signal_blue2" ///
		| fl_310_do=="Signal_blue2" ///
		| fl_229_do=="Signal_blue2" ///
		| fl_325_do=="Signal_blue2" ///
		| fl_127_do=="Signal_blue2")	
	
			
*Conflict	
g conflict=0
replace conflict=1 if signalblue==1 & commission=="commissionA"
replace conflict=1 if signalblue==0 & commission=="commissionB"


*Treatment 
g treatment="ChoiceFree" if condition=="choiceFreeA" | condition=="choiceFreeB"
replace treatment="ChoicePay" if condition=="choicePayA" | condition=="choicePayB"


*Random assignment to order
g get=fl_264_do if condition=="choiceFreeA" & choicefreeq==2
replace get=fl_265_do if condition=="choiceFreeA" & choicefreeq==1

replace get=fl_289_do if condition=="choiceFreeB" & choicefreeq==1
replace get=fl_287_do if condition=="choiceFreeB" & choicefreeq==2

replace get=fl_342_do if condition=="choicePayA" & choicepayq==2
replace get=fl_272_do if condition=="choicePayA" & choicepayq==1

replace get=fl_320_do if condition=="choicePayB" & choicepayq==1
replace get=fl_331_do if condition=="choicePayB" & choicepayq==2

g getafter=(get=="GetAfter") 
g getbefore=1-getafter

g costlydemand=(condition=="choicePayA" | condition=="choicePayB") 
g choicewave=1

g study=2 if costlydemand==0
replace study=3 if costlydemand==1

replace age="51" if age=="51`"
replace age="" if age=="re"
destring age, replace

*Prosociality
destring q82 q83 q84 q85 q176, force replace
 		
forval i=2(1)5{
recode q8`i' (1=0) (2=1)
}

rename q82 alpha1
rename q83 alpha2
rename q84 alpha3
rename q85 alpha4
rename q176 alpha5
recode alpha5 (1=0) (2=1)
egen alphamiss=rowmiss(alpha1 alpha2 alpha3 alpha4 alpha5) 
keep if alphamiss==0

g choice="Choice"


drop fl_17_do fl_264_do fl_298_do fl_265_do fl_211_do fl_272_do fl_304_do fl_342_do ///
*fl_289_do fl_221_do fl_310_do fl_287_do fl_229_do fl_320_do fl_320_do fl_325_do fl_331_do fl_127_do
	
 
save "${data}choice1.dta", replace

/*-------------------------------*/	
*PROFESSIONALS STUDY
/*-------------------------------*/	

*** CLEAN DE_IDENTIFIED DATA
**PROLIFIC
clear
import delimited "${raw}ProfessionalsProlific_deidentified.csv", varnames(1)

drop if status==1
drop if distributionchannel=="preview"
keep if finished==1
sort newid
quietly by newid:  gen dup = cond(_N==1,0,_n)
drop if dup>0


g sample="Prolific"



save "${data}professionals_prolific", replace

*CLOUDRESEARCH
clear
import delimited "${raw}ProfessionalsCloudresearch_deidentified.csv", varnames(1)

g sample="CloudResearch"
drop if status==1
drop if distributionchannel=="preview"
keep if finished==1

sort newid
quietly by newid:  gen dup = cond(_N==1,0,_n)
tab dup
drop if dup!=0


drop fl_17_do fl_481_do fl_523_do fl_528_do fl_548_do fl_555_do fl_563_do fl_657_do fl_629_do fl_656_do fl_642_do
 
rename industry industry_cloudresearch
append using "${data}professionals_prolific.dta", force

	
g attentionpassed=attention==3 
drop if attentionpassed==0


*Choice
g choicebefore=(choicefree==1)
replace choicebefore=. if choicefree==.
drop if choicebefore==.

*Beliefs
g beliefbin=q45 
g beliefnum=q43
g belief=q43 
replace belief=q43*100 if q43<1

*Recommendation 
g recommendb=(recommendationa==2 | recommendationb==2)
replace recommendb=. if recommendationa==. & recommendationb==.
g recommenda=1-recommendb	
g recommendincentive=recommenda if condition=="choicefreeA"
replace recommendincentive=recommendb if condition=="choicefreeB"
		
*Get before		
g getafter=(getbefore=="GetAfter") 
g getbefore2=(getbefore=="GetBefore")
drop getbefore
rename getbefore2 getbefore

*Signal
g signalblue=signal=="Blue"

*Conflict
g conflict=0
replace conflict=1 if signalblue==1 & (condition=="choicefreeA")	
replace conflict=1 if signalblue==0 & (condition=="choicefreeB")	 

g incentiveA=(commission=="commissionA")
g incentiveB=1-incentiveA
g costlydemand=0	
	
g female=(gender==2)
	
g choice="Professionals"

g beliefcorrect=0
replace beliefcorrect=1 if beliefbin==4 & signalblue==1 /*signal $2, quality low with 25% chance*/
replace beliefcorrect=1 if beliefbin==8 & signalblue==0 /*signal $0, quality low with 66.7% chance*/

g explanationchoice=q211

destring age, replace

g id=_n+20000



keep recommendincentive getbefore incentiveB incentiveA ///
costlydemand choicebefore belief conflict signalblue ///
female age  sample beliefbin choice beliefcorrect ///
 newid explanationchoice 	jobtitle industry shortid currentcountr
 
g study=1 

save "${data}professionals.dta", replace

/*-------------------------------*/	
*PROFESSIONALS - JOB TITLES
/*-------------------------------*/	


clear
import excel "${raw}Professionals_jobtitles.xlsx", first case(l)
save "${data}professionals_jobtitles.dta", replace

/*-------------------------------*/	
*CHOICE 2 - REVERSED
/*-------------------------------*/	
*** CLEANING DEIDENTIFIED DATA
clear
import delimited "${raw}Choice2_deidentified.csv", varnames(1)

drop if status==1

tab distributionchannel
drop if distributionchannel=="preview"
tab finished
keep if finished==1

sort newid
quietly by newid:  gen dup = cond(_N==1,0,_n)
tab dup
drop if dup>0



drop if  q_recaptchascore<.50


g id=_n+30000

g attention=attention_a
replace attention=attention_b if attention_b!=.
replace attention=att_highro if att_highro!=.
replace attention=att_highflipped if att_highflipped!=.
drop attention_a attention_b att_highro att_highflipped

g attentionpassed=attention==3 
tab attentionpassed 
drop if attentionpassed==0
rename q211 explanationchoice
replace explanationchoice=q188 if explanationchoice==""
drop q188



*Choice before
g choicebefore=.
replace choicebefore=1 if choicefreereghigh==1
replace choicebefore=0 if choicefreereghigh==2

*Beliefs
g beliefbin=q45 
*g beliefnum=q43
g belief=q43 if incentives=="$0.15 "
replace belief=q551 if incentives=="$15 "
replace beliefbin=q550 if incentives=="$15 "
replace belief=q43*100 if q43<1 

*Recommendation 
g recommendb=.
replace recommendb=1 if  recommendationb==2
replace recommendb=0 if  recommendationb==1
replace recommendb=1 if  recommendationb_flip==2
replace recommendb=0 if  recommendationb_flip==1

replace recommendb=1 if  recommendationa==2
replace recommendb=0 if  recommendationa==1
replace recommendb=1 if  recommendationa_flip==2
replace recommendb=0 if  recommendationa_flip==1

g recommenda=1-recommendb	

g recommendincentive=recommendb if commission=="commissionB"
replace recommendincentive=recommenda if commission=="commissionA"

*Signal
g signalblue=.
*replace signalblue=1 if signal=="Blue"
replace signalblue=1 if fl_272_do=="Signal_blue2"
replace signalblue=1 if fl_459_do=="Signal_blue2HIGHINCENTIVES"
replace signalblue=1 if fl_260_do=="Signal_blue2"
replace signalblue=1 if fl_450_do=="Signal_blue2HIGHINCENTIVES"
replace signalblue=1 if fl_248_do=="Signal_blue2REVERSEPRODUCTORDER"
replace signalblue=1 if fl_508_do=="Signal_blue2REVERSEPRODUCTORDER-HIGHINCENTIVES"
replace signalblue=1 if fl_236_do=="Signal_blue2REVERSEPRODUCTORDER"
replace signalblue=1 if fl_469_do=="Signal_blue2REVERSEPRODUCTORDER-HIGHINCENTIVES"
replace signalblue=1 if fl_163_do=="Signal_blue2"
replace signalblue=1 if fl_476_do=="Signal_blue2HIGHINCENTIVES"
replace signalblue=1 if fl_151_do=="Signal_blue2"
replace signalblue=1 if fl_483_do=="Signal_blue2HIGHINCENTIVES"
replace signalblue=1 if fl_139_do=="Signal_blue2REVERSEPRODUCTORDER"
replace signalblue=1 if fl_491_do=="Signal_blue2REVERSEPRODUCTORDER-HIGHINCENTIVES"
replace signalblue=1 if fl_127_do=="Signal_blue2REVERSEPRODUCTORDER"
replace signalblue=1 if fl_499_do=="Signal_blue2REVERSEPRODUCTORDER-HIGHINCENTIVES"
replace signalblue=1 if signal=="Blue"



replace signalblue=0 if fl_272_do=="Signal_red0"
replace signalblue=0 if fl_459_do=="Signal_red0HIGHINCENTIVES"
replace signalblue=0 if fl_260_do=="Signal_red0"
replace signalblue=0 if fl_450_do=="Signal_red0HIGHINCENTIVES"
replace signalblue=0 if fl_248_do=="Signal_redREVERSEPRODUCTORDER"
replace signalblue=0 if fl_508_do=="Signal_redREVERSEPRODUCTORDER-HIGHINCENTIVES"
replace signalblue=0 if fl_236_do=="Signal_redREVERSEPRODUCTORDER"
replace signalblue=0 if fl_469_do=="Signal_redREVERSEPRODUCTORDER-HIGHINCENTIVES"
replace signalblue=0 if fl_163_do=="Signal_red0"
replace signalblue=0 if fl_476_do=="Signal_red0HIGHINCENTIVES"
replace signalblue=0 if fl_151_do=="Signal_red0"
replace signalblue=0 if fl_483_do=="Signal_red0HIGHINCENTIVES"
replace signalblue=0 if fl_139_do=="Signal_redREVERSEPRODUCTORDER"
replace signalblue=0 if fl_491_do=="Signal_redREVERSEPRODUCTORDER-HIGHINCENTIVES"
replace signalblue=0 if fl_127_do=="Signal_redREVERSEPRODUCTORDER"
replace signalblue=0 if fl_499_do=="Signal_redREVERSEPRODUCTORDER-HIGHINCENTIVES"
replace signalblue=0 if signal=="Red"


*Conflict
g conflict=.
replace conflict=1 if signalblue==0 & commission=="commissionB"
replace conflict=0 if signalblue==1 & commission=="commissionB"
replace conflict=1 if signalblue==1 & commission=="commissionA"
replace conflict=0 if signalblue==0 & commission=="commissionA"

g incentiveshigh=(incentives=="$15 ")
g flippedorder=(uncertainproduct=="Product A")
g costlydemand=0	
g incentiveA=(commission=="commissionA")
g incentiveB=1-incentiveA

gen incentiveleft=.
replace incentiveleft=1 if incentiveB==1 & flippedorder==1
replace incentiveleft=0 if incentiveB==1 & flippedorder==0
replace incentiveleft=1 if incentiveA==1 & flippedorder==0
replace incentiveleft=0 if incentiveA==1 & flippedorder==1

tab condition
replace condition="ChoiceFree" 

g choicewave=2
g getafter=(getbefore=="GetAfter") 
cap drop getbefore
g getbefore=1-getafter
g study=4 


*Prosociality


forval i=2(1)5{
recode q8`i' (1=0) (2=1)
}
forval i=4(1)8{
recode q54`i' (1=0) (2=1)
}

g alpha1=q82 if incentiveshigh==0
replace alpha1=q544 if incentiveshigh==1
g alpha2=q83 if incentiveshigh==0
replace alpha2=q545 if incentiveshigh==1
g alpha3=q84 if incentiveshigh==0
replace alpha3=q546 if incentiveshigh==1
g alpha4=q85 if incentiveshigh==0
replace alpha4=q547 if incentiveshigh==1

g alpha5=v193 if incentiveshigh==0
recode alpha5 (1=0) (2=1)
replace alpha5=q548 if incentiveshigh==1

egen alphamiss=rowmiss(alpha1 alpha2 alpha3 alpha4 alpha5) 
keep if alphamiss==0
g choice="Choice"


drop fl_101_do fl_226_do fl_341_do fl_314_do fl_411_do fl_271_do fl_272_do fl_459_do ///
fl_259_do fl_260_do fl_450_do fl_247_do fl_248_do fl_508_do fl_235_do fl_236_do fl_469_do /// 
fl_117_do fl_365_do fl_205_do fl_413_do fl_162_do fl_163_do fl_476_do fl_150_do fl_151_do fl_483_do ///
fl_138_do fl_139_do fl_491_do fl_126_do fl_127_do fl_499_do 

save "${data}choice2.dta", replace


/*-------------------------------*/	
*CHOICE 3 - wave3
/*-------------------------------*/	


*** CLEANING DEIDENTIFIED DATA
clear
import delimited "${raw}Choice3_deidentified.csv", varnames(1)

drop if status==1
*drop if workerid==""
tab distributionchannel
drop if distributionchannel=="preview"
tab finished
keep if finished==1

tab attention
replace attention=q343 if condition=="Highx10" | condition=="Highx100"
keep if attention!=.
gen attentionpassed=(attention==3)

tab attentionpassed
**94.86% passed the attention check (N=2095) out of 2202 who completed the attention check question
drop if attentionpassed!=1


sort newid
quietly by newid:  gen dup = cond(_N==1,0,_n)
tab dup
drop if dup!=0

drop if  q_recaptchascore<.50
tab status


sort startdate
g id=_n+40000


**Beliefs
replace beliefbin=beliefbin_high if beliefbin==.	
drop beliefbin_high
replace belief=belief_high if belief_high!=.
drop belief_high
replace belief=belief*100 if belief<1


replace age="" if age=="casian"
destring age, replace

***Gen recommendB and recommenda dummies

g recommendb=.
replace recommendb=1 if  recommendationb==2
replace recommendb=0 if  recommendationb==1

replace recommendb=1 if  recommendationa==2
replace recommendb=0 if  recommendationa==1


g recommenda=1-recommendb	

***gen IncentiveB
g incentiveB=.
replace incentiveB=1 if commission=="commissionB"
replace incentiveB=0 if commission=="commissionA"

g incentiveA=.
replace incentiveA=1 if commission=="commissionA"
replace incentiveA=0 if commission=="commissionB"


*GET by treatment
g getafter=(getbefore=="GetAfter") 
cap drop getbefore
g getbefore=1-getafter


**gen bluesignal dummy
g signalblue=.
replace signalblue=1 if signal=="Blue"
replace signalblue=1 if fl_750_do=="SignalBlue_HighIncentives"
replace signalblue=1 if fl_773_do=="SignalBlue_HighIncentives"
replace signalblue=1 if fl_785_do=="SignalBlue_HighIncentives"
replace signalblue=1 if fl_528_do=="Signal_blue2"
replace signalblue=1 if fl_555_do=="Signal_blue2"
replace signalblue=1 if fl_555_do=="Signal_blue2"
replace signalblue=1 if fl_629_do=="Signal_blue2"
replace signalblue=1 if fl_642_do=="Signal_blue2"
replace signalblue=0 if signal=="Red"
replace signalblue=0 if fl_750_do=="SignalRed_highIncentives"
replace signalblue=0 if fl_773_do=="SignalRed_highIncentives"
replace signalblue=0 if fl_785_do=="SignalRed_highIncentives"
replace signalblue=0 if fl_528_do=="Signal_red0"
replace signalblue=0 if fl_555_do=="Signal_red0"
replace signalblue=0 if fl_555_do=="Signal_red0"
replace signalblue=0 if fl_629_do=="Signal_red0"
replace signalblue=0 if fl_642_do=="Signal_red0"

g conflict=.
replace conflict=1 if signalblue==0 & commission=="commissionB"
replace conflict=0 if signalblue==1 & commission=="commissionB"
replace conflict=1 if signalblue==1 & commission=="commissionA"
replace conflict=0 if signalblue==0 & commission=="commissionA"
	

tab commission 
g recommendincentive=recommendb if commission=="commissionB"
replace recommendincentive=recommenda if commission=="commissionA"	


gen choicebefore=.
replace choicebefore=1 if choicefree==1
replace choicebefore=0 if choicefree==2
replace choicebefore=1 if choicepaybefore==1
replace choicebefore=0 if choicepaybefore==2
replace choicebefore=0 if choicepayafter==2
replace choicebefore=1 if choicepayafter==1



*** DUMMIES FOR STAKE TREATMENTS

gen HighStakes=""
replace HighStakes=condition if condition=="Highx10"
replace HighStakes=condition if condition=="Highx100"

gen Highx10=0
replace Highx10=1 if  condition=="Highx10"
gen  Highx100=0
replace Highx100=1 if  condition=="Highx100"

replace condition="ChoiceFree" if Highx10==1
replace condition="ChoiceFree" if Highx100==1


**Info avoidance
tab ia_choice
gen chooseblindness=ia_choice
replace chooseblindness=0 if ia_choice==2
tab chooseblindness

gen ia_recommend1=.
replace ia_recommend1=1 if ia_rec_quality0==1 | ia_recq2==1 | ia_rec_both2==1 | ia_rec_both0==1
replace ia_recommend1=0 if ia_rec_quality0==2 | ia_recq2==2 | ia_rec_both2==2 | ia_rec_both0==2

gen ia_recommendincentive=1 if ia_recommend1==1 & ia_incentive=="Product 1"
replace ia_recommendincentive=0 if ia_recommend1==0 & ia_incentive=="Product 1"
replace ia_recommendincentive=1 if ia_recommend1==0 & ia_incentive=="Product 2"
replace ia_recommendincentive=0 if ia_recommend1==1 & ia_incentive=="Product 2"




***Prosociality
forval i=1(1)5{
recode alpha`i' (1=0) (2=1)
}	

egen alphamiss=rowmiss(alpha1 alpha2 alpha3 alpha4) 
keep if alphamiss==0

gen study=6
rename q211 explanationchoice
gen costlydemand=0
replace costlydemand=1 if condition=="PayBefore"


g sample="MTurk"
	


drop fl_728_do fl_816_do fl_737_do fl_759_do fl_750_do fl_761_do fl_773_do fl_778_do ///
 fl_785_do fl_793_do fl_800_do fl_825_do fl_837_do fl_903_do fl_843_do fl_870_do fl_850_do ///
fl_880_do fl_855_do fl_889_do fl_17_do fl_481_do fl_523_do fl_528_do fl_548_do fl_555_do ///
 fl_563_do fl_657_do fl_629_do fl_656_do fl_642_do fl_933_do fl_916_do fl_935_do

g choice="Choice"
save "${data}choice3.dta", replace

	
	
/*-------------------------------*/	
*POOLING ALL CHOICE WAVES INTO 1 DATASET
/*-------------------------------*/	
		

	u "${data}choice1.dta", clear
	append using "${data}choice2.dta" 
	append using "${data}choice3.dta"

	*Prosociality - switch point 
	reshape long alpha, i(id) j(number)
			
	sort id number		
	g previouschoice=alpha[_n-1] if number>1		
	g steptob=(alpha-previouschoice) if number>1
	bys id: egen sumsteptob=total(steptob) 

	*Alpha value final = number of times you go with the incentive to recommend Y
	g alphavalue=0 if sumsteptob==0 & alpha==0 & number==5 /*always choosing X, no incentive*/
	replace alphavalue=5 if sumsteptob==0 & alpha==1 & number==5 /*always choosing Y, incentive*/
	*Alpha value is the question number when the steptob is 1 for those who switch
	replace alphavalue=5-(number-1) if sumsteptob==1 & steptob==1 

	g inconsistentchoice=(steptob==-1)
	bys id: egen inconsistentid=total(inconsistentchoice) 

	bys id: egen alphavaluefinal=total(alphavalue) 
	replace alphavaluefinal=. if inconsistentid>0	

	cap drop alphavalue inconsistentchoice previouschoice steptob

	reshape wide alpha, i(id) j(number)


	*ATTENTIVE GUYS are DROPPED FOR INCOMPLETE ANSWERS
	*KEEP CONSISTENT GUYS IN ALPHA TASK		
	tab alphavaluefinal, m
	egen stdalpha=std(alphavaluefinal)

	gen selfish=(alphavaluefinal>3)
	replace selfish=. if alphavaluefinal==.
		
	*tab attention0, m
	*keep if attention0==1
			
	destring gender age englishnative difficulty, force replace 
	g female=(gender==2)
	recode englishnative (2=0)
	g nodifficulty=(difficulty==2)
	replace nodifficulty=. if difficulty==.

	label var female "Female"
	label var age "Age"

	g beliefcorrect=0
	replace beliefcorrect=1 if beliefbin==4 & signalblue==1
	replace beliefcorrect=1 if beliefbin==8 & signalblue==0

	replace sample="MTurk"
	
	replace incentiveleft=0 if study!=4
	replace incentiveshigh=0 if study!=4
	
	drop if choicebefore==.
	tab condition, m
	replace condition="ChoiceFree" if condition=="choiceFreeA"
	replace condition="ChoiceFree" if condition=="choiceFreeB"
	replace condition="PayBefore" if condition=="choicePayA"
	replace condition="PayBefore" if condition=="choicePayB"

		
keep id startdate enddate attentionpassed recommendincentive getbefore incentiveB incentiveA commission costlydemand ///
choicebefore belief beliefbin conflict signalblue female age selfish stdalpha ///
alphavaluefinal beliefcorrect newid shortid ///
sample choice incentiveshigh flippedorder /// 
incentiveleft condition study  Highx10 Highx100 chooseblindness 

append using "${data}professionals.dta"

rename costlydemand incentivecostly
	g qualitycostly=0
	replace qualitycostly=1 if condition=="PayAfter"

g professionals=(study==1)
g prolific=(sample=="Prolific")
g cloudresearch=(sample=="CloudResearch")
gen professionalscloudresearch=0 if professionals==1 & cloudresearch==1
replace professionalscloudresearch=1 if professionals==1 & cloudresearch==1
replace incentiveshigh=0 if incentiveshigh==.
replace incentiveleft=0 if incentiveleft==.
replace Highx10=0 if Highx10==.
replace Highx100=0 if Highx100==.
g mturkwave=.
replace mturkwave=1 if study==2 | study==3
replace mturkwave=2 if study==4
replace mturkwave=3 if study==6

***drop 1 participant <18 to comply w IRB rules
drop if age<18
replace age=. if age==545


replace condition="ChoiceFree_Professionals" if study==1
tab condition

drop if choicebefore==.

replace study=7 if study==6 & condition=="PayBefore"
replace study=8 if study==6 & condition=="PayAfter"

** waves
g wave1=(study==2 | study==3)
g wave2=(study==4)
g wave3=(study==6 | study==7 | study==8)

**professionals cloudresearch
replace professionalscloudresearch=0 if professionalscloudresearch==.

**probabilistic incentives
g probabilistic_incentive=1 if incentiveshigh==1
replace probabilistic_incentive=1 if professionals==1
replace probabilistic_incentive=0 if professionals==0 & incentiveshigh==0

**locatedUS

	g locatedus=1 if study>1
	replace locatedus=1 if study==1 & prolific==0
	replace locatedus=1 if study==1 & prolific==1 & currentcountryofresidence=="United States"
	replace locatedus=0 if study==1 & prolific==1 & currentcountryofresidence!="United States"
	drop currentcountryofresidence

**For appendix
g wave="AMT-1" if wave1==1
replace wave="AMT-2" if wave2==1
replace wave="AMT-3" if wave3==1
replace wave="Prolific/CloudResearch" if study==1

g incentivedesign="0.15" if wave1==1
replace incentivedesign="15 (to 1 out of 100)" if incentiveshigh==1 | wave=="Prolific/CloudResearch"
replace incentivedesign="1.50" if Highx10==1
replace incentivedesign="15 (to everyone)" if Highx100==1
replace incentivedesign="0.15" if wave2==1 & incentiveshigh==0
replace incentivedesign="0.15" if wave3==1 & Highx10==0 & Highx100==0

g year=2019 if wave1==1
replace year=2020 if wave2==1 | wave=="Prolific/CloudResearch"
replace year=2021 if wave3==1


g treatment=0 if condition=="ChoiceFree"
replace treatment=1 if  professionals==1
replace treatment=2 if condition=="PayBefore" 
replace treatment=3 if condition=="PayAfter"
gen selfishseeincentivecostly=stdalpha*incentivecostly
gen selfishseequalitycostly=stdalpha*qualitycostly

gen committ=1-choicebefore

*Separate dummies, instead of factor variables in regression
g professionalsfree=(professionals==1)
g seeincentivecostly=condition=="PayBefore" 
g seequalitycostly=condition=="PayAfter"
g incentiveshigh_incentiveleft=incentiveshigh*incentiveleft

*blinding

g avoid_incentiveinfo=(chooseblindness==1)
replace avoid_incentiveinfo=. if chooseblindness==.
gen choiceafter=1-choicebefore

**Recommendations
***Gen indicator for whether participants get their desired choice of info order
gen getyourchoice=0
replace getyourchoice=1 if choicebefore==1 & getbefore==1
replace getyourchoice=1 if choicebefore==0 & getbefore==0
label var getyourchoice "Random Assignment to Preferred Order"

g choicebefore_getyourchoice=choicebefore*getyourchoice	
label var choicebefore_getyourchoice "Prefer to See Incentive First X Random Assignment"


g getbeforenoconflict=getbefore*(1-conflict)	
g noconflict=1-conflict
g choicebeforenoconflict=choicebefore*(1-conflict)

g choicebefore_getbefore=choicebefore*getbefore
g choicebefore_getbefore_nocon=choicebefore*getbefore*(1-conflict)

g notgetyourchoice=(1-getyourchoice)
g notgetyourchoicenoconflict=(1-getyourchoice)*(1-conflict)
g choicebeforenotgetyourchoice=notgetyourchoice*choicebefore

gen choicebeforegetbefore=choicebefore*getbefore

***Beliefs
g logsignalhigh=-log(2) /*prob signal H if q=L 0.4 / prob signal H if q=H 0.8 --> log(1/2)=-log(2)*/
g logsignallow=log(3) /*prob signal L if q=L 0.6 / prob signal L if quality H 0.2 --> log(3) */

g logitbelief=log(belief/(100-belief))	/*that B is low quality*/


g good=0
replace good=logsignallow if incentiveA==1 & signalblue==0
replace good=logsignalhigh if incentiveB==1 & signalblue==1
g bad=0
replace bad=logsignallow if incentiveB==1 & signalblue==0
replace bad=logsignalhigh if incentiveA==1 & signalblue==1

g goodchoicebefore=good*choicebefore 
g badchoicebefore=bad*choicebefore 

g goodgetyourchoice=good*getyourchoice
g badgetyourchoice=bad*getyourchoice

g goodnotgetyourchoice=good*(1-getyourchoice)
g badnotgetyourchoice=bad*(1-getyourchoice)

g goodchoicebeforenotgetyourchoice=good*(1-getyourchoice)*choicebefore
g badchoicebeforenotgetyourchoice=bad*(1-getyourchoice)*choicebefore

g bayesianposterior=33 if signalblue==1
replace bayesianposterior=75 if signalblue==0

label var good "Good news (log odds incl.)"
label var bad "Bad news (log odds incl.)"
label var badchoicebefore "Bad news (log) X Pref Inc 1st"
label var goodchoicebefore "Good news (log) X Pref Inc 1st"
label var goodgetyourchoice "Good news (log) X Get Choice"
label var badgetyourchoice "Bad news (log) X Get Choice"
label var goodnotgetyourchoice "Good news (log) X Not Get Choice"
label var badnotgetyourchoice "Bad news (log) X Not Get Choice"
label var goodchoicebeforenotgetyourchoice "Good news (log) X Prefer Inc 1st X Not Get Choice"
label var badchoicebeforenotgetyourchoice "bad news (log) X Prefer Inc 1st X Not Get Choice"
label var bayesianposterior "Bayesian Posterior"

g badsignal=0
replace badsignal=1 if incentiveB==1 & signalblue==0
replace badsignal=1 if incentiveA==1 & signalblue==1 
			

*Gen variables for those who update in correct direction
*belief is likelihood that quality is LOW, so if signal is blue($2), likelihood should be 25
g updatecorrect=(belief<50) if signalblue==1
replace updatecorrect=(belief>50) if signalblue==0

g updatedirection=1 if signalblue==1 & belief<50
replace updatedirection=1 if signalblue==0 & belief>50
replace updatedirection=0 if belief==50
replace updatedirection=-1 if belief>50 & signalblue==1
replace updatedirection=-1 if belief<50 & signalblue==0

g updatewrong=(updatedirection==-1)

**cumulative beliefs distance
g beliefdistance=belief-33.3 if signalblue==1 /*positive difference=too little updating*/
replace beliefdistance=75-belief if signalblue==0 /*positive difference=too little updating*/

g relbeliefdistance=(50-belief)/(50-33) if signalblue==1
replace relbeliefdistance=(50-belief)/(50-75) if signalblue==0

**correct direction - incentivized measure
*Prob quality of B is low, bin #5 - 41-50%

g correctdirbin=0
replace correctdirbin=1 if signalblue==1 & beliefbin<6 & beliefbin!=. /*if signal is blue, $2, it decreases*/
replace correctdirbin=1 if signalblue==0 & beliefbin>4 & beliefbin!=. /*if signal is blue, $0, it decreases*/

*correct bin - incentivized measure
g beliefinbin=0
replace beliefinbin=1 if belief<11 & beliefbin==1
replace beliefinbin=1 if belief>10 & belief<21 & beliefbin==2
replace beliefinbin=1 if belief>20 & belief<31 & beliefbin==3
replace beliefinbin=1 if belief>30 & belief<41 & beliefbin==4
replace beliefinbin=1 if belief>40 & belief<51 & beliefbin==5
replace beliefinbin=1 if belief>50 & belief<61 & beliefbin==6
replace beliefinbin=1 if belief>60 & belief<71 & beliefbin==7
replace beliefinbin=1 if belief>70 & belief<81 & beliefbin==8
replace beliefinbin=1 if belief>80 & belief<91 & beliefbin==9
replace beliefinbin=1 if belief>90 & belief!=. & beliefbin==10
replace beliefinbin=. if belief==. | beliefbin==. 

** belief at prior

g belief_prior=0
replace belief_prior=1 if beliefbin==5
replace belief_prior=. if beliefbin==.

*** belief at prior - continuous
g belief_prior_continuos=0
replace belief_prior_continuos=1 if belief==50
replace belief_prior_continuos=. if belief==.

drop newid 

save "${data}choice_experiments.dta", replace


/*-----CHOICE - CODED EXPLANATIONS OF CHOICE -------*/
*IMPORT EXPLANATIONS CSV
clear
import excel "${raw}Choice_explanations_deidentified.xlsx", first case(l)		
save "${data}coding_explanations.dta", replace

u "${data}choice_experiments.dta", clear
keep if mturkwave==2 | professionals==1
rename id participant_id 
g id=shortid 
bys id: egen countid=count(id)
tab countid study
drop if alphavaluefinal==. & study!=1

merge 1:m shortid using "${data}coding_explanations.dta", force
rename _merge _mergedecisions
keep if _mergedecisions==3
tab countid study if rater==1

replace condition="ChoiceFree_Professionals" if study==1
egen rowsum=rowtotal(limitbias nomatter commission_expl feeling other)

g mergedcategory=1 if limitbias==1 & rowsum<2
replace mergedcategory=2 if nomatter==1 & rowsum<2
replace mergedcategory=3 if commission_expl==1 & rowsum<2
replace mergedcategory=4 if (feeling==1 | other==1) & (rowsum==1 | rowsum==2)
replace mergedcategory=5 if rowsum==0
replace mergedcategory=6 if rowsum>1 & limitbias==1 & nomatter==1
replace mergedcategory=7 if rowsum>1 & limitbias==1 & commission_expl==1
replace mergedcategory=8 if rowsum>1 & limitbias==1 & (feeling==1 | other==1)
replace mergedcategory=9 if rowsum>1 & commission_expl==1 & nomatter==1
replace mergedcategory=10 if rowsum>1 & nomatter==1 & (feeling==1 | other==1)		
replace mergedcategory=11 if rowsum>1 & commission_expl==1 & (feeling==1 | other==1)

g otherreason=(other==1 | feeling==1)

reshape wide limitbias nomatter commission_expl feeling other comments rowsum ///
	mergedcategory otherreason, i(shortid) j(rater)
	
g nocategory1=(limitbias1==0 & nomatter1==0 & commission_expl1==0 & otherreason1==0)
g nocategory2=(limitbias2==0 & nomatter2==0 & commission_expl2==0 & otherreason2==0)
	
foreach v of newlist limitbias nomatter commission_expl feeling other otherreason nocategory {
egen `v'=rowmean(`v'1 `v'2)
}
egen mergedcategory=rowmean(mergedcategory1 mergedcategory2)

drop shortid

save "${data}Choice_coding_explanations.dta", replace



/*-------------------------------*/	
*STAKES STUDY
/*-------------------------------*/	


*** CLEANING DEIDENTIFIED DATA
clear
import delimited "${raw}Stakes_deidentified.csv", varnames(1)

drop if status==1
tab distributionchannel
drop if distributionchannel=="preview"
tab finished
keep if finished==1

g attentionpassed=attention==3 
keep if attentionpassed==1


sort newid
quietly by newid:  gen dup = cond(_N==1,0,_n)
tab dup
drop if dup!=0

drop if  q_recaptchascore<.50

g id=_n+50000


*Treatment
g condition2=0.30 if condition=="PayBefore_HighCommission"
replace condition2=0.15 if condition=="PayBefore_main"
replace condition2=0.01 if condition=="PayBeforeLowCommission"

recode choicepaybefore (2=0)
recode choicehighcommission (2=0)
recode choicelowcommission (2=0)

g choicebefore=choicepaybefore if condition=="PayBefore_main"
replace choicebefore=choicehighcommission if condition=="PayBefore_HighCommission"
replace choicebefore=choicelowcommission if condition=="PayBeforeLowCommission"

*Signal
g signalblue=(signal=="Blue")

*Beliefs
g beliefbin=q45		
g belief=q43

g beliefcorrect=0
replace beliefcorrect=1 if beliefbin==4 & signalblue==1
replace beliefcorrect=1 if beliefbin==8 & signalblue==0

*Recommendation
g recommendb=(recommendationa==2 | recommendationb==2)
g recommenda=1-recommendb	
g recommendincentive=recommenda if commission=="commissionA"
replace recommendincentive=recommendb if commission=="commissionB"


			
*Conflict
g conflict=0
replace conflict=1 if signalblue==1 & commission=="commissionA"
replace conflict=1 if signalblue==0 & commission=="commissionB"	 

*Get before
g getbeforenum=(fl_523_do=="GetBeforeIncentiveforA" | ///
				fl_548_do=="GetBeforeIncentiveforA" |  ///
				fl_657_do=="GetBeforeIncentiveforB" | ///
				fl_656_do=="GetBeforeIncentiveforB" )
drop getbefore
rename getbeforenum getbefore

destring age, replace

**Gender
gen female=.
replace female=1 if gender==2
replace female=0 if gender==1

*Prosociality
forval i=2(1)5{
recode q8`i' (1=0) (2=1)
}

rename q82 alpha1
rename q83 alpha2
rename q84 alpha3
rename q85 alpha4
rename q176 alpha5
recode alpha5 (1=0) (2=1)

egen alphamiss=rowmiss(alpha1 alpha2 alpha3 alpha4) 
keep if alphamiss==0

*Prosociality - switch point 
	reshape long alpha, i(id) j(number)
			
	sort id number		
	g previouschoice=alpha[_n-1] if number>1		
	g steptob=(alpha-previouschoice) if number>1
	bys id: egen sumsteptob=total(steptob) 

	*Alpha value final = number of times you go with the incentive to recommend Y
	g alphavalue=0 if sumsteptob==0 & alpha==0 & number==5 /*always choosing X, no incentive*/
	replace alphavalue=5 if sumsteptob==0 & alpha==1 & number==5 /*always choosing Y, incentive*/
	*Alpha value is the question number when the steptob is 1 for those who switch
	replace alphavalue=5-(number-1) if sumsteptob==1 & steptob==1 

	g inconsistentchoice=(steptob==-1)
	bys id: egen inconsistentid=total(inconsistentchoice) 

	bys id: egen alphavaluefinal=total(alphavalue) 
	replace alphavaluefinal=. if inconsistentid>0

	cap drop alphavalue inconsistentchoice previouschoice steptob

	reshape wide alpha, i(id) j(number)


	*ATTENTIVE GUYS are DROPPED FOR INCOMPLETE ANSWERS
	*KEEP CONSISTENT GUYS IN ALPHA TASK		
	tab alphavaluefinal, m
	gen selfish=(alphavaluefinal>3)
	replace selfish=. if alphavaluefinal==.
		
g choice="Stakes"

drop if choicebefore==.

g conditionnum=1 if condition=="PayBeforeLowCommission"
replace conditionnum=2 if condition=="PayBefore_main"
replace conditionnum=3 if condition=="PayBefore_HighCommission"

keep if conditionnum!=.

label var female "Female"
label var age "Age"

replace age=61 if age==611
		
g commissionlow=conditionnum==1
g commissionintermediate=conditionnum==2
g commissionhigh=conditionnum==3
		
g noconflict=1-conflict
g choicebeforenoconflict=choicebefore*noconflict
g incentiveB=commission=="commissionB"

g getyourchoice=0
replace getyourchoice=1 if choicebefore==1 & getbefore==1
replace getyourchoice=1 if choicebefore==0 & getbefore==0

g notgetyourchoice=1-getyourchoice
g choicebeforenotgetyourchoice=choicebefore*notgetyourchoice
g notgetyourchoicenoconflict=noconflict*notgetyourchoice

g commissionlowchoicebefore=commissionlow*choicebefore
g commissionhighchoicebefore=commissionhigh*choicebefore

egen stdalpha=std(alphavaluefinal)


drop responseid newid    
drop   distributionchannel userlanguage
drop fl_17_do fl_481_do fl_523_do fl_528_do fl_548_do fl_555_do fl_563_do fl_657_do fl_629_do fl_656_do fl_642_do


save "${data}stakes.dta", replace


/*-------------------------------*/	
* INFORMATION ARCHITECT STUDY
/*-------------------------------*/	

*** INFORMATION ARCHITECTS*****

*** CLEANING DEIDENTIFIED DATA
clear
import delimited "${raw}InformationArchitect_deidentified.csv", varnames(1)


drop if status==1
tab distributionchannel
drop if distributionchannel=="preview"
tab finished
keep if finished==1


sort newid
quietly by newid:  gen dup = cond(_N==1,0,_n)
tab dup
drop if dup!=0

drop if  q_recaptchascore<.50


gen attentionpassed=.
replace attentionpassed=1 if attention==3
replace attentionpassed=0 if attention==1 | attention==2 | attention==4 
tab attentionpassed
**92.11% passed the attention check (N=549) out of 602 who completed the attention check question


drop if attentionpassed!=1
sort startdate
gen id=_n+70000

** condition
replace condition="IA-Advisor" if condition=="DM-Advisor"
replace condition="IA-Client" if condition=="DM-Client"


**Gender
gen female=.
replace female=1 if gender==2
replace female=0 if gender==1

**Age
destring age, replace

gen choicebefore=0
replace choicebefore=1 if condition=="IA-Advisor" & choicedmadvisor==1
replace choicebefore=1 if condition=="IA-Client" & choicedmclient==1

***Prosociality


forval i=1(1)5{
	recode alpha`i' (1=0) (2=1)
	}

egen alphamiss=rowmiss(alpha1 alpha2 alpha3 alpha4) 
keep if alphamiss==0

reshape long alpha, i(id) j(number)


sort id number		
	g previouschoice=alpha[_n-1] if number>1		
	g steptob=(alpha-previouschoice) if number>1
	bys id: egen sumsteptob=total(steptob) 

	g alphavalue=0 if sumsteptob==0 & alpha==0 & number==5 /*always choosing X, no incentive*/
	replace alphavalue=5 if sumsteptob==0 & alpha==1 & number==5 /*always choosing Y, incentive*/
	replace alphavalue=5-(number-1) if sumsteptob==1 & steptob==1 

	g inconsistentchoice=(steptob==-1)
	bys id: egen inconsistentid=total(inconsistentchoice) 

	bys id: egen alphavaluefinal=total(alphavalue) 
	replace alphavaluefinal=. if inconsistentid>0	
	
cap drop alphavalue inconsistentchoice previouschoice steptob

	reshape wide alpha, i(id) j(number)
	
gen study=9

sort choicebefore alphavaluefinal
gen missingalpha=0
replace missingalpha=1 if alphavaluefinal==.
sort missingalpha choicebefore
gen id_pair=_n


gen IAAdvisor=(condition=="IA-Advisor")
gen IAClient=(condition=="IA-Client")

drop newid

save "${data}InformationArchitect.dta", replace

*** ADVISORS*****

*** CLEANING DEIDENTIFIED DATA
clear
import delimited "${raw}InformationArchitect_Advisor_deidentified.csv", varnames(1)


drop if status==1
tab distributionchannel
drop if distributionchannel=="preview"
tab finished
keep if finished==1


**Gender
gen female=.
replace female=1 if gender==2
replace female=0 if gender==1

**Age
destring age, replace

**Signal Blue
gen signalblue=.
replace signalblue=1 if fl_211_do=="Signal_blue2"
replace signalblue=0 if fl_211_do=="Signal_red0"
replace signalblue=1 if fl_221_do=="Signal_blue2"
replace signalblue=0 if fl_221_do=="Signal_red0"
replace signalblue=1 if fl_229_do=="Signal_blue2"
replace signalblue=0 if fl_229_do=="Signal_red0"
replace signalblue=1 if fl_127_do=="Signal_blue2"
replace signalblue=0 if fl_127_do=="Signal_red0"

**Beliefs
g beliefcorrect=0
replace beliefcorrect=1 if beliefbin==4 & signalblue==1
replace beliefcorrect=1 if beliefbin==8 & signalblue==0

*Recommendation
g recommendb=(recommendationa==2 | recommendationb==2)

*Conflict
gen conflict=0
replace conflict=1 if condition=="AfterA" & signalblue==1
replace conflict=1 if condition=="AfterB" & signalblue==0
replace conflict=1 if condition=="BeforeA" & signalblue==1
replace conflict=1 if condition=="BeforeB" & signalblue==0

**gen id
gen id_advisor=_n+75000


**Prosociality
forval i=1(1)5{
	recode alpha`i' (1=0) (2=1)
	}


reshape long alpha, i(id_advisor) j(number)
				
		sort id_advisor number		
		g previouschoice=alpha[_n-1] if number>1		

		g steptob=(alpha-previouschoice) if number>1
		bys id_advisor: egen sumsteptob=total(steptob) 
	
	*Alpha value final = number of times you go with the incentive to recommend Y
	g alphavalue=0 if sumsteptob==0 & alpha==0 & number==5 /*always choosing X, no incentive*/
	replace alphavalue=5 if sumsteptob==0 & alpha==1 & number==5 /*always choosing Y, incentive*/
	*Alpha value is the question number when the steptob is 1 for those who switch
	replace alphavalue=5-(number-1) if sumsteptob==1 & steptob==1 

	g inconsistentchoice=(steptob==-1)
	bys id_advisor: egen inconsistentid=total(inconsistentchoice) 

	bys id_advisor: egen alphavaluefinal=total(alphavalue) 
	replace alphavaluefinal=. if inconsistentid>0
	
cap drop alphavalue inconsistentchoice previouschoice steptob

	reshape wide alpha, i(id_advisor) j(number)
	
gen study=9

tab conflict

*Recommend incentive
gen recommendincentive=0                             
replace recommendincentive=1 if (condition=="AfterA" | condition=="BeforeA" ) & recommendb==0
replace recommendincentive=1 if (condition=="AfterB" | condition=="BeforeB" ) & recommendb==1

gen treatment=0
replace treatment =1 if condition=="BeforeA" | condition=="BeforeB"
replace treatment =2 if condition=="AfterA" | condition=="AfterB"

gen incentiveforA=.
replace incentiveforA=1 if  (condition=="AfterA" | condition=="BeforeA" ) 
replace incentiveforA=0 if  (condition=="AfterB" | condition=="BeforeB" ) 

gen incentiveB=1-incentiveforA

drop newid

save "${data}InformationArchitect_advisors.dta", replace


	
******* APPENDIX *********
	
/*-------------------------------*/	
*NO CHOICE - TOGETHER for APPENDIX
/*-------------------------------*/	

clear
import delimited "${raw}NoChoiceSimoultaneous_deidentified.csv", varnames(1)


drop if status==1
*drop if workerid==""
tab distributionchannel
drop if distributionchannel=="preview"
tab finished
keep if finished==1


sort newid
quietly by newid:  gen dup = cond(_N==1,0,_n)
tab dup
drop if dup!=0

rename q32 attention

g attentionpassed=attention==3
keep if attentionpassed==1


tab q_recaptchascore
drop if q_recaptchascore<.5


sort startdate
gen id=_n + 80000


*Beliefs
g beliefbin=q45 
g beliefnum=q43

**Gender
gen female=.
replace female=1 if gender==2
replace female=0 if gender==1



gen treatment=0
replace treatment =1 if condition=="BeforeA" | condition=="BeforeB"
replace treatment =2 if condition=="AfterA" | condition=="AfterB"
replace treatment =3 if condition=="TogetherA" | condition=="TogetherB"


g recommendb=(recommendationa==2 | recommendationb==2)


gen signalblue=.
replace signalblue=1 if fl_259_do=="All-Signalblue-incentiveA"
replace signalblue=0 if fl_259_do=="All-Signalred-incentiveA"
replace signalblue=0 if fl_268_do=="All-Signalred-incentiveB"
replace signalblue=1 if fl_268_do=="All-Signalblue-incentiveB"
replace signalblue=1 if fl_211_do=="Signal_blue2"
replace signalblue=0 if fl_211_do=="Signal_red0"
replace signalblue=1 if fl_221_do=="Signal_blue2"
replace signalblue=0 if fl_221_do=="Signal_red0"
replace signalblue=1 if fl_229_do=="Signal_blue2"
replace signalblue=0 if fl_229_do=="Signal_red0"
replace signalblue=1 if fl_127_do=="Signal_blue2"
replace signalblue=0 if fl_127_do=="Signal_red0"

gen conflict=0
replace conflict=1 if cond=="AfterA" & signalblue==1
replace conflict=1 if cond=="AfterB" & signalblue==0
replace conflict=1 if cond=="BeforeA" & signalblue==1
replace conflict=1 if cond=="BeforeB" & signalblue==0
replace conflict=1 if cond=="TogetherA" & signalblue==1
replace conflict=1 if cond=="TogetherB" & signalblue==0


tab conflict

gen recommendincentive=0                             
replace recommendincentive=1 if (cond=="AfterA" | cond=="BeforeA" | cond=="TogetherA") & recommendb==0
replace recommendincentive=1 if (cond=="AfterB" | cond=="BeforeB" | cond=="TogetherB") & recommendb==1

gen incentiveforA=.
replace incentiveforA=1 if  (cond=="AfterA" | cond=="BeforeA" | cond=="TogetherA") 
replace incentiveforA=0 if  (cond=="AfterB" | cond=="BeforeB" | cond=="TogetherB") 

gen incentiveB=1-incentiveforA

*Prosociality 		
forval i=2(1)5{
recode q8`i' (1=0) (2=1)
}

rename q82 alpha1
rename q83 alpha2
rename q84 alpha3
rename q85 alpha4
rename q176 alpha5
recode alpha5 (1=0) (2=1)

egen alphamiss=rowmiss(alpha1 alpha2 alpha3 alpha4) 
tab alphamiss

*alpha1-5 equal 1 is recommend the incentivized option 

*RESTRICTING RESPONDENTS TO THOSE WITH COMPLETE ALPHA CHOICES
keep if alphamiss==0

*Prosociality switch 
reshape long alpha, i(id) j(number)
	
sort id number		
g previouschoice=alpha[_n-1] if number>1		
g steptob=(alpha-previouschoice) if number>1
bys id: egen sumsteptob=total(steptob) 

*Alpha value final = number of times you go with the incentive to recommend Y
g alphavalue=0 if sumsteptob==0 & alpha==0 & number==5 /*always choosing X, no incentive*/
replace alphavalue=5 if sumsteptob==0 & alpha==1 & number==5 /*always choosing Y, incentive*/
*Alpha value is the question number when the steptob is 1 for those who switch
replace alphavalue=5-(number-1) if sumsteptob==1 & steptob==1 

g inconsistentchoice=(steptob==-1)
bys id: egen inconsistentid=total(inconsistentchoice) 

bys id: egen alphavaluefinal=total(alphavalue) 
replace alphavaluefinal=. if inconsistentid>0

tab alphavaluefinal, m
cap drop alphavalue inconsistentchoice previouschoice steptob
reshape wide alpha, i(id) j(number)

*alpha
tab alphavaluefinal if treatment!=., m

gen seeincentivefirst=(treatment==1)

gen Together=(treatment==3)

gen noconflict=1-conflict

gen wave=2


gen missingalpha=(alphavaluefinal==.)

drop responseid newid shortid

save "${data}NoChoiceSimoultaneous.dta", replace

	
/*-------------------------------*/	
*CHOICE DETERMINISTIC
/*-------------------------------*/	

clear
import delimited "${raw}ChoiceDeterministic_deidentified.csv", varnames(1)


drop if status==1
tab distributionchannel
drop if distributionchannel=="preview"
tab finished
keep if finished==1


sort newid
quietly by newid:  gen dup = cond(_N==1,0,_n)
tab dup
drop if dup!=0

drop if  q_recaptchascore<.50


g id=_n + 90000
tab attention, m
g attentionpassed=attention==3

tab attentionpassed
**78.89% passed the attention check (N=953) out of 1200 who completed the attention check question and were not excluded according to the criteria above
drop if attentionpassed!=1


tab belief
replace belief=belief*100 if belief<1

***Gen recommendB and recommenda dummies

g recommendb=.
replace recommendb=1 if  recommendationb==2
replace recommendb=0 if  recommendationb==1

replace recommendb=1 if  recommendationa==2
replace recommendb=0 if  recommendationa==1


g recommenda=1-recommendb	


***gen IncentiveB
g incentiveB=.
replace incentiveB=1 if commission=="commissionB"
replace incentiveB=0 if commission=="commissionA"

g incentiveA=.
replace incentiveA=1 if commission=="commissionA"
replace incentiveA=0 if commission=="commissionB"


*GET by treatment
g getafter=(getbefore=="GetAfter") 
cap drop getbefore
g getbefore=1-getafter



**gen bluesignal dummy
g signalblue=.
replace signalblue=1 if fl_528_do=="Signal_blue2"
replace signalblue=1 if fl_1034_do=="Signal_blue2"
replace signalblue=1 if fl_555_do=="Signal_blue2"
replace signalblue=1 if fl_1047_do=="Signal_blue2"
replace signalblue=1 if fl_629_do=="Signal_blue2"
replace signalblue=1 if fl_642_do=="Signal_blue2"
replace signalblue=1 if fl_1060_do=="Signal_blue2"
replace signalblue=1 if fl_1073_do=="Signal_blue2"

replace signalblue=0 if fl_528_do=="Signal_red0"
replace signalblue=0 if fl_555_do=="Signal_red0"
replace signalblue=0 if fl_1047_do=="Signal_red0"
replace signalblue=0 if fl_1034_do=="Signal_red0"
replace signalblue=0 if fl_629_do=="Signal_red0"
replace signalblue=0 if fl_642_do=="Signal_red0"
replace signalblue=0 if fl_1060_do=="Signal_red0"
replace signalblue=0 if fl_1073_do=="Signal_red0"
drop if signalblue==.

g conflict=.
replace conflict=1 if signalblue==0 & commission=="commissionB"
replace conflict=0 if signalblue==1 & commission=="commissionB"
replace conflict=1 if signalblue==1 & commission=="commissionA"
replace conflict=0 if signalblue==0 & commission=="commissionA"
	

tab commission 
g recommendincentive=recommendb if commission=="commissionB"
replace recommendincentive=recommenda if commission=="commissionA"	


gen choicebefore=.
replace choicebefore=1 if choicefree==1
replace choicebefore=0 if choicefree==2



**Gender
gen female=.
replace female=1 if gender==2
replace female=0 if gender==1


***Prosociality
forval i=1(1)5{
recode alpha`i' (1=0) (2=1)
}	

egen alphamiss=rowmiss(alpha1 alpha2 alpha3 alpha4) 
keep if alphamiss==0

g sample="MTurk"
rename q211 explanationchoice
gen costlydemand=0


reshape long alpha, i(id) j(number)
	
sort id number		
g previouschoice=alpha[_n-1] if number>1		
g steptob=(alpha-previouschoice) if number>1
bys id: egen sumsteptob=total(steptob) 

*Alpha value final = number of times you go with the incentive to recommend Y
g alphavalue=0 if sumsteptob==0 & alpha==0 & number==5 /*always choosing X, no incentive*/
replace alphavalue=5 if sumsteptob==0 & alpha==1 & number==5 /*always choosing Y, incentive*/
*Alpha value is the question number when the steptob is 1 for those who switch
replace alphavalue=5-(number-1) if sumsteptob==1 & steptob==1 

g inconsistentchoice=(steptob==-1)
bys id: egen inconsistentid=total(inconsistentchoice) 

bys id: egen alphavaluefinal=total(alphavalue) 
replace alphavaluefinal=. if inconsistentid>0

tab alphavaluefinal, m
cap drop alphavalue inconsistentchoice previouschoice steptob
reshape wide alpha, i(id) j(number)

*Average alpha
tab alphavaluefinal, m


gen Deterministic=(condition=="ChoiceFree_Deterministic")

egen stdalpha=std(alphavaluefinal)

* prepare data for regressions
g getbefore_Deterministic=getbefore*Deterministic	
label var getbefore_Deterministic "Get Before X Deterministic"

gen Not_getyourchoice=0
replace Not_getyourchoice=1 if choicebefore==1 & getbefore==0
replace Not_getyourchoice=1 if choicebefore==0 & getbefore==1
gen getyourchoice=1-Not_getyourchoice

gen choicebefore_Notgetyourchoice=choicebefore*Not_getyourchoice
gen noconflict=1-conflict
gen noconflict_choicebefore=noconflict*choicebefore
gen noconflict_Notgetyourchoice=noconflict*Not_getyourchoice

gen choicebefore_Deterministic=choicebefore*Deterministic
gen choicebefore_getyourchoiceDet=choicebefore*getyourchoice*Deterministic
gen choicebefore_noconflict=choicebefore*noconflict
gen Deterministic_noconflict=Deterministic*noconflict
gen Deterministic_choicebeforeNocon=Deterministic*noconflict*choicebefore

label var getyourchoice "Random Assignment to Preferred Order"
label var choicebefore_getyourchoice "Prefer to See Incentive First X Random Assignment"

gen getyourchoice_Det=getyourchoice*Deterministic

drop responseid newid shortid dup
drop fl_728_do fl_17_do fl_481_do fl_523_do fl_528_do fl_1034_do fl_548_do fl_555_do fl_1047_do fl_563_do fl_657_do fl_629_do fl_1060_do fl_656_do fl_642_do fl_1073_do
save "${data}Choice_Deterministic.dta", replace

/*-------------------------------*/	
*PREDICTIONS
/*-------------------------------*/	
clear
import excel "${raw}predictions_explanations_coded_deidentified.xlsx", firstrow
save "${data}predictions_explanations_coded.dta", replace

clear
import excel "${raw}predictions_deidentified.xlsx", firstrow


drop if _n<3
drop if status==1
drop if q_relevantidduplicate==1
drop if q_relevantidfraudscore!=0

destring  predictbeforered_es_1 predictafterred_es_1 predictbeforeblue_es_1 predictafterblue_es_1, force replace

g predictbeforered1=predictbeforered_es_1
g predictafterred1=predictafterred_es_1
g predictbeforeblue1=predictbeforeblue_es_1
g predictafterblue1=predictafterblue_es_1
g gapred=predictbeforered1-58 if predictbeforered1!=.
replace gapred=68-predictafterred1 if predictafterred1!=.
g gapblue=predictbeforeblue1-75 if predictbeforeblue1!=.
replace gapblue=86-predictafterblue1 if predictafterblue1!=.

g gap=gapred if gapred!=.
replace gap=gapblue if gapblue!=.

g choice="Predictions"

tab gap
drop if gap==.

merge 1:1 shortid using "${data}predictions_explanations_coded.dta" 


g attentivenum=attentive=="YES"
tab attentivenum
keep if attentivenum==1

tab gender 
destring age, force replace
tabstat age


replace gap=gap/100	
replace gapred=gapred/100	
replace gapblue=gapblue/100	
	
egen meangap=mean(gap)
egen sdgap=sd(gap)
egen ngap=count(gap)

g logap = meangap - sdgap/(sqrt(ngap))
g higap = meangap + sdgap/(sqrt(ngap))


g predicteddirection=predictafterred if predictafterred!=.
replace predicteddirection=predictbeforered if predictbeforered!=.
replace predicteddirection=predictafterblue if predictafterblue!=.
replace predicteddirection=predictbeforeblue if predictbeforeblue!=.


g before=1 if predictbeforered!=. | predictbeforeblue!=.
replace before=0 if predictafterred!=. | predictafterblue!=.

g predictcorrect=1 if (predicteddirection==1 & before==0) | (predicteddirection==3 & before==1)
replace predictcorrect=0 if (predicteddirection==2 & before==0) | (predicteddirection==2 & before==1)
replace predictcorrect=-1 if (predicteddirection==3 & before==0) | (predicteddirection==1 & before==1)

drop _merge responseid
save "${data}predictionsstudy.dta", replace

/*-------------------------------*/	
*CLIENTS
/*-------------------------------*/	

*** NOCHOICE, CHOICE AND STAKES EXPERIMENTS - MAIN
clear
import delimited using "${raw}Clients/Clients_Main_deidentified.csv", varnames(1)

gen follow=.
replace follow=1 if client_choiceb==1 & advisor_recommendb==1
replace follow=1 if client_choiceb==0 & advisor_recommendb==0
replace follow=0 if client_choiceb==0 & advisor_recommendb==1
replace follow=0 if client_choiceb==1 & advisor_recommendb==0

save "${data}Clients_Main.dta", replace


*** CLIENTS ** NOCHOICE, CHOICE AND STAKES EXPERIMENTS - MPL

clear
import delimited using "${raw}Clients/Clients_MPL_deidentified.csv", varnames(1)

gen follow=.
replace follow=1 if advisor_recommends_producty==1 & client_choicey==1
replace follow=1 if advisor_recommends_producty==0 & client_choicey==0
replace follow=0 if advisor_recommends_producty==0 & client_choicey==1
replace follow=0 if advisor_recommends_producty==1 & client_choicey==0

save "${data}Clients_MPL.dta", replace

*** CLIENTS ** NOCHOICE, CHOICE AND STAKES EXPERIMENTS - BLINDING

clear
import delimited using "${raw}Clients/Clients_Blinding_deidentified.csv", varnames(1)


gen follow=.
replace follow=1 if recommendation_infoavoidance==1 & choice1==1
replace follow=1 if recommendation_infoavoidance==0 & choice1==0
replace follow=0 if recommendation_infoavoidance==0 & choice1==1
replace follow=0 if recommendation_infoavoidance==1 & choice1==0

save "${data}Clients_Blinding.dta", replace

*** CLIENTS ** INFORMATION ARCHITECTS - MAIN
clear 
import delimited using "${raw}Clients/Clients_InfoArchitectsMain_deidentified.csv", varnames(1)

gen follow=.
replace follow=1 if client_choiceb==1 & advisor_recommendb==1
replace follow=1 if client_choiceb==0 & advisor_recommendb==0
replace follow=0 if client_choiceb==0 & advisor_recommendb==1
replace follow=0 if client_choiceb==1 & advisor_recommendb==0

save "${data}Clients_InfoArchitectsMain.dta", replace

*** CLIENTS ** INFORMATION ARCHITECTS - MPL ADVISORS

clear 
import delimited using "${raw}Clients/Clients_IAAdvisors_MPL_deidentified.csv", varnames(1)


gen follow=.
replace follow=1 if advisor_recommends_producty==1 & client_choicey==1
replace follow=1 if advisor_recommends_producty==0 & client_choicey==0
replace follow=0 if advisor_recommends_producty==0 & client_choicey==1
replace follow=0 if advisor_recommends_producty==1 & client_choicey==0

save "${data}Clients_IAAdvisors_MPL.dta", replace

*** CLIENTS ** INFORMATION ARCHITECTS - MPL INFO ARCHITECTS
clear
import delimited using "${raw}Clients/Clients_IAInfoArchitects_MPL_deidentified.csv", varnames(1)


gen follow=.
replace follow=1 if advisor_recommends_producty==1 & client_choicey==1
replace follow=1 if advisor_recommends_producty==0 & client_choicey==0
replace follow=0 if advisor_recommends_producty==0 & client_choicey==1
replace follow=0 if advisor_recommends_producty==1 & client_choicey==0

save "${data}Clients_IAInfoArchitects_MPL.dta", replace


*** CLIENTS ** NOCHOICE SIMULTANEOUS - MAIN

clear 
import delimited using "${raw}Clients/Clients_NoChoiceSimultaneousMain_deidentified.csv", varnames(1)

gen follow=.
replace follow=1 if advisor_recommendb==1 & client_choiceb==1
replace follow=1 if advisor_recommendb==0 & client_choiceb==0
replace follow=0 if advisor_recommendb==0 & client_choiceb==1
replace follow=0 if advisor_recommendb==1 & client_choiceb==0

save "${data}Clients_NoChoiceSimultaneousMain.dta", replace


*** CLIENTS ** NOCHOICE SIMULTANEOUS - MPL

clear
import delimited using "${raw}Clients/Clients_NoChoiceSimultaneousMPL_deidentified.csv", varnames(1)


gen follow=.
replace follow=1 if advisor_recommends_producty==1 & client_choicey==1
replace follow=1 if advisor_recommends_producty==0 & client_choicey==0
replace follow=0 if advisor_recommends_producty==0 & client_choicey==1
replace follow=0 if advisor_recommends_producty==1 & client_choicey==0

save "${data}Clients_NoChoiceSimultaneousMPL.dta", replace

*** CLIENTS ** CHOICE DETERMINISTIC - MAIN
clear 
import delimited using "${raw}Clients/Clients_ChoiceDeterministic_Main_deidentified.csv", varnames(1)
gen follow=.
replace follow=1 if advisor_recommendb==1 & client_choiceb==1
replace follow=1 if advisor_recommendb==0 & client_choiceb==0
replace follow=0 if advisor_recommendb==0 & client_choiceb==1
replace follow=0 if advisor_recommendb==1 & client_choiceb==0

save "${data}Clients_ChoiceDeterministic_Main.dta", replace

*** CLIENTS ** CHOICE DETERMINISTIC - MPL


clear 
import delimited using "${raw}Clients/Clients_ChoiceDeterministic_MPL_deidentified.csv", varnames(1)


gen follow=.
replace follow=1 if advisor_recommends_producty==1 & client_choicey==1
replace follow=1 if advisor_recommends_producty==0 & client_choicey==0
replace follow=0 if advisor_recommends_producty==0 & client_choicey==1
replace follow=0 if advisor_recommends_producty==1 & client_choicey==0

save "${data}Clients_ChoiceDeterministic_MPL.dta", replace

*drop unnecessary files
rm "${data}choice1.dta"
rm "${data}choice2.dta"
rm "${data}choice3.dta"
rm "${data}professionals_prolific.dta"
rm "${data}coding_explanations.dta"
