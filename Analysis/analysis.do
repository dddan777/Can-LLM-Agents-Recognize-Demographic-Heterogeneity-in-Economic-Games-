capture cd "C:\Users\xiaog\Dropbox\Shared Research Folders\ChatGPT-A Turing test\12.11 Can LLM Display Demographic Heterogeneity\Analysis"
capture cd "C:\Users\Jubo Yan\Dropbox\Shared Research Folders\ChatGPT-A Turing test\12.11 Can LLM Display Demographic Heterogeneity\Analysis"
use gpt_player.dta, replace
*use gpt_opponent.dta, replace
local filename "player"
*local filename "opponent"

* delete game_type "no_boundary_beauty_contest_game"
drop if game_type == "no_boundary_beauty_contest_game"

* delete attribute "occupation"
drop if attribute == "occupation"

***shorten game type name***
replace game_type = strreverse(game_type)
replace game_type = substr(game_type, 6, 30)
replace game_type = strreverse(game_type)
replace game_type = "ultimatumR" if game_type == "ultimatumResponder"

***encode all the demographic values***
gen gender = attribute_value if attribute == "gender"
encode gender, gen(gender_n)
gen income = attribute_value if attribute == "annual income"
encode income, gen(income_n)
gen age = attribute_value if attribute == "age"
encode age, gen(age_n)
recode age_n (1 = 3) (3 = 1)
gen education = attribute_value if attribute == "educational attainment"
encode education, gen(education_n)
gen employment = attribute_value if attribute == "employment status"
encode employment, gen(employment_n)
gen occupation = attribute_value if attribute == "occupation"
encode occupation, gen(occupation_n)
gen race = attribute_value if attribute == "race"
encode race, gen(race_n)

***set the demographic value to 0 if no demographic info is provided in prompts***
foreach var_n in "gender_n" "income_n" "age_n" "education_n" "employment_n" "occupation_n" "race_n" {
	replace `var_n' = 0 if attribute == "control"
}

***reorder the values for each demographic variable***
recode gender_n (0 = 0 "no gender info"), gen(gender_nn)
drop gender gender_n
rename gender_nn gender

recode income_n (0 = 0 "no income info") (1 = 3 "high income(higher than 145,500$)") (2 = 1 "lower income (less than 48,500$)") (3 = 2 "middle income (between 48,500$-145,500$)"), gen(income_nn)
drop income income_n
rename income_nn income

recode age_n (0 = 0 "no age info") (1 = 3 "65 years old and over") (2 = 2 "between 18-65 years old") (3 = 1 "under 18 years old"), gen(age_nn)
drop age age_n
rename age_nn age

recode education_n (0 = 0 "no edu info") (1 = 3 "Bachelor's degree") (2 = 4 "Graduate or professional degree") (3 = 2 " associate's degree") (4 = 1 "high school diploma (includes equivalency)"), gen(education_nn)
drop education education_n
rename education_nn education

recode employment_n (0 = 0 "no employment info") (1 = 2 "unemployed") (2 = 1 "employed"), gen(employment_nn)
drop employment employment_n
rename employment_nn employment

recode race_n (0 = 0 "no race info") (1 = 3 "american indian and alaska native") (5 = 1 "caucasian") (3 = 2 "black or african american") (2 = 4 "asian") (4 = 5 "native hawaiian and other pacific islander"), gen(race_nn)
drop race race_n
rename race_nn race

recode occupation_n (0 = 0 "no occupation info"), gen(occupation_nn)
drop occupation occupation_n
rename occupation_nn occupation

*Data Description & Descriptive Statistics
foreach var in gender age income race education employment {
    di as text "===== Summaries of 'answer' by game_type and `var' ====="
    bysort game_type `var': summarize answer
    di as text "====================================================="
}



***standardize answers by game-demographic***
bysort game_type: egen std_answer = std(answer)
foreach var in "gender" "age" "race" "employment" "education" "income" {
	bysort game_type: egen std_answer_`var' = std(answer) if `var' != .
}
***standardize answers by game***
bysort game_type: egen std2_answer = std(answer)

gen ln_answer = log(answer + 1)


***generage dummy variables for each demographic value***
gen control = (attribute_value == "control")

gen female = (gender == 1)
gen male = (gender == 2)

gen low = (income == 1)
gen medium = (income == 2)
gen high = (income == 3) 

gen caucasian = (race == 1)
gen black = (race == 2)
gen native_american = (race == 3)
gen asian = (race == 4)
gen pacific = (race == 5)

gen highschool = (education == 1)
gen associate = (education == 2)
gen bachelor = (education == 3)
gen graduate = (education == 4)

gen young = (age == 1)
gen middle = (age == 2)
gen old = (age == 3)

gen unemployed = (employment == 1)
gen employed = (employment == 2)

***label all the explanation variables***
label var altruism "Altruism"
label var social_norm "Social Norm" 
label var self_interest_maximizing "Self-interest"
label var risk_aversion "Risk-aversion" 
label var stereotype "Stereotyping" 
label var trust "Trust" 
label var strategic_thinking "Strategy"

--
***Test variance equality using variance ratio test***

capture drop d_*
log using var_test, replace
foreach demo in "gender" "age" "income" "race" "education" "employment" {
	gen d_`demo' = `demo' > 0
	replace d_`demo' = . if `demo' == .
	by game_type, sort: sdtest answer, by(d_`demo')
}
log close
--
***To draw histogram***
foreach game in "dictator" "public_good" "beauty_contest" "ultimatum" "ultimatumR" "trust" "trustBanker"  {
	
	foreach demo in "gender" "age" "income" "race" "education" "employment" {
		local xxlabel = "nolabels"
		local yytitle = " "
		
		if "`demo'" == "income"{
			local xxlabel = "labels"
		}
		else if "`demo'" == "employment"{
			local xxlabel = "labels"
		}
		
		if "`game'" == "dictator" { 
			local yytitle "`demo'"
		}
		
		twoway (histogram answer if game_type == "`game'" & `demo' == 0, width(10) percent xlabel(0(10)100) color(green%30)) (histogram answer if game_type == "`game'" & `demo' > 0 & `demo' < . , width(10) percent xlabel(0(10)100) color(red%30)), legend(order(1 "no demo" 2 "`demo'" )) legend(off) graphregion(margin(zero)) xlabel(0(25)100, `xxlabel') xtitle(`" "') ylabel(none, nolabels) ytitle("`yytitle'", size(large)) name(hist_`game'_`demo', replace) 
		
	}

}

foreach game in "dictator" "public_good" "beauty_contest" "ultimatum" "ultimatumR" "trust" "trustBanker" {
	if "`game'" == "dictator" {
		local ttitle = "Dictator"
	}
	else if "`game'" == "public_good" {
		local ttitle = "Public Good"
	}
	else if "`game'" == "beauty_contest"{
		local ttitle = "Beauty Contest"
	}
	else if "`game'" == "ultimatum"{
		local ttitle = "Ultimatum Proposer"
	}
	else if "`game'" == "ultimatumR"{
		local ttitle = "Ultimatum Responder"
	}
	else if "`game'" == "trust" {
		local ttitle = "Trust"
	}
	else if "`game'" == "trustBanker" {
		local ttitle = "Trustworthiness"
	}
	
	
	graph combine hist_`game'_gender hist_`game'_age hist_`game'_income, title(`ttitle', size(medium)) xcommon rows(3) name(`game'_1, replace)
	graph combine hist_`game'_race hist_`game'_education hist_`game'_employment, title(`ttitle', size(medium)) xcommon rows(3) name(`game'_2, replace)
}

graph combine dictator_1 public_good_1 beauty_contest_1 ultimatum_1 ultimatumR_1 trust_1 trustBanker_1, ycommon xcommon cols(7) xsize(20) ysize(10) 
graph export hist1_`filename'.png, replace
graph combine dictator_2 public_good_2 beauty_contest_2 ultimatum_2 ultimatumR_2 trust_2 trustBanker_2, ycommon xcommon cols(7) xsize(20) ysize(10) 
graph export hist2_`filename'.png, replace

***To draw regression coefficients of each different demographic value***
replace std_answer_gender = 0 if game_type == "dictator" 
replace std_answer_race = 0 if game_type == "dictator"

foreach demo in "gender" "age" "race" "employment" "education" "income" {
	local xlist = ""
	if "`demo'" == "gender" {
		local xlist female male
		local llabel female = "Female" male = "Male"
	}
	else if "`demo'" == "age" {
		local xlist young middle old
		local llable young = "Below 18" middle = "18-65" old = "Above 65"
	}
	else if "`demo'" == "race" {
		local xlist caucasian black native asian pacific
		local llabel caucasian = "Caucasian" black = "African American" native_american = "Native American" asian = "Asian" pacific = "Pacific Islander"
	}
	else if "`demo'" == "employment" {
		local xlist unemployed employed
		local llabel unemployed = "Unemployed" employed = "Employed"
	}
	else if "`demo'" == "education" {
		local xlist highschool associate bachelor graduate
		local llabel highschool = "High School" associate = "Associate Degree" bachelor = "Bachelor Degree" graduate = "Graduate Degree"
	}
	else if "`demo'" == "income" {
		local xlist low medium high
		local llabel low = "Low" medium = "Medium" high = "High"
	}
		
	foreach game in "dictator" "ultimatum" "ultimatumR" "trust" "trustBanker" "public_good" "beauty_contest"{
	quietly eststo `game'_`demo': reg std_answer_`demo' `xlist' if game_type == "`game'"
	
	}

	coefplot (dictator_`demo', drop(_cons) label (Dictator))   (public_good_`demo', drop(_cons) label(Public Good)) (beauty_contest_`demo', drop(_cons) label(Beauty Contest)), bylabel("symmetric games")  || (ultimatum_`demo', drop(_cons) label (Ultimatum Proposer)) (ultimatumR_`demo', drop(_cons) label(Ultimatum Responder)) (trust_`demo', drop(_cons) label(Trust)) (trustBanker_`demo', drop(_cons) label(Trustworhiness)), bylabel("asymmetric games") ||, grid(between glcolor(orange) glpattern(dash)) msymbol(D) mfcolor(white) coeflabel(_cons = "Constant" control = "No Demo" `llabel', wrap(20) notick) xline(0) xsize(20) ysize(10)  norecycle legend(order(2 4 8 10 6 - "" 12 14) row(2)) mlabel format(%9.2f) mlabposition(12) mlabgap(*2)
	graph export `demo'_`filename'.png, replace
}

	   
***regression on the impact of demographics on properties***

local replace replace
	foreach var in "altruism" "social_norm" "self_interest_maximizing" "risk_aversion" "stereotype" "trust" "strategic_thinking" {
	capture drop std_`var'
	egen std_`var' = std(`var')
	quietly reg std_`var' female male young middle old low medium high caucasian black native asian pacific highschool associate bachelor graduate unemployed employed if attribute != "Control" 
	outreg2 using property_`filename'.xls, `replace' label
	
	local replace
}

***regression on the impact of properties on behavior***

local replace replace
foreach game in "dictator" "public_good" "beauty_contest" "ultimatum" "ultimatumR" "trust" "trustBanker" {
	quietly eststo `game': reg std2_answer altruism social_norm self_interest_maximizing risk_aversion stereotype trust strategic_thinking if game_type == "`game'"
	outreg2 using explanation_`filename'.xls, `replace' label ctitle(`game')
	local replace
}



