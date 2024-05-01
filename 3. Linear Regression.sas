/*Step 3: Linear Regression.*/

/*Filtering rows containing only individuals who donated this year,
for further Linear Regression building.*/
data pm_game.hist2_filtered;
	set pm_game.hist2;
	if GaveThisYear=1;
run;

/*Correlation analysis*/
ods graphics / reset=all imagemap;
proc corr data=pm_game.hist2_filtered rank
	plots(only maxpoints=150000)=scatter(nvar=all ellipse=none);
	var Age AmtLastYear
		Frequency MaxGift MinGift
		NbActivities Recency Referrals Salary Seniority
		SeniorList TotalGift Woman Contact education_1
		education_2 city_1 city_2 city_3;
	with AmtThisYear;
run;
proc corr data=pm_game.hist2_filtered;
	var AmtThisYear Age AmtLastYear
		Frequency MaxGift MinGift
		NbActivities Recency Referrals Salary Seniority
		SeniorList TotalGift Woman Contact education_1
		education_2 city_1 city_2 city_3;
run;

/*ANOVA to identify significant interactions*/
proc glm data=pm_game.hist2_filtered;
	class woman education city;
model amtthisyear = woman education city woman*education woman*city city*education;
run;

/*Linear regression building using the backwards selection method*/
proc reg data=pm_game.hist2_filtered plots(only)=(rsquare adjrsq cp);
Backward: model AmtThisYear = Age AmtLastYear
		Frequency MaxGift MinGift
		NbActivities Recency Referrals Salary Seniority
		SeniorList TotalGift Woman Contact education_1
		education_2 city_1 city_2 city_3
	/ selection=backward;;
run;

/*Linear regression building using the automated model selection
technique based on AIC and stepwise selection.
After the model is built, the scoring on two datasets is performed.*/
title1 'AIC';
proc glmselect data=pm_game.hist2_filtered plots=all;
	class education city;
	AIC: model AmtThisYear=Age AmtLastYear Education City
		Frequency MaxGift MinGift
		NbActivities Recency Referrals Salary Seniority
		SeniorList TotalGift Woman Contact woman*education/
	selection=stepwise select=AIC details=steps showpvalues;
	score data=pm_game.score2_contact out=pm_game.score2_contact;
	score data=pm_game.score2_nocontact out=pm_game.score2_nocontact;
run;

/*Merging of the two scoring datasets,
calculation of Uplift,
sorting by Uplift value in descending order.*/
data pm_game.hist2_combined;
	merge pm_game.score2_contact (rename=(p_1=PGivingContact
		p_AmtThisYear=PredContact))
		pm_game.score2_nocontact (keep=id p_1 p_AmtThisYear 
		rename=(p_1=PGivingNoContact p_AmtThisYear=PredNoContact));
	by id;
run;
data pm_game.hist2_combined;
	set pm_game.hist2_combined;
	EC = predcontact*pgivingcontact;
	ENC = prednocontact*pgivingnocontact;
	Uplift = EC - ENC;
	keep id GaveLastYear AmtLastYear PredContact PredNoContact PGivingContact PGivingNoContact
		EC ENC Uplift;
run;
proc sort data=pm_game.hist2_combined;
	by descending Uplift;
run;

