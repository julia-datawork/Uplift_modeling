libname pm_game "E:\Schulich MBAN\5210 Predictive Modelling II\Assignment Data 5";
/*Step 1: Data exploration and cleaning*/

title1 "Means for numerical variables";
proc means data=pm_game.hist2;
	var Age AmtLastYear AmtThisYear Frequency
		 MaxGift MinGift NbActivities
		Recency Referrals Salary SeniorList Seniority TotalGift;
run;

title1 "Frequency tables for categorical variables";
proc freq data=pm_game.hist2;
	tables City Education GaveLastYear GaveThisYear Woman Contact;
run;

/*Training dataset: Decoding of categorical variables and missing values imputation.
Missing values in Recency, Frequency, Seniority, MinGift, MaxGift and TotalGift 
were imputed with zeros (in this case, missing value mean that an individual
did not donate in the past. e.g. last year was the first donation.*/
data pm_game.hist2;
	set pm_game.hist2;
	drop LastName FirstName;
	if city="City" then city_1=1;
		else city_1=0;
	if city="Downtown" then city_2=1;
		else city_2=0;
	if city="Rural" then city_3=1;
		else city_3=0;
	if education="Elementary" then education_1=1;
		else education_1=0;
	if education="High School" then education_2=1;
		else education_2=0;
	if recency="" then recency=0;
	if frequency="" then frequency=0;
	if seniority="" then seniority=0;
	if totalgift="" then totalgift=0;
	if mingift="" then mingift=0;
	if maxgift="" then maxgift=0;
run;
/*Scoring dataset #1: Decoding of categorical variables and missing values imputation.*/
data pm_game.score2_contact;
	set pm_game.score2_contact;
	if city="City" then city_1=1;
		else city_1=0;
	if city="Downtown" then city_2=1;
		else city_2=0;
	if city="Rural" then city_3=1;
		else city_3=0;
	if education="Elementary" then education_1=1;
		else education_1=0;
	if education="High School" then education_2=1;
		else education_2=0;
	if recency="" then recency=0;
	if frequency="" then frequency=0;
	if seniority="" then seniority=0;
	if totalgift="" then totalgift=0;
	if mingift="" then mingift=0;
	if maxgift="" then maxgift=0;
run;

/*Scoring dataset #2: Decoding of categorical variables and missing values imputation.*/
data pm_game.score2_nocontact;
	set pm_game.score2_nocontact;
	if city="City" then city_1=1;
		else city_1=0;
	if city="Downtown" then city_2=1;
		else city_2=0;
	if city="Rural" then city_3=1;
		else city_3=0;
	if education="Elementary" then education_1=1;
		else education_1=0;
	if education="High School" then education_2=1;
		else education_2=0;
	if recency="" then recency=0;
	if frequency="" then frequency=0;
	if seniority="" then seniority=0;
	if totalgift="" then totalgift=0;
	if mingift="" then mingift=0;
	if maxgift="" then maxgift=0;
run;
