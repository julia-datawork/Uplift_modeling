/*Step 2: Logistic Regression*/

/*Most of custom code for the advanced logistic regression implementation
was borrowed from Lecture 5: Advanced Regression*/

/* prepare for stratification by GaveThisYear*/
proc sort data=pm_game.hist2 out=hist2; 
   by GaveThisYear; 
run;
/* set up data split indicator */
proc surveyselect noprint data=hist2 seed=1234 samprate=0.7 
		stratumseed=restore outall out=sample;
	strata GaveThisYear;
run;
/* verify target distribution split */
proc freq data=sample;
	tables GaveThisYear*selected;
run;

/* split data into training and validation */
data pm_game.train(drop=selected SelectionProb SamplingWeight) 
     pm_game.valid(drop=selected SelectionProb SamplingWeight);
	set sample;
	if selected=1 then output pm_game.train;
	else output pm_game.valid;
run;



/* Store predictor variables to one variable,
for more convenient use during the further coding.*/
%let vars = Age AmtLastYear GaveLastYear city_1 city_2 city_3
		education_1 education_2 NbActivities Referrals Salary SeniorList Woman
		recency frequency seniority totalgift mingift maxgift contact;

title1 'Determine P-Value for Entry and Retention';
proc sql;
	select 1-probchi(log(sum(GaveThisYear ge 0)),1) into :sl
	from pm_game.train;
quit;
/*Detection of interactions using the forward selection method.
Entry level was determined on the previous step.*/
title1 'Detect Interactions using Forward Selection';
proc logistic data=pm_game.train;
	model GaveThisYear(event='1')= &vars
		Age | AmtLastYear | GaveLastYear | city_1 | city_2 | city_3 |
		education_1 | education_2 | NbActivities | Referrals | Salary | SeniorList | Woman |
		recency | frequency | seniority | totalgift | mingift | maxgift | contact @2 / 
			include=20 clodds=pl selection=forward slentry=&sl;
run;

/*Store significant interactions obtained during the forward selection
logistic model building into one variable.*/
%let interactions = city_1*education_1 city_2*education_1
	city_1*education_2 city_3*education_2 GaveLastYear*Referrals city_2*Referrals
	NbActivities*Referrals GaveLastYear*SeniorList AmtLastYear*Salary AmtLastYear*SeniorList
	city_1*SeniorList city_2*SeniorList city_3*SeniorList
	Salary*Woman NbActivities*Recency NbActivities*Frequency Referrals*Frequency
	Recency*Frequency Referrals*Seniority SeniorList*Seniority Recency*Seniority
	Age*Contact GaveLastYear*Contact city_1*contact
	city_3*contact education_1*contact education_2*contact NbActivities*Contact
	Referrals*Contact Salary*Contact SeniorList*Contact Woman*Contact Frequency*Contact
	Seniority*Contact TotalGift*Contact MinGift*Contact MaxGift*Contact; 

/*Fast Backward method including the previously obtained interactions */
/*After the observation of results from Fast Backward and All Possible methods,
the model obtained using Fast Backward techique was selected.
Therefore, scoring is also performed in this procedure.*/
title1 'Fast Backward Selection';
proc logistic data=pm_game.train;
	model GaveThisYear(event='1')= &vars &interactions / 
		clodds=pl selection=backward fast slstay=&sl hier=single;
run;
/*Store variables and interactions finally obtained with
the fast backwards method into one variable*/
%let backwards_selected = GaveLastYear city_1 city_2 city_3 education_1
	education_2 NbActivities Referrals Salary SeniorList Woman Recency
	Frequency Seniority TotalGift MinGift MaxGift Contact
	city_1*education_1 city_2*education_1 city_1*education_2 city_3*education_2
	GaveLastYear*Referrals city_2*Referrals NbActivities*Referrals GaveLastYear*SeniorList
	city_1*SeniorList city_2*SeniorList city_3*SeniorList Salary*Woman
	NbActivities*Recency NbActivities*Frequency Referrals*Frequency Recency*Frequency
	Referrals*Seniority Age*Contact GaveLastYear*Contact city_1*Contact
	city_3*Contact education_1*Contact education_2*Contact NbActivities*Contact
	Referrals*Contact Salary*Contact SeniorList*Contact Woman*Contact
	Frequency*Contact Seniority*Contact TotalGift*Contact MinGift*Contact MaxGift*Contact;

/*Score the model obtained by fast backwards selection method on the validation set*/
proc logistic data=pm_game.train;
	model GaveThisYear(event='1')=&backwards_selected;
	score data=pm_game.valid fitstat out=pm_game.score_valid_b;
run;



/*All Possible Selection method including the previously obtained interactions*/
title1 'All possible Selection';
proc logistic data=pm_game.train;
	model GaveThisYear(event='1')=&vars &interactions /
	selection=score best=1;
run;

/*Model fit statistics for the models obtained on All Possible Selection Step.*/
%macro fitstat(data=,target=,event=,inputs=,best=);
	ods select none;
	ods output bestsubsets=score;
	proc logistic data=&data namelen=50;
		model &target(event="&event")=&inputs / 
			selection=score best=&best;
	run;
	proc sql noprint;
		select variablesinmodel into :inputs1 -  
		from score;
  		select NumberOfVariables into :ic1 - 
		from score;
	quit;
	%let lastindx=&SQLOBS;
	%do model_indx=1 %to &lastindx;
		%let im=&&inputs&model_indx;
		%let ic=&&ic&model_indx;
		ods output scorefitstat=stat&ic;
		proc logistic data=&data namelen=50;
			model &target(event="&event")=&im;
			score data=&data out=scored fitstat;
		run;
		proc datasets library=work nodetails nolist;
			delete scored;
		run;
		quit;
	%end;
	data modelfit;
		set stat1 - stat&lastindx;
		model=_n_;
	run;
%mend fitstat;
%fitstat(data=pm_game.train,target=GaveThisYear,event=1,
	inputs=&vars &interactions, best=1);
/* order the models by Schwarz Criterion */
proc sort data=modelfit;
	by sc;
run;

ods select all;
title1 "Fit Statistics from Models selected from Best-Subsets";
proc print data=modelfit;
	var model sc auc aic bic misclass adjrsquare brierscore;
run;

proc sql;
	select VariablesInModel into :allpos_selected
   	from score
	where numberofvariables=47;
quit;


title1 'Generate Selected Model';
proc logistic data=pm_game.train;
	model GaveThisYear(event='1')=&allpos_selected;
	score data=pm_game.valid fitstat out=pm_game.score_valid_a;
	score data=pm_game.score2_contact out=pm_game.score2_contact;
	score data=pm_game.score2_nocontact out=pm_game.score2_nocontact;
run;

/*According to the fit statistics received after model scoring on the validation set,
the  model from All-possible selection method slightly outperforms the one got through
the fast-backward method, however, two models demonstrate good performance (the value of AUC ~0.73).
So, for scoring on the new, unseen data the model obtained with All-possible technique was selected.*/

/*Confusion matrix*/
title1 'Confusion matrix for the model obtained with All Possible Selection';
proc freq data=pm_game.score_valid_a;
	tables F_GaveThisYear*I_GaveThisYear / nocol norow;
run;
title1 'Confusion matrix for the model obtained with Fast Backward Selection';
proc freq data=pm_game.score_valid_b;
	tables F_GaveThisYear*I_GaveThisYear / nocol norow;
run;