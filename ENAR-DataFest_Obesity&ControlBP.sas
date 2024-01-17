*** Set the path;
%LET PATH = C:/Users/sdutta3/OneDrive - University of Kansas Medical Center/ENAR DataFest/Data;
*%let path = C:/Users/shixi/Desktop/ENAR/Data;
%LET PATHOUT = C:\Users\sdutta3\OneDrive - University of Kansas Medical Center\ENAR DataFest\Final code;

*** ImpORt the cleaned DATA SETs;
PROC IMPORT DATAFILE="&path/dat_2011_2012_new.csv" 
	OUT=DATA_2011_2012 REPLACE
	DBMS=CSV;
RUN;

PROC IMPORT DATAFILE="&path/dat_2017_2020_new.csv" 
	OUT=DATA_2017_2020 REPLACE
	DBMS=CSV;
RUN;


** Creating data set for Propensity score matching;
DATA dat_propensity;
	SET WORK.DATA_2011_2012
		WORK.DATA_2017_2020;
	IF bmi>30 THEN exposed=1; ELSE exposed=0;
	IF svy_year="2011-2012" THEN post=0; ELSE post=1;
	IF bp_sys_mean >=130 OR bp_dia_mean >=80 OR bp_med_use="Yes";
	IF demo_age_years >= 18;
RUN;

** Divide the entire data set into HIGH BMI and LOW BMI;
DATA HIGH LOW;
	SET WORK.DAT_PROPENSITY;
	IF EXPOSED = 0 THEN OUTPUT LOW;
	ELSE OUTPUT HIGH;
RUN;

** Running propensity score matching;
PROC PSMATCH DATA=WORK.HIGH;
	CLASS demo_race demo_gender htn_aware 
		bp_med_n_class bp_med_use cc_smoke
		cc_diabetes cc_ckd cc_cvd_any edu_factor 
		marital_factor alcohol_factor svy_year demo_age_cat;
	PSMODEL svy_year = demo_race demo_gender htn_aware 
		bp_med_n_class bp_med_use cc_smoke
		cc_diabetes cc_ckd cc_cvd_any edu_factor 
		marital_factor alcohol_factor demo_age_cat incomeratio;
	MATCH method=greedy(k=1 ORDER=DESCENDING) stat=lps exact=(demo_race demo_gender demo_age_cat) caliper=.;
	OUTPUT OUT(obs=all)= yearmatched_HIGH WEIGHT=matcheattwgt MATCHid=mid1;
RUN;

** Running propensity score matching;
PROC PSMATCH DATA=WORK.LOW;
	CLASS demo_race demo_gender htn_aware 
		bp_med_n_class bp_med_use cc_smoke
		cc_diabetes cc_ckd cc_cvd_any edu_factor 
		marital_factor alcohol_factor svy_year demo_age_cat;
	PSMODEL svy_year = demo_race demo_gender htn_aware 
		bp_med_n_class bp_med_use cc_smoke
		cc_diabetes cc_ckd cc_cvd_any edu_factor 
		marital_factor alcohol_factor demo_age_cat incomeratio;
	MATCH method=greedy(k=1 ORDER=DESCENDING) stat=lps exact=(demo_race demo_gender demo_age_cat ) caliper=.;
	OUTPUT OUT(obs=all)= yearmatched_LOW WEIGHT=matcheattwgt MATCHid=mid1;
RUN;

DATA DID_DATA_PS1;
	SET WORK.yearmatched_HIGH 
		WORK.yearmatched_LOW;
RUN;

PROC SORT DATA=DID_DATA_PS1(where=(mid1>.)) OUT=did_DATA_ps1;
	BY mid1 bmi svy_year;
RUN;

PROC FREQ DATA=did_DATA_ps1;
  TABLES svy_YEAR svy_year*exposed/NOCOL NOROW NOCUM;
  TABLES exposed*svy_year*demo_race / NOCOL NOROW NOCUM;
  TABLES exposed*svy_year*demo_gender / NOCOL NOROW NOCUM;
  TABLES exposed*svy_year*demo_age_cat / NOCOL NOROW NOCUM;
  TABLES exposed*svy_year*bp_med_use / NOCOL NOROW NOCUM;
  TABLES exposed*svy_year*bp_med_n_class;
RUN;

PROC EXPORT DATA=WORK.did_DATA_ps1 DBMS=CSV REPLACE
	OUTFILE="&PATHOUT\TwoStepMatchedOut.csv";
RUN;

/***************** SBP *****************/
** Before we perform DID, we need to adjust the propensity scORe BY the other variables;
PROC GENMOD DATA=did_DATA_ps1 /*PLOTS=ALL*/;
	CLASS demo_race demo_gender 
		  demo_age_cat cc_diabetes cc_ckd cc_cvd_any
		  edu_factor marital_factOR alcohol_factor 
		  htn_aware;
	MODEL bp_sys_mean = demo_race demo_gender 
		  demo_age_cat cc_diabetes cc_ckd cc_cvd_any
		  edu_factor marital_factOR alcohol_factor
		  htn_aware incomeratio ;
	OUTPUT OUT=did_preDATA1 PRED=adjusted_sys_mean;
RUN;

* Unadjusted SBP - effects swapped and insig;
PROC MIXED DATA = did_DATA_ps1;
	CLASS post(ref="0") exposed(ref="0");
	MODEL  bp_sys_mean=post|exposed/ SOLUTION;
	LSMEANS post|exposed / DIFF;
	ESTIMATE 'DID' exposed*post 1 -1 -1 1;
	ODS OUTPUT LSMEANS=lsmeans
		SolutionF=Fixed_effect;
RUN;

PROC EXPORT DATA=WORK.LSMEANS DBMS=CSV REPLACE
	OUTFILE="&PATHOUT\UnadjSBP_overall.csv";
RUN;

PROC EXPORT DATA=WORK.Fixed_effect DBMS=CSV REPLACE
	OUTFILE="&PATHOUT\UnadjSBP_overall_FE.csv";
RUN;


PROC SGPLOT DATA=LSMEANS;
	SERIES X=post Y=estimate/ GROUP=exposed;
	SCATTER X=post Y=estimate/ GROUP=exposed;
RUN;

* Unadjusted SBP, BY group - effects swapped AND insig;
PROC SORT DATA=did_DATA_ps1;
	BY bp_med_use;
RUN;

PROC MIXED DATA = did_DATA_ps1;
	CLASS post(ref="0") exposed(ref="0") bp_med_use;
	BY bp_med_use;
	MODEL  bp_sys_mean=post|exposed/ SOLUTION;
	LSMEANS post|exposed / DIFF;
	ESTIMATE 'DID' exposed*post 1 -1 -1 1;
	ODS OUTPUT LSMEANS=lsmeans
		SolutionF=Fixed_effect;
RUN;

PROC EXPORT DATA=WORK.LSMEANS DBMS=CSV REPLACE
	OUTFILE="&PATHOUT\UnadjSBP_bygroup.csv";
RUN;

PROC EXPORT DATA=WORK.Fixed_effect DBMS=CSV REPLACE
	OUTFILE="&PATHOUT\UnadjSBP_bygroup_FE.csv";
RUN;

PROC SORT DATA=LSMEANS;
	BY bp_med_use;
RUN;

PROC SGPLOT DATA=LSMEANS;
	BY bp_med_use;
	SERIES X=post Y=estimate/ GROUP=exposed;
	SCATTER X=post Y=estimate/ GROUP=exposed;
RUN;

* Adjusted SBP - effects swapped AND insig AND even wORse than unadjusted;
PROC MIXED DATA = did_preDATA1;
	CLASS post (ref="0") exposed (ref="0");
	MODEL  adjusted_sys_mean=post|exposed/SOLUTION;
	LSMEANS post|exposed / DIFF;
	ESTIMATE 'DID' exposed*post 1 -1 -1 1;
	ODS OUTPUT LSMEANS=lsmeans
		SolutionF=Fixed_effect;
RUN;

PROC EXPORT DATA=WORK.LSMEANS DBMS=CSV REPLACE
	OUTFILE="&PATHOUT\AdjSBP_overall.csv";
RUN;

PROC EXPORT DATA=WORK.Fixed_effect DBMS=CSV REPLACE
	OUTFILE="&PATHOUT\AdjSBP_overall_FE.csv";
RUN;

PROC SGPLOT DATA=LSMEANS;
	SERIES X=post Y=estimate/ GROUP=exposed;
	SCATTER X=post Y=estimate/ GROUP=exposed;
RUN;

* Adjusted SBP, BY group - effects swapped AND insig AND even wORse than unadjusted;
PROC SORT DATA=did_preDATA1;
	BY bp_med_use;
RUN;

PROC MIXED DATA = did_preDATA1;
	CLASS post (ref="0") exposed (ref="0") bp_med_use;
	BY bp_med_use;
	MODEL  adjusted_sys_mean=post|exposed/ SOLUTION;
	LSMEANS post|exposed / DIFF;
	ESTIMATE 'DID' exposed*post 1 -1 -1 1;
	ODS OUTPUT LSMEANS=lsmeans
		SolutionF=Fixed_effect;
RUN;

PROC EXPORT DATA=WORK.LSMEANS DBMS=CSV REPLACE
	OUTFILE="&PATHOUT\AdjSBP_bygroup.csv";
RUN;

PROC EXPORT DATA=WORK.Fixed_effect DBMS=CSV REPLACE
	OUTFILE="&PATHOUT\AdjSBP_bygroup_FE.csv";
RUN;

PROC SORT DATA=LSMEANS;
	BY bp_med_use;
RUN;

PROC SGPLOT DATA=LSMEANS;
	BY bp_med_use;
	SERIES X=post Y=estimate/ GROUP=exposed;
	SCATTER X=post Y=estimate/ GROUP=exposed;
RUN;

/***************** DBP *****************/
** BefORe we performm DID, we need to adjust the propensity scORe BY the other variables;
PROC GENMOD DATA=did_DATA_ps1 /*PLOTS=ALL*/;
	CLASS demo_race demo_gender 
		  demo_age_cat cc_diabetes cc_ckd cc_cvd_any
		  edu_factor marital_factOR alcohol_factor 
		  htn_aware;
	MODEL bp_dia_mean = demo_race demo_gender 
		  demo_age_cat cc_diabetes cc_ckd cc_cvd_any
		  edu_factor marital_factor alcohol_factor
		  htn_aware incomeratio ;
	OUTPUT OUT=did_preDATA1 PRED=adjusted_dia_mean;
RUN;

* Unadjusted DBP - sig;
PROC MIXED DATA = did_DATA_ps1;
	CLASS post(ref="0") exposed(ref="0");
	MODEL  bp_dia_mean=post|exposed/ SOLUTION;
	LSMEANS post|exposed / DIFF;
	ESTIMATE 'DID' exposed*post 1 -1 -1 1;
	ODS OUTPUT LSMEANS=lsmeans
		SolutionF=Fixed_effect;
RUN;

PROC EXPORT DATA=WORK.LSMEANS DBMS=CSV REPLACE
	OUTFILE="&PATHOUT\UnadjDBP_overall.csv";
RUN;

PROC EXPORT DATA=WORK.Fixed_effect DBMS=CSV REPLACE
	OUTFILE="&PATHOUT\UnadjDBP_overall_FE.csv";
RUN;

PROC SGPLOT DATA=LSMEANS;
	SERIES X=post Y=estimate/ GROUP=exposed;
	SCATTER X=post Y=estimate/ GROUP=exposed;
RUN;

* Unadjusted SBP, by group - sig for med users;
PROC SORT DATA=did_DATA_ps1;
	BY bp_med_use;
RUN;

PROC MIXED DATA = did_DATA_ps1;
	CLASS post(ref="0") exposed(ref="0") bp_med_use;
	BY bp_med_use;
	MODEL  bp_dia_mean=post|exposed/ SOLUTION;
	LSMEANS post|exposed / DIFF;
	ESTIMATE 'DID' exposed*post 1 -1 -1 1;
	ODS OUTPUT LSMEANS=lsmeans
		SolutionF=Fixed_effect;
RUN;

PROC EXPORT DATA=WORK.LSMEANS DBMS=CSV REPLACE
	OUTFILE="&PATHOUT\UnadjDBP_bygroup.csv";
RUN;

PROC EXPORT DATA=WORK.Fixed_effect DBMS=CSV REPLACE
	OUTFILE="&PATHOUT\UnadjDBP_bygroup_FE.csv";
RUN;

PROC SORT DATA=LSMEANS;
	BY bp_med_use;
RUN;

PROC SGPLOT DATA=LSMEANS;
	BY bp_med_use;
	SERIES X=post Y=estimate/ GROUP=exposed;
	SCATTER X=post Y=estimate/ GROUP=exposed;
RUN;

* Adjusted DBP - insig;
PROC MIXED DATA = did_preDATA1;
	CLASS post (ref="0") exposed(ref="0");
	MODEL  adjusted_dia_mean=post|exposed/ SOLUTION;
	LSMEANS post|exposed / DIFF;
	ESTIMATE 'DID' exposed*post 1 -1 -1 1;
	ODS OUTPUT LSMEANS=lsmeans
		SolutionF=Fixed_effect;
RUN;

PROC EXPORT DATA=WORK.LSMEANS DBMS=CSV REPLACE
	OUTFILE="&PATHOUT\AdjDBP_overall.csv";
RUN;

PROC EXPORT DATA=WORK.Fixed_effect DBMS=CSV REPLACE
	OUTFILE="&PATHOUT\AdjDBP_overall_FE.csv";
RUN;


PROC SGPLOT DATA=LSMEANS;
	SERIES X=post Y=estimate/ GROUP=exposed;
	SCATTER X=post Y=estimate/ GROUP=exposed;
RUN;

* Adjusted DBP, by group - insig;
PROC SORT DATA=did_preDATA1;
	BY bp_med_use;
RUN;

PROC MIXED DATA = did_preDATA1;
	CLASS post exposed bp_med_use;
	BY bp_med_use;
	MODEL  adjusted_dia_mean=post|exposed/ SOLUTION;
	LSMEANS post|exposed / DIFF;
	ESTIMATE 'DID' exposed*post 1 -1 -1 1;
	ODS OUTPUT LSMEANS=lsmeans
		SolutionF=Fixed_effect;
RUN;

PROC EXPORT DATA=WORK.LSMEANS DBMS=CSV REPLACE
	OUTFILE="&PATHOUT\AdjDBP_bygroup.csv";
RUN;

PROC EXPORT DATA=WORK.Fixed_effect DBMS=CSV REPLACE
	OUTFILE="&PATHOUT\AdjDBP_bygroup_FE.csv";
RUN;

PROC SORT DATA=LSMEANS;
	BY bp_med_use;
RUN;

PROC SGPLOT DATA=LSMEANS;
	BY bp_med_use;
	SERIES X=post Y=estimate/ GROUP=exposed;
	SCATTER X=post Y=estimate/ GROUP=exposed;
RUN;