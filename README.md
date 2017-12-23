# groupedData

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%CHS_frailty%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

“baselineData.R”: This script constructs a data frame of baseline variables for the Cardiovascular Health Study (CHS) participants. The baseline variables include self-reported age at baseline, gender (male or female), smoking (never, former, or current), C-reactive protein level, and education history.

“frail.R”: This script constructs a data frame of outcome variables (delta, V) for the CHS participants. The variable delta is an indicator of whether or not the participant was observed to be frail. If delta = 1, V is the first visit at which frailty was observed for the participant. If delta = 0, V is the last visit at which the participant’s frailty status was measured. 

“analysis.R”: This script merges the data frames that were constructed by the other two scripts into a single data frame with baseline and outcome variables. Age and C-reactive protein level are standardized. Smoking and education are simplified to binary variables. We compute regression coefficient and standard error estimates from the Cox-Laplace, Cox-Analytic, Cox-Efron, Cox-Breslow, and Prentice-Gloeckler methods. Also, we plot the marginal distributions of the baseline variables.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%genericCode%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

“allCode.R”: This script has the function applyMethods() that takes in a data set as input and outputs the regression coefficient and standard error estimates by the Cox-Laplace, Cox-Analytic (nicknamed as Felix), Cox-Efron, Cox-Breslow, and Prentice-Gloeckler (PG) methods. This function is used in the simulation studies and CHS data application.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Simulations%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

“SingleCovariate/varyBeta” folder: In the “code” subfolder, we have the scripts for the Single Covariate simulation studies. Each script corresponds to a single simulation study; the title of the script indicates the unique setting of Beta and n in the simulation study. The results from running the scripts are in the “results” subfolder. The results include the regression coefficient estimates, standard error estimates, and number of ties per visit, in each simulation. There are 10,000 simulations per simulation study.

“CHS_frailty” folder: In the “code” subfolder, we have the scripts for the Data-Based simulation studies. Each script corresponds to a single simulation study; the title of the script indicates the unique setting of n in the simulation study. The results from running the scripts are in the “results” subfolder. The results include the regression coefficient estimates, standard error estimates, and number of ties per visit, in each simulation. There are 10,000 simulations per simulation study.

“processResults” folder: The scripts of this folder are described below.

- The “processingResults.R” script has two functions that are used frequently in the “CHS_frailty.R” and “SingleCovariate_varyBeta.R” scripts. The estimator() function is used to compute the bias, standard error, and root-mean-square error of the Cox-Laplace, Cox-Analytic, PG, Cox-Efron, and Cox-Breslow regression coefficient estimators. The ci() function is used to compute the coverage probability of the 95% confidence interval constructed by each method.

- The “nties.R” script constructs LaTeX tables of the average number of ties for each follow-up visit, for each simulation study.

- In the “CHS_frailty.R” script, we compute the following for each Data-Based simulation study: for each covariate (i.e., age, gender, CRP, education, smoking), the bias, standard error, and root-mean-square error of the regression coefficient estimator and the coverage probability of the 95% confidence interval for the regression coefficient. This is done for each of the methods (i.e., Cox-Laplace, Cox-Analytic, PG, Cox-Efron, and Cox-Breslow).

- In the “SingleCovariate_varyBeta.R” script, we compute the following for each Single Covariate simulation study: the bias, standard error, and root-mean-square error of the regression coefficient estimator and the coverage probability of the 95% confidence interval for the regression coefficient. This is done for each of the methods (i.e., Cox-Laplace, Cox-Analytic, PG, Cox-Efron, and Cox-Breslow).
