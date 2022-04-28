# CCM analyses

1. We extract the seasonal component of each causal variable (climatic factor). Use [SeasonalSplines.R](SeasonalSplines.R) to generate the seasonal component for all climate variables, saved to the seasonality file.
2. Then we test the variables, lagged by a certain time `tp`, for causality using surrogates. Run [CCMSplines.R](CCMSplines.R)<sup id="a1">[1](#f1)</sup> with original data and seasonality files as inputs to perform the CCM analyses with surrogates for all variables against the number of expected cases for a given time for prediction `tp` (`TimeForPred` parameter), yielding, for each variable, a value for prediction skill (![equation](https://latex.codecogs.com/gif.latex?%5Crho)) and a *p*-value, considered significant if it is below a threshold. This is repeated for a range of time lags of interest (in our case, up to 30 weeks). Each variable may or may not be significant, at several lags. We pick the significant variables/lags that have maximum prediction skill (![equation](https://latex.codecogs.com/gif.latex?%5Crho)).
3. Finally, we investigate the interaction strength for each significant driver variable at a certain lag. This is done in the file [CCMCoefficients.R](CCMCoefficients.R).


<b id="f1">1</b> Note that this function also uses `make_pred_nozero.R`. [↩](#a1)
