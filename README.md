# -FL_corr

The R code FL_cor calculate the federated correlation coefficient of x and y using the summary statistics from multi-data sources (k sites).
Input:
The input data should contain the (mean_x, mean_y, Var_x, Var_y, correlation (x,y)) from each source. The example data shows the format of data to be input.
Output: 
The package output the point estimate and its standard deviation of the federated correlation coefficient, which should be the same as those calculated from the aggregated data.
