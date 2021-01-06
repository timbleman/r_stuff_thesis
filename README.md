## Preliminaries
The R scripts included in this repository are used to evaluate data created for my bachelor thesis.
The data has been created using https://github.com/timbleman/asfault_evaluator. 
This repository is able to calculate similaries between roads and driver output and performs adaptive random sampling.
The folders /div_bng5_only_results/ and /div_drvr5_only_results/ contain adaptive random sampling subsets created for the thesis.
The others (/driver-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200818T120651Z-001/, /experiments-beamng-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200821T084856Z-001/,
/experiments-driver-ai-no-obe-wo-minlen-wo-infspeed/, /experiments-beamng-ai-no-obe-wo-minlen-wo-infspeed/) contain csv files created for 
DriverAI and BeamNG.AI datasets with or without OBE-tests (Out of Bound Episodes).  

## Prerequisites
This list includes the R version and the libraries I used for evaluating my thesis.
* R 4.0.2
* ggplot2
* hrbrthemes
* vegan
* egg
* cowplot
* dplyr
* reshape2
* fields
* xtable

## Usage
Select one of the scripts from the list below.
Configure the script according to the comments.
Make sure that the paths are correct and the right names for loading csv files are used.
Run the script. 

## The scripts and their purpose
Over the course of writing my bachelor thesis, the evaluation performed in R grew much bigger than I anticipated.
Many new experiments were added with time, others were discarded or the plotting was significantly changed.
For this reason, the repository is quite chaotic. The following list should give you an idea what scripts should be used to create which plots.
Each script contains further explanation and instructions what can and should be configured. 
The research questions that these plots were used in are names from RQ1 to RQ4.

* Estimations of correlation: Here I try to estimate correlation by looking at certain ratios in a neighborhood of close tests.
	* nearest_neighbors_in_out_metric.R: Plots the ratio of neighbors found in the close output as well as in the close input. Used heavily in RQ2.
	* nearest_neighbors_non_obes.R: Plots the ratios of OBEs in close neighborhoods of non-OBE tests. Used in RQ1.
	* nearest_neighbors_obes.R: Deprecated, replaced by nearest_neighbors_obes_3metrics.R.
	* nearest_neighbors_obes_2metrics.R: Deprecated, replaced by nearest_neighbors_obes_3metrics.R.
	* nearest_neighbors_obes_3metrics.R: Plots the ratios of OBEs in close neighborhoods of OBE tests. Is able to plot 1, 2 or 3 metrics at the same time. Used heavily in RQ1.
* Boxplots of similartiy values:
	* boxplots_2_siliarities.R: Draws box plots for all similarity values of two metrics
	* boxplots_3_siliarities.R: Draws box plots for all similarity values of three metrics
* Evaluation preliminaries: Here R was only used to plot values I collected elsewhere.
	* alphabet_times_comp.R: Time needed to compute similarity score for different metrics and alphabet sizes. In the Evaluation chapter 5.2.
	* stacked_bar_plot_datasets.R: Stacked bar plots of full and trimmed dataset sizes. Thesis Evaluation --> The datasets.
	* stacked_bar_plot_broken.R: Stacked bar plots, shows which tests have been removed. Thesis Evaluation --> The datasets.
* Investigation of bins:
	* obe_vs_non_obe_coverages.R: Plots the development of coverage metrics for OBE and non-OBE tests. Is able to remove bins by an absolute or percentage based threshold. Used in RQ3.
	* print_bins_abs_perc.R: Plots bins as heatmaps. Is able to remove bins by an absolute or percentage based threshold. Used in RQ3.
	* rand_samp_subset_obe_and_cov.R: Outputs averaged out coverage and OBE count of a selection of smaller subsets. Used in RQ3. 
* Investigation of adaptive random sampling: 
	* avg_dist_art.R: Calculates the average similarity for a number of sambled subsets. Used in RQ4.
	* sampled_subset_coverage_development.R: Cumulative plots for the growth of subsets using adaptive random sampling. Used in RQ4.
* Unused experiments: These are experiments that did not make it into the final thesis. There are three main reasons: Either the plots produced were not interesting enough, inconclusive or I encountered errors that could not be fixed in the time frame of the thesis.
	* boxplots_states_obe_vs_non_obe.R: Box plots of the number of states in OBE and non-OBE tests. 
	* in_out_scatterplot.R: Scatterplot of output similarities of neighbors on the y-axis, input distance on the x-axis. Has one tests as center point.
	* Classical correlation metrics: Due to the low correlation values, these were not used heavily or at all in my thesis.
		* obe_correlation_calculator.R: Calculates Spearman correlation between a selected set of similarity metrics and the test outcome.
		* mantel_calculator.R:
		* obe_correlation_calculator_threshold.R: Like obe_correlation_calculator.R, but only used tests at a similarity between 1.0 and a certain threshold.
	* smaller_input_smaller_output_manual.R: Looks whether closer tests in the input are also closer in output. Did not work out.
* file_renamer.py: Renames the similarity matrices. jaccard_7ang_4len --> jaccard_28alph.



