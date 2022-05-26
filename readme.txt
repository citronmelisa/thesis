All of this code is code made for my thesis: Change point in mean detection for structural health monitoring data obtained from vibration based methods.

In different disciplines of engineering it is often paramount to be able to unambiguously identify change points in experimentally obtained data. 
One such example is civil engineering while monitoring structural health, where a change point can be a signal of structural change or damage to the structure. 
In this work the application of three different algorithms for structural health monitoring with vibration based methods data with the goal of detecting change points in mean values is analysed. 
In the scope of this work the robustness of the obtained results is evaluated and recommendations for the use of analysed algorithms in the industry are provided. 
As the basis for this work, experimental data obtained while monitoring the health of a model structure under the influence of environmental effects has been used. 
The resulting preliminary data is a set of six-dimensional time series which consist of resonance frequency changes in time.

In this repository you will find:

folder containing written code (code)
	pca_for_cp_analysis.R -- contains code where PCA model is made on data and ERMS data obtained and cp detected by using a treshold.
	robts.R -- contains code where Hodge-Lehmann test is run on previously obtained ERMS data
	CUSUM_controls_for_erms -- contains code where CUSUM is run on previously obtained ERMS data. 
	ELmeans_cpts -- not used. 
folder containing used data (data_2703)
folders containing obtained figures in analysis (all_erms_jpegs, erms_robts_cusum_jpegs, erms_wilcox_jpegs, cusum_qcc_jpegs, HL_erms_jpegs)

