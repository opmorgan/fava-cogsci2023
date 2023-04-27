This repo contains data and analysis code for the 45th Annual Conference of the Cognitive Science Society paper, "Frequency asymmetries in vision and action."

Preregistration: https://aspredicted.org/yc8g2.pdf

# Experiment script and stimuli
The folder "inquisit_task" contains experiment scripts. Because of Inquisit Web's design, all scripts and resources are in the same, top-level directory, organized with filename prefixes.

## Scripts (prefix: scripts_)
script_main.iqx is the main executable. It calls the other files that have the prefix "script_".

## Stimuli (prefix: stimuli_)
The heirarchical shape stimuli and mask have the prefix "stimuli_". These svg files were created in inkscape by Mahek Majithia.

## Resources and instructions (prefixes: resources_, inst_)
Files with the prefixes "resources_" and "inst_" contain images and html shown as instructions, used in scripts. 'style.css" contains css to style these instructions.

## Consent
The consent form is the file "consent.html."

# Data and analysis code

Data, analysis code, and output (figures, tables, and html reports) are in the analysis/ directory. Analysis scripts are found in analysis/analyses/. The folder analyses/lib/ contains supporting functions used by analysis scripts. analysis/figures/ contains figures, and analysis/tables contains generated .tex tables. analysis/manual_cache is used to cache models and other objects that take a while to create. Raw, intermediate, and processed data are found in analyses/data/.

An additional copy of "the data" (the preprocessed data for every participant with at least one full experimental run) is available in the data/ directory. data/aah_long.tsv includes task data, with a row for every trial. data/aah_summary.tsv includes demographic and summary data, with a row for every subject.

## Pilot data and power analysis
Rmarkdown scripts used to process and analyze the pilot data (a sample of 112 right handed recruits) are found in the folder analysis/analyses/pilot/:
- 1_process.Rmd: Load and process raw data.
- 2_exclude.Rmd: Apply and describe exclusions.
- 3_analyze.Rmd: Test for the effect of field by level.
- 3b_analyze_power_[acc/rt].Rmd: Power analyses.


## Full sample data and analyses
Rmarkdown scripts used to process and analyze the pilot data (a sample of 1008 recruits) are found in the folder analysis/analyses/experiment_n1008/.
- 1_process.Rmd: Load and process raw data.
- 2_exclude.Rmd: Apply and describe exclusions.
- 3a_analyze_FL.Rmd: Test for the effect of field by level.
- 3b_analyze_FLH.Rmd: Test for the effect of field by level handedness.
- 3c_analyze_shape.Rmd: Test for the effect of shape.
- 3d_analyze_FLH_extremes.Rmd: Test for the effect of field by level by handedness, in extreme right and left handers (EHI +/-100)
- 3e_analyze_shape_FLH_extremes.Rmd: Test for the effect of field by level by handedness, in extreme right and left handers (EHI +/-100)
- 3f_analyze_demographics.Rmd: Test for group differences in age, education, and sex.
- 4a_figures_cogsci2023.Rmd: Create figures.
- The subfolder "components" includes Rmarkdown files that are sourced in primary analysis scripts.







