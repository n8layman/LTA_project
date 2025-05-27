################################################################################
#
# Example project build script
#
################################################################################

# Load libraries in packages.R project-specific functions in R folder ----------
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)

# Set targets options ----------------------------------------------------------
source("_targets_settings.R")

# Set build options ------------------------------------------------------------


# Groups of targets ------------------------------------------------------------


## Data input
data_input_targets <- tar_plan(
  
  # Read in prevalence and site characteristic data
  tar_file_read(prevalence_data, here::here("data/WABNet_CoV_prevalence_9July2024.csv"), read_csv(!!.x)),
  tar_file_read(site_characteristics, here::here("data/Site_characterization_information.csv"), read_csv(!!.x)),
  
  # Specify the response variable to use
  tar_target(response_variable, "co_v_final_results"),
  
  # Read in a list of which variables to use as explanatory variables. Edit this to change model specifications
  tar_file_read(explanatory_variables, here::here("data/explanatory_variables.csv"), 
                read_csv(!!.x) |> filter(include == T)),
)

## Data processing
data_processing_targets <- tar_plan(
  
  # Plot site characteristic variance-covariance matrix
  tar_target(site_cov_matrix, get_corr_matrix(site_characteristics)),
  
  # Use fuzzy join in case of minor spelling errors between Site names
  tar_target(analysis_data, fuzzyjoin::stringdist_join(prevalence_data, site_characteristics, by = "Site", max_dist = 2, method = "jw") |> 
               janitor::clean_names() |>
               filter(co_v_final_results != "na") |>
               select_at(c(response_variable, explanatory_variables$var))),
  
  # Split the data into training and test splits
  tar_target(analysis_split, analysis_data |>
               initial_split()),
  
  # Extract the training split
  tar_target(analysis_data_train, training(analysis_split)),
  
  # Extract the test split
  tar_target(analysis_data_test, testing(analysis_split)),
  
  # Set up cross validation folds
  tar_target(training_data_folds, vfold_cv(analysis_data_train)),
  
  # Set up the recipe and formula
  # By using step_dummy here instead of manually
  # making dummy variables DALEX can evaluate the
  # importance of each variable BEFORE it gets dummified.
  # Which is pretty nifty.
  tar_target(analysis_recipe, recipe(paste(response_variable, "~ .") |> as.formula(), data = analysis_data_train) |>
               step_naomit() |>
               step_string2factor(all_string()) |>
               step_novel(all_nominal(), -all_outcomes()) |>
               step_dummy(all_nominal(), -all_outcomes()) |>
               step_zv(all_predictors())),
)

## BART analysis
bart_analysis_targets <- tar_plan(
  
  # Set up the BART model
  tar_target(bart_model, 
             parsnip::bart(trees = tune(),
                           prior_terminal_node_coef = tune(),
                           prior_terminal_node_expo = tune()) |> 
               set_engine("dbarts") |>
               set_mode("classification")),
  
  # Set up the BART model workflow
  tar_target(bart_workflow, workflow() |> 
               add_recipe(analysis_recipe) |> 
               add_model(bart_model)),
  
  # Set up the hyper-parameter grid search.
  # Automatically extract the parameters to tune across.
  # From the workflow.
  tar_target(bart_gridsearch, bart_workflow |> 
               extract_parameter_set_dials() |>
               dials::grid_latin_hypercube(size = 10),
             iteration = "vector"),
  
  # Go out of tidymodels to utilize dynamic branching in targets
  # to tune the model. This works because we have a lot of cores
  # on the server and the pre-processing steps are not too intensive
  tar_target(bart_tuned, tune_grid_branch(workflow = bart_workflow, 
                                          params = bart_gridsearch, 
                                          training_data_folds = training_data_folds),
             pattern = cross(bart_gridsearch, training_data_folds)),
  
  tar_target(bart_tuned_file, "data/logistic_en_tuned.RDS.gz"),
  tar_target(bart_tuned_local, write_rds(bart_tuned,
                                         file = bart_tuned_file,
                                         compress = "gz"),
             cue = tar_cue(re_tune_models)),
  tar_file_read(bart_tuned_loaded, bart_tuned_file, read_rds(!!.x)),
  
  # Extract the best set of hyper-parameters
  # metric = "pr_auc" due to unbalanced response groups 
  # Precision-Recall AUC (PR AUC) measures the area under the precision-recall 
  # curve, which evaluates a model's performance in detecting positive disease 
  # cases. It is especially valuable for imbalanced datasets, as it 
  # highlights how well the model identifies the positive class.
  tar_target(bart_best_params, select_best_params(bart_tuned, metric = "roc_auc")),
  
  # Setup the final fit workflow using the best hyper-parameters
  # identified during model tuning.
  tar_target(bart_final_workflow, bart_workflow |>
               finalize_workflow(bart_best_params)),
  
  # Fit the final model on the best set of hyper-parameters and evaluate against the test data
  tar_target(bart_final_fit, bart_final_workflow |>
               last_fit(split = analysis_split)),
  
  # DALEX explainer to evaluate variable importance
  tar_target(bart_explainer, DALEXtra::explain_tidymodels(extract_workflow(bart_final_fit),
                                                          data = analysis_data_train |> select(-any_of(response_variable)),
                                                          y = analysis_data_train |> pull(response_variable) == "Positive"
  )),
  
  # Get variable importance
  # Get variable importance
  tar_target(bart_vi, DALEX::model_parts(explainer = bart_explainer, label = "BART")),
  
)

## BART analysis output figures
bart_output_targets <- tar_plan(
  
  # Extract final fit metrics
  tar_target(bart_final_fit_metrics, collect_metrics(bart_final_fit)),
  
  # Extract final fit predictions
  tar_target(bart_final_fit_predictions, collect_predictions(bart_final_fit)),
  
  # Plot ranking of the importance of explanatory variables
  tar_target(bart_vi_plot, plot(bart_vi) +
               ggtitle("Mean variable-importance over 100 permutations", "") +
               theme(plot.title = element_text(hjust = 0.5, size = 20),
                     axis.title.x = element_text(size = 15),
                     axis.text = element_text(size = 15))),
  
)

## Report
report_targets <- tar_plan(
  
  # tar_render(
  #   example_report, path = "reports/example_report.Rmd", 
  #   output_dir = "outputs", knit_root_dir = here::here()
  # )
  
  # In order to call tar_mermaid inside the readme.Rmd format must be "file" 
  # and repository must be "local" so no tar_render().
  # Monitor both the readme rmd and md files. If either changes remake the readme
  tar_target(readme_rmd, "README.Rmd", format = "file"),
  # tar_target(readme, rmarkdown::render(readme_rmd), format = "file", repository = "local"),
)


# List targets -----------------------------------------------------------------

list(
  data_input_targets,
  data_processing_targets,
  bart_analysis_targets,
  bart_output_targets,
  report_targets
)
