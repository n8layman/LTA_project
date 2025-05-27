#' Grid search for a tidymodels workflow using targets dynamic branching
#'
#' This function performs grid search tuning for a machine learning workflow 
#' using cross-validation. It iterates over provided folds and grid search 
#' parameters and computes specified evaluation metrics (e.g., AUC, F1 score) 
#' and profiles memory usage and timing for each model fit.
#' 
#' @author Nathan Layman
#'
#' @param workflow # A tidymodels workflow with recipe and model already attached
#' @param gridsearch_params # A tibble where each row is a set of hyperparameters
#' @param training_data_folds # A tibble where each row is a training data fold
#' @param metrics # A set of metrics produced by `yardstick::metric_set`
#'
#' @return Returns a tibble with fit performance metrics, fit time, and the ram used while fitting
#'
#' @examples
#' \dontrun{
#' performance <- tune_grid_branch(workflow, gridsearch_params, training_data_folds, verbose = TRUE)
#' }
#'
#' @export
tune_grid_branch <- function(workflow, 
                             gridsearch_params, 
                             training_data_folds, 
                             metrics = metric_set(pr_auc,           # Precision-Recall AUC
                                                  roc_auc,          # ROC AUC
                                                  accuracy,         # Accuracy
                                                  f_meas,           # F1 Score
                                                  recall,           # Recall
                                                  precision),
                             verbose = F) {
  
  
  # Look into geo_targets
  # Sample splits how to push them forward as targets branches. Targetize 
  # For helper that will convert X into Y these are the things that could be in there
  # For now just did this. Free to implenet. 
  
  # Get the performance and profiling metrics of every combination of 
  # data fold and hyper-parameter combination passed in to the function
  performance <- map_dfr(1:nrow(training_data_folds), function(i) {
    map_dfr(1:nrow(gridsearch_params), function(j) {
      
      rsamp <- rsample::manual_rset(training_data_folds[i,]$splits, training_data_folds[i,]$id)
      params <- gridsearch_params[j,]
      if(verbose) print(rsamp |> bind_cols(params) |> select(-splits))
      
      # Grab start time
      start_time <- Sys.time()
      
      # Fit the model against the training data and profile memory usage
      # Using tune_grid here but we could make this simpler and just
      # fit and evaluate the model manually
      mem_usage_bytes <- profmem::profmem({
        fold_param <- tune::tune_grid(workflow,
                                      resamples = rsamp,
                                      grid = params,
                                      metrics = metrics)
      })
      
      # Report fit performance metrics
      fold_param |> select(-splits) |>
        mutate(id = rsamp$id,
               branch = targets::tar_name(),
               mem_usage_bytes = sum(mem_usage_bytes$bytes, na.rm=T),
               fit_time = Sys.time() - start_time)
    })
  })
  
  # Clean up environment in case targets tries to store extra stuff
  rm(list=setdiff(ls(), "performance"))
  
  # Return performance
  performance
}

#' Select Best Parameters from Tuned Model Results
#'
#' This function extracts the best parameters from a tuned model's metrics based on a specified evaluation metric. 
#' It calculates the average of the specified metric across tuning folds and selects the parameters with the 
#' minimum value of the specified metric (e.g., "roc_auc"). Unnecessary columns such as splits, IDs, and memory usage are removed.
#'
#' @author Nathan Layman
#'
#' @param tuned A tibble containing the results of the tuning process, including the model metrics.
#' @param metric A character string specifying the evaluation metric to be used for selecting the best parameters. 
#' The default is `"roc_auc"`.
#'
#' @return A tibble containing the best parameters, excluding unnecessary columns such as `.estimate`, `mem_usage`, and any matching branches.
#' 
#' @details The function first unnests the `.metrics` column of the `tuned` tibble, filters by the selected metric, 
#' and calculates the mean of the evaluation metric for each set of parameters. It then selects the parameters 
#' that minimize the metric, without ties.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' best_params <- select_best_params(tuned_model_results, metric = "roc_auc")
#' }
#'
#' @export
select_best_params <- function(tuned, metric = "roc_auc") {
  
  best_params <- tuned |> 
    unnest(.metrics) |> 
    filter(.metric == metric) |> 
    select(-splits, -id, -starts_with("."), .estimate) |>
    group_by(across(-.estimate)) |>
    summarize(.estimate = mean(.estimate), .groups = "drop") |>
    slice_min(.estimate, with_ties = F) |>
    select(-.estimate, -starts_with("mem_usage"), -matches("branch"))
  
  return(best_params)
}
