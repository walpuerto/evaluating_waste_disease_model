source("scripts/load_packages.r")
source("scripts/rgb_metrics/rank_box_graduation.R")
source("scripts/helper_functions.r")

set.seed(1000)

evaluate_model <- function(
    disease_name,
    training_file,
    testing_file,
    notify_progress = TRUE
) {
  # Load data
  if (notify_progress) print("Evaluation: Loading files ...")
  
  training_set <- training_file |> 
    clean_set()
  
  testing_set <- testing_file |> 
    clean_set()
  
  # Generate full models
  if (notify_progress) print("Evaluation: Generating models ...")
  untuned_model <- training_set |>
    generate_untuned()
  
  tuned_model <- training_set |>
    generate_tuned()
  
  # Calculate RGA
  if (notify_progress) print("Evaluation: Calculating RGA ...")
  rga_untuned <- untuned_model |>
    rg_a(testing_set, TRUE)
  
  rga_tuned <- tuned_model |>
    rg_a(testing_set, TRUE)
  
  # Calculate RGA significance
  if (notify_progress) print("Evaluation: Calculating RGA significance ...")
  rga_significance <- z_rga(
    untuned_model,
    tuned_model,
    testing_set
  )
  
  # Perturb testing set
  if (notify_progress) print("Evaluation: Perturbing testing set ...")
  perturbed_testing <- testing_set |> 
    perturb_set()
  
  # Calculate RGR
  if (notify_progress) print("Evaluation: Calculating RGR ...")
  rgr_untuned <- untuned_model |> 
    rg_r(testing_set, perturbed_testing, TRUE)
  
  rgr_tuned <- tuned_model |> 
    rg_r(testing_set, perturbed_testing, TRUE)
  
  # Calculate RGR significance
  if (notify_progress) print("Evaluation: Calculating RGR significance ...")
  rgr_significance <- z_rgr(
    untuned_model,
    tuned_model,
    testing_set,
    perturbed_testing
  )
  
  # Generate reduced sets (RGE)
  if (notify_progress) print("Evaluation: Generating reduced sets (RGE) ...")
  reduced_training_sets <- training_set |>
    reduce_set_rge()
  
  reduced_testing_sets <- testing_set |>
    reduce_set_rge()
  
  # Generate reduced models (RGE)
  if (notify_progress) print("Evaluation: Generating reduced models (RGE) ...")
  reduced_untuned_models <- reduced_training_sets |>
    map(generate_untuned)
  
  reduced_tuned_models <- reduced_training_sets |> 
    map(\(x) generate_custom_mtry(x, tuned_model$mtry))
  
  # Calculate RGE
  if (notify_progress) print("Evaluation: Calculating RGE ...")
  rge_untuned <- mass_calculate_rge(
    untuned_model,
    reduced_untuned_models,
    testing_set,
    reduced_testing_sets
  )
  
  rge_tuned <- mass_calculate_rge(
    untuned_model,
    reduced_tuned_models,
    testing_set,
    reduced_testing_sets
  )
  
  # Calculate RGE significance
  if (notify_progress) print("Evaluation: Calculating RGE significance ...")
  rge_untuned_significance <- mass_calculate_rge_significance(
    untuned_model,
    reduced_untuned_models,
    testing_set,
    reduced_testing_sets
  )
  
  rge_tuned_significance <- mass_calculate_rge_significance(
    untuned_model,
    reduced_tuned_models,
    testing_set,
    reduced_testing_sets
  )
  
  # Generate binarized sets
  if (notify_progress) print("Evaluation: Generating binarized sets ...")
  binarized_training_set <- training_set |> 
    binarize_set()
  
  binarized_testing_set <- testing_set |> 
    binarize_set()
  
  # Generate binarized full models
  if (notify_progress) print("Evaluation: Generating binarized models ...")
  bin_untuned_model <- binarized_training_set |>
    generate_untuned()
  
  bin_tuned_model <- binarized_testing_set |>
    generate_custom_mtry(tuned_model$mtry)
  
  # Generate reduced sets (RGF)
  if (notify_progress) print("Evaluation: Generating reduced sets (RGF) ...")
  reduced_bin_training_sets <- binarized_training_set |> 
    reduce_set_rgf()
  
  reduce_bin_testing_sets <- binarized_testing_set |> 
    reduce_set_rgf()
  
  # Generate reduced models (RGF)
  if (notify_progress) print("Evaluation: Generating reduced models (RGF) ...")
  reduced_bin_untuned_models <- reduced_bin_training_sets |> 
    map(generate_untuned)
  
  reduced_bin_tuned_models <- reduced_bin_training_sets |> 
    map(\(x) generate_custom_mtry(x, tuned_model$mtry))
  
  # Calculate RGF
  if (notify_progress) print("Evaluation: Calculating RGF ...")
  rgf_untuned <- mass_calculate_rgf(
    bin_untuned_model,
    reduced_bin_untuned_models,
    binarized_testing_set,
    reduce_bin_testing_sets
  )
  
  rgf_tuned <- mass_calculate_rgf(
    bin_tuned_model,
    reduced_bin_tuned_models,
    binarized_testing_set,
    reduce_bin_testing_sets
  )
  
  # Calculate RGF Significance
  if (notify_progress) print("Evaluation: Calculating RGF significance ...")
  rgf_untuned_significance <- mass_calculate_rgf_significance(
    bin_untuned_model,
    reduced_bin_untuned_models,
    binarized_testing_set,
    reduce_bin_testing_sets
  )
  
  rgf_tuned_significance <- mass_calculate_rgf_significance(
    bin_tuned_model,
    reduced_bin_tuned_models,
    binarized_testing_set,
    reduce_bin_testing_sets
  )
  
  # Save RGA
  if (notify_progress) print("Evaluation: Saving RGA ...")
  save_metric_unit(
    "RGA",
    rga_untuned,
    rga_tuned,
    rga_significance,
    paste0("results/", disease_name, "_rga.csv")
  )
  
  # Save RGR
  if (notify_progress) print("Evaluation: Saving RGR ...")
  save_metric_unit(
    "RGR",
    rgr_untuned,
    rgr_tuned,
    rgr_significance,
    paste0("results/", disease_name, "_rgr.csv")
  )
  
  # Save RGE (Untuned)
  if (notify_progress) print("Evaluation: Saving RGE ...")
  save_metric_vector(
    training_set,
    1:24,
    rge_untuned,
    rge_untuned_significance,
    paste0("results/", disease_name, "_rge_untuned.csv")
  )
  
  # Save RGE (Tuned)
  if (notify_progress) print("Evaluation: Saving RGE (Tuned) ...")
  save_metric_vector(
    training_set,
    1:24,
    rge_tuned,
    rge_tuned_significance,
    paste0("results/", disease_name, "_rge_tuned.csv")
  )
  
  # Save RGF (Untuned)
  if (notify_progress) print("Evaluation: Saving RGE (Untuned) ...")
  save_metric_vector(
    binarized_training_set,
    25:31,
    rgf_untuned,
    rgf_untuned_significance,
    paste0("results/", disease_name, "_rgf_untuned.csv")
  )
  
  # Save RGF (Tuned)
  if (notify_progress) print("Evaluation: Saving RGR (Tuned) ...")
  save_metric_vector(
    binarized_training_set,
    25:31,
    rgf_tuned,
    rgf_tuned_significance,
    paste0("results/", disease_name, "_rgf_tuned.csv")
  )

}
