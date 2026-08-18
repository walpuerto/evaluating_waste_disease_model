clean_set <- function(file_name) {
  file_name |>
    read_rds() |> 
    clean_names() |> 
    drop_na() |> 
    select(-name)
}

generate_untuned <- function(training_set) {
  randomForest(
    x = select(training_set, -cumulative_cases),
    y = training_set$cumulative_cases
  )
}

generate_tuned <- function(training_set) {
  tuneRF(
    x = select(training_set, -cumulative_cases),
    y = training_set$cumulative_cases,
    doBest = TRUE
  )
}

generate_custom_mtry <- function(training_set, custom_mtry) {
  randomForest(
    x = select(training_set, -cumulative_cases),
    y = training_set$cumulative_cases,
    mtry = custom_mtry
  )
}

perturb_set <- function(set) {
  set |> 
    mutate(
      across(
        c(
          total_waste_produced_per_day,
          total_waste_received_by_slf,
          biodegradable_waste_produced_per_day,
          recyclable_waste_produced_per_day,
          residual_waste_produced_per_day,
          special_waste_produced_per_day
        ),
        \(x) x + 10000 * rnorm(length(x))
      )
    )
}

reduce_set <- function(set, range) {
  map(range, \(i) select(set, -any_of(i)))
}

reduce_set_rge <- function(set) {
  # Exclude cumulative cases
  reduce_set(set, 1:(length(set |> select(-cumulative_cases))))
}

reduce_set_rgf <- function(set) {
  # Only reduce the binarized columns
  reduce_set(set, (length(set) - 6):length(set))
}

mass_wrapper <- function(
    full_model,
    reduced_models,
    testing_set,
    reduced_testing_sets,
    base_function
) {
  map2_dbl(
    reduced_models,
    reduced_testing_sets,
    function (reduced_model, reduced_testing_set) {
      base_function(full_model, reduced_model, testing_set, reduced_testing_set)
    }
  )
}

mass_calculate_rge <- function(
    full_model,
    reduced_models,
    testing_set,
    reduced_testing_sets
) {
  mass_wrapper(
    full_model, 
    reduced_models,
    testing_set,
    reduced_testing_sets,
    rg_e)
}

mass_calculate_rge_significance <- function(
    full_model,
    reduced_models,
    testing_set,
    reduced_testing_sets
) {
  mass_wrapper(
    full_model,
    reduced_models,
    testing_set,
    reduced_testing_sets,
    z_rge
  )
}


binarize_set <- function(set) {
  set |> 
    mutate(
      slf_site_aloguinsan = if_else(slf_site_operational=="Aloguinsan", 1, 0),
      slf_site_none = if_else(slf_site_operational=="None", 1, 0),
      slf_site_asturias = if_else(slf_site_operational=="Asturias", 1, 0),
      slf_site_balamban = if_else(slf_site_operational=="Balamban", 1, 0),
      slf_site_consolacion = if_else(slf_site_operational=="Consolacion", 1, 0),
      slf_site_pinamungajan =
        if_else(slf_site_operational=="Pinamungajan", 1, 0),
      slf_site_talisay = if_else(slf_site_operational=="Talisay", 1, 0)
    ) |> 
    select(-slf_site_operational)
}

mass_calculate_rgf <- function(
    full_model,
    reduced_models,
    testing_set,
    reduced_testing_sets
) {
  mass_wrapper(
    full_model,
    reduced_models,
    testing_set,
    reduced_testing_sets,
    rg_f
  )
}

mass_calculate_rgf_significance <- function(
    full_model,
    reduced_models,
    testing_set,
    reduced_testing_sets
) {
  mass_wrapper(
    full_model,
    reduced_models,
    testing_set,
    reduced_testing_sets,
    z_rgf
  )
}

save_metric_unit <- function(metric_name, untuned, tuned, z, save_file) {
  metric_untuned <- paste(metric_name, "untuned")
  metric_tuned <- paste(metric_name, "tuned")
  
  tibble(
    metric = c(metric_untuned, metric_tuned, "Z"),
    value = c(untuned, tuned, z)
  ) |> 
    write_csv(save_file)
}

save_metric_vector <- function(set, range, untuned, z, save_file) {
  column_names <- set |>
    select(range) |> colnames()
  
  tibble(
    variables = column_names,
    metric = untuned,
    Z = z,
  ) |>
    write_csv(save_file)
}
