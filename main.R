source("scripts/prepare_raw_data.R")
source("scripts/evaluate_model.R")

# Prepare dengue data sets
prepare_set("raw_data/Dengue Training.csv",
            "data/dengue_training_set.rds")

prepare_set("raw_data/Dengue Testing.csv",
            "data/dengue_testing_set.rds")

# Prepare leptospirosis data sets
prepare_set("raw_data/Leptospirosis Training.csv",
            "data/leptospirosis_training_set.rds")

prepare_set("raw_data/Leptospirosis Testing.csv",
            "data/leptospirosis_testing_set.rds")

# Evaluate dengue
evaluate_model("dengue",
               "data/dengue_training_set.rds",
               "data/dengue_training_set.rds")

# Evaluate leptospirosis
evaluate_model("leptospirosis",
               "data/leptospirosis_training_set.rds",
               "data/leptospirosis_testing_set.rds")
