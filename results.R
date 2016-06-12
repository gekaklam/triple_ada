library(data.table)
library(dplyr)
library(ggplot2)
var_names <- c("run_number", "run_number",
               "true_pos", "true_neg",
               "false_pos", "false_neg",
               "normal", "attacks",
               "perc.norma","perc.attac",
               "correct", "wrong",
               "accuracy", "precision",
               "recall", "f_measure",
               "g_mean", "false_alrm",
               "det_rate", "sizeof_L",
               "sizeof_U", "unlab_h1",
               "unlab_h2", "unlab_h3",
               "tritr.iter", "auc",
               "time.train", "time.pred",
               "L.ratio", "U.ratio",
               "T.length"
               )

dt.res <- data.table(read.csv(file = "results_all.csv", 
                              header = FALSE,
                              col.names = var_names)
                     )

