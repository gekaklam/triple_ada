library(data.table)
library(dplyr)
library(ggplot2)
var_names <- c("run_number",
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
                              col.names = var_names))


dt.res.mean <- dt.res[, lapply(.SD, mean), 
                      by=list(L.ratio, U.ratio, T.length)]

dt.res.median <- dt.res[, lapply(.SD, median), 
                      by=list(L.ratio, U.ratio, T.length)]

> qplot(x=sizeof_L,
        +       y=(unlab_h1+unlab_h2+unlab_h3)/3,
        +       data=dt.tmp,
        +       main = "Unlabeled data sample, vs Labeled set size",
        +       ylab = "Average size of unlabeled sample",
        +       xlab = "Size of Labeled dataset",
        +       color=L.ratio) + geom_smooth(se=F)
> 

  > qplot(x=sizeof_L,
          +       y=precision,
          +       data=dt.tmp,
          +       main = "Precision, vs Labeled set size",
          +       ylab = "Precision",
          +       xlab = "Size of Labeled dataset",
          +       color=L.ratio) + geom_smooth(se=F) + labs(colour="Labeled \nset ratio")  
  
  