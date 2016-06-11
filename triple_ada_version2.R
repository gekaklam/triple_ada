library("ada")
library("data.table")
library("dplyr")
library("splitstackshape")
library("ROCR")
#Rprof(filename = "Rprof.out", memory.profiling = TRUE )

##################################
# Start of preprocessing section #
##################################

var_names <- c("duration","protocol_type",
               "service","flag","src_bytes",
               "dst_bytes","land","wrong_fragment",
               "urgent","hot","num_failed_logins",
               "logged_in","num_compromised","root_shell",
               "su_attempted","num_root","num_file_creations",
               "num_shells","num_access_files","num_outbound_cmds",
               "is_host_login","is_guest_login","count",
               "srv_count","serror_rate","srv_serror_rate",
               "rerror_rate","srv_rerror_rate","same_srv_rate",
               "diff_srv_rate","srv_diff_host_rate","dst_host_count",
               "dst_host_srv_count","dst_host_same_srv_rate",
               "dst_host_diff_srv_rate", "dst_host_same_src_port_rate",
               "dst_host_srv_diff_host_rate","dst_host_serror_rate",
               "dst_host_srv_serror_rate","dst_host_rerror_rate",
               "dst_host_srv_rerror_rate", "type")

col.classes <-  c("land"="factor",
                  "logged_in"="factor",
                  "is_host_login"="factor",
                  "is_guest_login"="factor")


dt.train <- data.table(read.csv("data_cleaned.csv", header = FALSE,
                             col.names = var_names,
                             colClasses = col.classes))

dt.test <- data.table(read.csv("data_test_cleaned.csv", header = FALSE,
                              col.names = var_names,
                              colClasses = col.classes))


# Remove data from train / test datasets, which levels don't exist on
# both of them. (~20)  #TODO add exact number


dt.test <- dt.test[dt.test[,service != "icmp"]]
dt.test <- dt.test[dt.test[,is_host_login != "1"]]
dt.train <- dt.train[dt.train[,service != "red_i"]]
dt.train <- dt.train[dt.train[,service != "urh_i"]]

# And their levels

dt.train$service <- droplevels(dt.train$service)
dt.test$service <- droplevels(dt.test$service)
dt.test$is_host_login <- droplevels(dt.test$is_host_login)
  
# Convert all entries that don't have a type normal,
# to an attack "2" and those which are normal to "1".

dt.train[(type!="normal")]$type <- "2"     
dt.train[(type=="normal")]$type <- "1"

dt.test[(type!="normal")]$type <- "2"     
dt.test[(type=="normal")]$type <- "1"

# Drop the unused levels from the type column
dt.train$type <- droplevels(dt.train$type)
dt.test$type <- droplevels(dt.test$type)

#############################
# End of preprocessing part # 
#############################

###########################
# Start of Functions Part #
###########################

MeasureError <- function(h_i, i) {
  h_j <- h_i[[1]]
  h_k <- h_i[[2]]
  d_t1 <- data.table(cbind(h_j$actual, h_j$fit))
  d_t2 <- data.table(cbind(h_k$actual, h_k$fit))
  n_inc <- sum(d_t1$V1 != d_t1$V2) + sum(d_t2$V3 != d_t2$V4)
  n_cor <- sum(d_t1$V1 == d_t1$V2) + sum(d_t2$V3 == d_t2$V4)
  er_i <- n_inc / n_cor
  #  er_i <- n_inc / (sum(dim(d_t1)[1] +dim(d_t2)[1]))
  return(er_i)
}

tri_train <- function(L, U, results.table, dbug = FALSE){
  train.start <- Sys.time()
  # Set the type for the three classifiers
  type_i <- c("discrete", "real", "gentle")
  # Create an empty placeholder for the classifier objects
  h_i <- list(0, 0, 0)
  L.sample_i <- list(0,0,0)
  
  # Create the stumps
  stump <- rpart.control(cp = -1, maxdepth = 1, minsplit = 0)
  
  # Train the three classifiers
  for (i in 1:3) {
    L.sample_i[[i]] <- stratified(L, group = "type",
                                  size = 1/3, replace = TRUE)
    
    h_i[[i]] <- ada(type~., data = L.sample_i[[i]],
                    iter = 50, loss="e",
                    type = type_i[i], control = stump )
  }
  
  # Initialize variables
  e.n_i    <- c(0.5, 0.5, 0.5)
  l.n_i    <- c(0, 0, 0)
  L_i      <- list(0, 0, 0)
  update_i <- c( FALSE, FALSE, FALSE)
  e_i      <- c(0,0,0)
  iter     <- 1

  unlbled.used <- data.frame("h1" = numeric(10),
                             "h2" = numeric(10),
                             "h3" = numeric(10))
   
  repeat {
    if (dbug) cat("########################\n")
    if (dbug) cat("# Start of iteration", iter, "#\n")
    if (dbug) cat("########################\n\n")
    
    for (i in 1:3) {
      L_i[[i]] <- 0
      update_i[i] <- FALSE
      e_i[i] <- MeasureError(h_i[-i], i)
      if (dbug) cat("Measured Error for", i, "is: \t",e_i[i],"\n")
      if (dbug) cat("Previous Error for", i, "was: \t",e.n_i[i],"\n\n")
      if (e_i[i] < e.n_i[i]){
        if (dbug) cat("Meassured Error is smaller than previous error. \n")
        if (dbug) cat("Unlabeled data size is:", dim(U)[1])
        if (dbug) cat("\n")
        #      cat("Inside the first if","\n")
        #      cat("\n")
        # Here we name pred_1 = type, so that it is easier later in the rbind
        type   <- predict(h_i[-i][[1]], U)
        pred_2 <- predict(h_i[-i][[2]], U)
        
        d_t <- data.table(cbind(U, type))
        agree <- type == pred_2
        if (dbug) cat("Number of predictions that classifier agree:", sum(agree), "\n")

        L_i[[i]] <- d_t[agree,]
        
        if (l.n_i[i] == 0) {
          if (dbug) cat("Previous length of L_",i,"was 0", sep = "")
          if (dbug) cat("\n")
          if (dbug) cat("Classifier:",i, "has not been updated before.\n")
          
          l.n_i[i] <- floor( e_i[i] / ( e.n_i[i] - e_i[i] ) + 1 )
          if (dbug) cat("Now length of L_",i, " is set to: \t", l.n_i[i],"\n", sep = "")
        }
        if (dbug) cat("\n")
        L.len <- dim(L_i[[i]])[1]
        if (dbug) cat("Length of tmp_labeled set, for", i, "is now", L.len,"\n")
        if (dbug) cat("\n")
        
        if (l.n_i[i] < L.len ) {
          if (dbug) cat("Previous length of tmp_labeled set, is less than new length. \n")
          #cat("Inside third if. \n")
          #cat("ln_i[",i,"] is:",l.n_i[i],"and L.len:",L.len, "\n")
          #cat("\n")
          #cat("e_i[i]*L.len", e_i[i]*L.len,"e.n_i[i]*l.n_i[i]",e.n_i[i]*l.n_i[i])
          #cat("update",i,update_i[i])
          #cat("l.n_i",i,"=", l.n_i[i])
          #cat("(e_i[i] / (e.n_i[i] - e_i[i]))", (e_i[i] / (e.n_i[i] - e_i[i])))
          if (e_i[i]*L.len < e.n_i[i]*l.n_i[i]) {
            if (dbug) cat("Eq 9, is not violated \n")
            if (dbug) cat("Classifier: ", i," is set to be updated. \n", sep = "")
            
            # cat(e_i[i]*L.len, "is less than", e.n_i[i]*l.n_i[i], "\n")
            
            update_i[i] <- TRUE
            
            # print(c("update",i,update_i[i]))
            # cat("l[",i,"] is:", l.n_i[i], "\n")
            if (dbug) cat("\n")
          } else if ( l.n_i[i] > (e_i[i] / (e.n_i[i] - e_i[i])) ) {
            if (dbug) cat("Need to subsample tmp_labeled set. \n")
            
            s.len <- ceiling( (e.n_i[i]*l.n_i[i]) / e_i[i] - 1)
            
            if (dbug) cat("Size of sample is equal to:", s.len, "\n")
            
            # Set the number of unlabeled data used to pass to next function
            
            s.idx <- sample(1:L.len, s.len)
            
            L_i[[i]] <- L_i[[i]][s.idx,]
            
            if (dbug) cat("Classifier:", i, "is set to be updated. \n")
            
            update_i[i] <- TRUE
            if (dbug) cat("\n")
          }
        }
      }
    }
    if (dbug) cat("The classifiers to be updated are:",update_i,"\n\n")
    for (i in 1:3) {
      if (update_i[i] == TRUE) {
        if (dbug) cat("Now updating:", i, "\n")
        if (dbug) cat("\n")
        
        if (dbug) cat("Dim, Train",i, "is:\t", dim(L.sample_i[[i]])[1], "\n")
        
        if (dbug) cat("Dim, L_",i," is:\t\t ", dim(L_i[[i]])[1], "\n", sep = "")
        
        if (dbug) cat("Dim, combined is: \t",  dim(L_i[[i]])[1]+dim(L.sample_i[[i]])[1], "\n")
        
        unlbled.used[iter,i] <- dim(L_i[[i]])[1]
        
        h_i[[i]] <- ada(type~. ,
                        data = rbind(L.sample_i[[i]],L_i[[i]]),
                        iter = 50, loss="e",
                        type = type_i[i],
                        control = stump )
        
        if (dbug) cat("Re-trained classifier with", h_i[[i]]$dim[1],"data points. \n")
        if (dbug) cat("\n")
        e.n_i[[i]] <- e_i[[i]]
        l.n_i[[i]] <- dim(L_i[[i]])[1]
      } 
    }
    if (dbug) cat("# of iterations:",iter, "\n")
    if (update_i[1] != TRUE &
        update_i[2] != TRUE &
        update_i[3] != TRUE) {
      break
    }
    else if (iter > 10) {
      break
    }
    if (dbug) cat("######################\n")
    if (dbug) cat("# End of iteration", iter, "#\n")
    if (dbug) cat("######################\n\n")
    iter <- iter + 1
  }
  
  
  train.end      <- Sys.time()
  train.elapsed  <- train.end - train.start
  unlab_h1 <- sum(unlbled.used[1:iter,1])/iter
  unlab_h2 <- sum(unlbled.used[1:iter,2])/iter
  unlab_h3 <- sum(unlbled.used[1:iter,3])/iter
  
  results.table$unlab_h1[r]   <<- unlab_h1
  results.table$unlab_h2[r]   <<- unlab_h2
  results.table$unlab_h3[r]   <<- unlab_h3
  results.table$tritr.iter[r] <<- iter
  results.table$time.train[r] <<- as.numeric(train.elapsed)
  return(h_i)
  }

tri_pred <- function(tst.dset, h_i, results.table) {
  pred.start <- Sys.time()
  tprd.1 <- as.integer(predict(h_i[[1]], tst.dset))
  tprd.2 <- as.integer(predict(h_i[[2]], tst.dset))
  tprd.3 <- as.integer(predict(h_i[[3]], tst.dset))
  tprd.agr <- tprd.1 + tprd.2 + tprd.3
  tprd.agr <- as.data.table(tprd.agr)
  tprd.agr[, "p" := ifelse( (get("tprd.agr") == 3) | (get("tprd.agr") == 4), 1,2 )]
  pred.end <- Sys.time()
  pred.elapsed <- pred.end - pred.start
  
  results.table$time.pred[r] <<- as.numeric(pred.elapsed) 
  return(tprd.agr$p)
}

cmp_lbls <- function(pred.lbls, test.lbls,
                     results.table, dbug=FALSE,
                     batch=FALSE) {
  num.normal <- sum(test.lbls == 1)
  num.attack <- sum(test.lbls == 2)
  nrm.rat <- num.normal / (num.normal + num.attack)
  att.rat <- num.attack / (num.normal + num.attack)
  true.pos   <- sum((pred.lbls ==1) & (test.lbls == 1))
  true.neg   <- sum((pred.lbls ==2) & (test.lbls == 2))
  false.pos  <- sum((test.lbls == 1) & ( pred.lbls ==2 ))
  false.neg  <- sum((test.lbls == 2) & ( pred.lbls ==1 ))
  num.cor  <- sum(pred.lbls == test.lbls)
  num.wrg  <- sum(pred.lbls != test.lbls)
  acc <- (true.pos + true.neg) / (num.normal + num.attack) *100
  pres <- true.pos / (true.pos + false.pos) * 100
  rcl  <- true.pos / (true.pos + false.neg) * 100
  fmes <- (2 * rcl * pres)/(rcl + pres)
  gmean <- sqrt( (true.pos / (true.pos + false.neg))*( true.neg / (true.neg + false.pos)) )
  dtrt <- true.neg / num.attack * 100
  falr <- false.neg / (true.neg + false.pos)
  
  roc.pred <- prediction(pred.lbls, test.lbls)
  auc      <- performance(roc.pred, "auc")
  auc      <- unlist(slot(auc, "y.values"))
  
  results.table$true_pos[r]     <<- true.pos
  results.table$true_neg[r]     <<- true.neg
  results.table$false_pos[r]    <<- false.pos
  results.table$false_neg[r]    <<- false.neg
  results.table$normal[r]       <<- num.normal
  results.table$attacks[r]      <<- num.attack
  results.table$perc.norma[r]   <<- nrm.rat
  results.table$perc.attac[r]   <<- num.attack
  results.table$correct[r]      <<- num.cor
  results.table$wrong[r]        <<- num.wrg
  results.table$accuracy[r]     <<- acc
  results.table$precision[r]    <<- pres
  results.table$recall[r]       <<- rcl
  results.table$f_measure[r]    <<- fmes
  results.table$g_mean[r]       <<- gmean
  results.table$false_alrm[r]   <<- falr
  results.table$det_rate[r]     <<- dtrt
  results.table$auc[r]          <<- auc
  
  if (dbug) cat("True Pos:   \t", true.pos, "\n")
  if (dbug) cat("True Neg:   \t", true.neg, "\n")
  if (dbug) cat("False Pos:  \t", false.pos, "\n")
  if (dbug) cat("False Neg:  \t", false.neg, "\n")
  if (dbug) cat("normal:     \t", num.normal, "\n")
  if (dbug) cat("attacks:    \t", num.attack, "\n")
  if (dbug) cat("Normal rat: \t", nrm.rat, "\n")
  if (dbug) cat("Attack rat: \t", att.rat, "\n")
  if (dbug) cat("Correct:    \t", num.cor, "\n")
  if (dbug) cat("Wrong:      \t", num.wrg, "\n")
  if (dbug) cat("Accuracy:   \t", acc, "\n")
  if (dbug) cat("Precission: \t", pres, "\n")
  if (dbug) cat("Recall:     \t", rcl, "\n")
  if (dbug) cat("F-Measure:  \t", fmes, "\n")
  if (dbug) cat("G-mean:     \t", gmean, "\n")
  if (dbug) cat("False Alarm:\t", falr, "\n")
  if (dbug) cat("Det Rate:   \t", dtrt ,"\n")
  if (dbug) cat("AUC         \t", auc  ,"\n")

  if (batch) {
    return(acc)
  }
}

#########################
# End of functions part #
#########################


##############################
# Start of main program part #
##############################

# Set seed so that result will be reproducible
#set.seed(42)

# Enable debug

dbug <- FALSE

# Set number of runs
r <- 3

# Allocate the datatable
results.table <- data.table(
  run_number  = integer(r),
  true_pos    = integer(r),
  true_neg    = integer(r),
  false_pos   = integer(r),
  false_neg   = integer(r),
  normal      = integer(r),
  attacks     = integer(r),
  perc.norma  = numeric(r),    
  perc.attac  = numeric(r),   
  correct     = integer(r), 
  wrong       = integer(r), 
  accuracy    = numeric(r), 
  precision   = numeric(r),  
  recall      = numeric(r), 
  f_measure   = numeric(r),    
  g_mean      = numeric(r), 
  false_alrm  = numeric(r),    
  det_rate    = numeric(r),  
  sizeof_L    = integer(r),  
  sizeof_U    = integer(r),  
  unlab_h1    = numeric(r),    
  unlab_h2    = numeric(r),    
  unlab_h3    = numeric(r),    
  tritr.iter  = integer(r),    
  auc         = numeric(r),    
  time.train  = numeric(r),
  time.pred   = numeric(r)
)
# Set numbers for the data that will be used. 
p.L    <- 0.75
L.size <- 1200
p.U    <- 0.7
U.size <- 10000
p.T    <- 5/6
T.size <- 30000


# Divide the training dataset into two separate one, from which
# we'll get the parts for our labeled and unlabeled data.

parts <- stratified(dt.train, group = "type",
                    size = 0.5, bothSets = TRUE)

# Create the labeled dataset.
dt.train.labeled   <- stratified(parts$SAMP1, group = "type",
                              size = c("1" = p.L*L.size, 
                                       "2" = (1 - p.L)*L.size
                                       )
                              )

# Create the unlabeled dataset
dt.train.unlabeled <- stratified(parts$SAMP2, group = "type",
                              size = c("1" = p.U*U.size, 
                                       "2" = (1 - p.U)*U.size
                                       )
                              )
# Create the test dataset
dt.test.part <- stratified(dt.test, group = "type",
                           size = c("1" = p.T*T.size,
                                    "2" = (1 - p.T)*T.size
                                    )
                           )


# Remove the labels from the unlabeled dataset.
dt.train.unlabeled <- subset(dt.train.unlabeled, select = -type)


# Call Tri-training with the labeled and the unlabeled datasets.

ttr_c <- tri_train(dt.train.labeled, 
                   dt.train.unlabeled, 
                   dbug = dbug,
                   results.table = results.table)

# Get prediction for the test data.

preds <- tri_pred(dt.test.part, ttr_c)
results <- cmp_lbls(preds,
                    dt.test.part[,type],
                    dbug = dbug,
                    results.table = results.table)


#######################
# End of main section #
#######################


#Rprof(filename = "Rprof.out", memory.profiling = TRUE )
#tprd.cmb <- cbind(tprd.cmb, tri_pred(subset(dt.tst, select = -type), h_i))
#tprd.cmb <- tri_pred(subset(dt.tst, select = -type), h_i)
#cmp_lbls(tprd.cmb, subset(dt.tst, select = type))
#table(tprd.cmb, subset(dt.tst, select = type)[,type])
#Rprof()

#cmp_lbls(tprd.1, test.set.lbls$y)
#cmp_lbls(tprd.2, test.set.lbls$y)
#cmp_lbls(tprd.3, test.set.lbls$y)



# Add test to gdis
#gdis <- addtest(gdis, train_unl_cmb[, var_names, with=FALSE], train_unl_cmb[,y])

# Add test to greal
#greal <- addtest(greal, train_unl_cmb[, var_names, with=FALSE], train_unl_cmb[,y])

# Add test to ggen
#ggen <- addtest(ggen, train_unl_cmb[, var_names, with=FALSE], train_unl_cmb[,y])

# get prediction from gdis
#pred_dis  <- predict(gdis,  train_unlabeled)
#pred_real <- predict(greal, train_unlabeled)
#pred_gent <- predict(ggen,  train_unlabeled)

# Convert predictions to data tables
#pred_dis  <- as.data.table(pred_dis)
#pred_real <- as.data.table(pred_real)
#pred_gent <- as.data.table(pred_gent)

## Combine the predictions with the correct labels
#dis_res <- cbind(pred_dis_dt[,"pred_dis"], test_set_lbls[,.(y)])
#real_res <- cbind(pred_real_dt[,"pred_real"], test_set_lbls[,.(y)])
#gent_res <- cbind(pred_gent_dt[,"pred_gent"], test_set_lbls[,.(y)])

# Create a new column. If the prediction and the correct label match,
# then put 1. Else put 0
#dis_res[, "p" := ifelse(get("V1")==get("y"), 1, 0)]
#real_res[, "p" := ifelse(get("V1")==get("y"), 1, 0)]
#gent_res[, "p" := ifelse(get("V1")==get("y"), 1, 0)]

# Convert the columns to factors
#dis_res$y <- as.factor(dis_res$y)
#real_res$y <- as.factor(real_res$y)
#gent_res$y <- as.factor(gent_res$y)

#dis_res$p <- as.factor(dis_res$p)
#real_res$p <- as.factor(real_res$p)
#gent_res$p <- as.factor(gent_res$p)

# Display the summary

#summary(dis_res)
#summary(real_res)
#summary(gent_res)

#comb <- cbind(pred_dis_dt, pred_gent_dt, pred_real_dt)
#comb <- as.data.table(comb)
#comb[,tr:= ifelse( (get("pred_dis") == get("pred_gent")) & (get("pred_dis") == get("pred_real")), 1,0 )]
#comb$tr <- as.factor(comb$tr)

#pred_rd <- cbind(pred_real_dt, pred_dis_dt)
#pred_rg <- cbind(pred_real_dt, pred_gent_dt)
#pred_gd <- cbind(pred_gent_dt, pred_dis_dt)



