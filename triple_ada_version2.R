library("ada")
library("data.table")
library("dplyr")
library("splitstackshape")

#Rprof(filename = "Rprof.out", memory.profiling = TRUE )
#set.seed(123456)

var_names <- c("duration","protocol_type","service","flag","src_bytes","dst_bytes","land","wrong_fragment","urgent","hot","num_failed_logins","logged_in","num_compromised","root_shell","su_attempted","num_root","num_file_creations","num_shells","num_access_files","num_outbound_cmds","is_host_login","is_guest_login","count","srv_count","serror_rate","srv_serror_rate","rerror_rate","srv_rerror_rate","same_srv_rate","diff_srv_rate","srv_diff_host_rate","dst_host_count","dst_host_srv_count","dst_host_same_srv_rate","dst_host_diff_srv_rate","dst_host_same_src_port_rate","dst_host_srv_diff_host_rate","dst_host_serror_rate","dst_host_srv_serror_rate","dst_host_rerror_rate","dst_host_srv_rerror_rate", "type")

dt.tr <- data.table(read.csv("data_cleaned.csv", header = FALSE, col.names = var_names))
dt.tst <- data.table(read.csv("data_test_cleaned.csv", header = FALSE, col.names = var_names))

# Remove two values from the test set, which have
# a service not included in training data (probably error)
dt.tst <- dt.tst[dt.tst[,service != "icmp" ]]

# and their levels
dt.tst$service <- droplevels(dt.tst$service)


# Set the percentance of training data that will be used as labeled 
p <- 0.1

#Create the two samples from the training data

smpls <- stratified(dt.tr, "type", p, bothSets = TRUE)

train.labeled   <- smpls[[1]][sample(nrow(smpls$SAMP1)),]
train.unlabeled <- smpls[[2]][sample(nrow(smpls$SAMP2)),]

train.labeled[, "type" := ifelse(get("type") == "normal", 1, 2) ]
train.labeled$type <- as.factor(train.labeled$type)

dt.tst[, "type" := ifelse(get("type") == "normal", 1, 2) ]
dt.tst$type <- as.factor(dt.tst$type)

train.unlabeled <- subset(train.unlabeled, select = -type)

# Split the datasets for the three adaboosts
train.labeled.split <- split(train.labeled , f = rep_len(1:3, nrow(train.labeled)))

# Set the types for the different adaboost methods
type_i <- c("discrete", "real", "gentle")

h_i <- list(0, 0, 0)

# Create the stumps
stump <- rpart.control(cp = -1, maxdepth = 1, minsplit = 0)

for (i in 1:3) {
  h_i[[i]] <- ada(type~., data = train.labeled.split[[i]], iter = 50, loss="e", type = type_i[i], control = stump )
}

# Initialize variables
e.n_i    <- c(0.5, 0.5, 0.5)
l.n_i    <- c(0, 0, 0)
L_i      <- list(0, 0, 0)
update_i <- c( FALSE, FALSE, FALSE)
e_i      <- c(0,0,0)
iter     <- 0

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

# Start of repeat
repeat {
  cat("########################\n")
  cat("# Start of iteration", iter, "#\n")
  cat("########################\n\n")

  for (i in 1:3) {
    L_i[[i]] <- 0
    update_i[i] <- FALSE
    e_i[i] <- MeasureError(h_i[-i], i)
    cat("Measured Error for", i, "is: \t",e_i[i],"\n")
    cat("Previous Error for", i, "was: \t",e.n_i[i],"\n\n")
    if (e_i[i] < e.n_i[i]){
      cat("Meassured Error is smaller than previous error. \n")
      cat("Using an unlabeled data set of size:", dim(train.unlabeled)[1])
      cat("\n")
#      cat("Inside the first if","\n")
#      cat("\n")
      # Here we name pred_1 = y, so that it is easier later in the rbind
      type   <- predict(h_i[-i][[1]], train.unlabeled)
      pred_2 <- predict(h_i[-i][[2]], train.unlabeled)
      
      d_t <- data.table(cbind(train.unlabeled, type))
      agree <- type == pred_2
      cat("Number of predictions that classifier agree:", sum(agree==1), "\n")
      L_i[[i]] <- d_t[agree,]
      
      if (l.n_i[i] == 0) {
        cat("Previous length of L_",i," was 0", sep = "")
        cat("\n")
        cat("Classifier:",i,"has not been updated before.\n")
        l.n_i[i] <- floor( e_i[i] / ( e.n_i[i] - e_i[i] ) + 1 )
        cat("Now length of L_",i," is set to: \t", l.n_i[i],"\n", sep = "")
      }
      cat("\n")
      L.len <- dim(L_i[[i]])[1]
      cat("Length of tmp_labeled set, for", i, "is now", L.len,"\n")
      cat("\n")
      if (l.n_i[i] < L.len ) {
        cat("Previous length of tmp_labeled set, is less than new length. \n")
        #cat("Inside third if. \n")
        #cat("ln_i[",i,"] is:",l.n_i[i],"and L.len:",L.len, "\n")
        #cat("\n")
        #cat("e_i[i]*L.len", e_i[i]*L.len,"e.n_i[i]*l.n_i[i]",e.n_i[i]*l.n_i[i])
        #cat("update",i,update_i[i])
        #cat("l.n_i",i,"=", l.n_i[i])
        #cat("(e_i[i] / (e.n_i[i] - e_i[i]))", (e_i[i] / (e.n_i[i] - e_i[i])))
        if (e_i[i]*L.len < e.n_i[i]*l.n_i[i]) {
          cat("Eq 9, is not violated \n")
          cat("Classifier: ", i, " is set to be updated. \n", sep = "")
          #cat(e_i[i]*L.len, "is less than", e.n_i[i]*l.n_i[i], "\n")
          update_i[i] <- TRUE
#          print(c("update",i,update_i[i]))
          #cat("l[",i,"] is:", l.n_i[i], "\n")
          cat("\n")
        } else if ( l.n_i[i] > (e_i[i] / (e.n_i[i] - e_i[i])) ) {
            cat("Need to subsample tmp_labeled set. \n")
            s.len <- ceiling( (e.n_i[i]*l.n_i[i]) / e_i[i] - 1)
            cat("Size of sample is equal to:", s.len, "\n")
            s.idx <- sample(1:L.len, s.len)
            L_i[[i]] <- L_i[[i]][s.idx,]
            cat("Classifier:", i, "is set to be updated. \n")
            update_i[i] <- TRUE
            cat("\n")
        }
      }
    }
  }
  cat("The classifiers to be updated are:",update_i,"\n\n")
  for (i in 1:3) {
    if (update_i[i] == TRUE) {
      cat("Now updating:", i, "\n")
      cat("\n")
      cat("Dim, Train",i,"is:\t", dim(train.labeled.split[[i]])[1], "\n")
      cat("Dim, L_",i," is:\t\t ", dim(L_i[[i]])[1], "\n", sep = "")
      cat("Dim, combined is: \t", dim(L_i[[i]])[1]+dim(train.labeled.split[[i]])[1], "\n")
      h_i[[i]] <- ada(type~., data = rbind(train.labeled.split[[i]],L_i[[i]]), iter = 50, loss="e", type = type_i[i], control = stump )
      cat("Re-trained classifier with", h_i[[i]]$dim[1], "data points. \n")
      cat("\n")
      e.n_i[[i]] <- e_i[[i]]
      l.n_i[[i]] <- dim(L_i[[i]])[1]
    } 
  }
  cat("# of iterations:",iter, "\n")
  if (update_i[1] != TRUE & update_i[2] !=TRUE & update_i[3] != TRUE) {
    break
  }
  else if (iter > 10) {
    break
  }
  cat("######################\n")
  cat("# End of iteration", iter, "#\n")
  cat("######################\n\n")
  iter <- iter + 1
}

tri_pred <- function(tst.dset, h_i) {
  tprd.1 <- as.integer(predict(h_i[[1]], tst.dset))
  tprd.2 <- as.integer(predict(h_i[[2]], tst.dset))
  tprd.3 <- as.integer(predict(h_i[[3]], tst.dset))
  tprd.agr <- tprd.1 + tprd.2 + tprd.3
  tprd.agr <- as.data.table(tprd.agr)
  tprd.agr[, "p" := ifelse( (get("tprd.agr") == 3) | (get("tprd.agr") == 4), 1,2 )]
  return(tprd.agr$p)
}

cmp_lbls <- function(pred.lbls, test.lbls) {
  num.normal   <- sum(test.lbls == 1)
  num.attack <- sum(test.lbls == 2)
  true.pos <- sum((pred.lbls ==1) & (test.lbls == 1))
  true.neg <- sum((pred.lbls ==2) & (test.lbls == 2))
  false.pos <- sum((test.lbls == 1) & ( pred.lbls ==2 ))
  false.neg <- sum((test.lbls == 2) & ( pred.lbls ==1 ))
  #wrg <- sum(lbls.1 != lbls.2)
  #err <- wrg/length(lbls.2)
  num.cor  <- sum(pred.lbls == test.lbls)
  acc <- (true.pos + true.neg) / (num.normal + num.attack) *100
  pres <- true.pos / (true.pos + false.pos) * 100
  rcl  <- true.pos / (true.pos + false.neg) * 100
  fmes <- (2 * rcl * pres)/(rcl + pres)
  gmean <- sqrt( (true.pos / (true.pos + false.neg))*( true.neg / (true.neg + false.pos)) )
  #fpos <- num.miss.norm / num.normal * 100
  dtrt <- true.neg / num.attack * 100
  falr <- false.neg / (true.neg + false.pos)
  #print(c("Correct: ", crt ))
  #print(c("Wrong: ",   wrg ))
  cat("True Pos:   \t", true.pos, "\n")
  cat("True Neg:   \t", true.neg, "\n")
  cat("False Pos:  \t", false.pos, "\n")
  cat("False Neg:  \t", false.neg, "\n")
  cat("normal:     \t", num.normal, "\n")
  cat("attacks:    \t", num.attack, "\n")
  cat("Correct:    \t", num.cor, "\n")
  cat("Accuracy:   \t", acc, "\n")
  cat("Precission: \t", pres, "\n")
  cat("Recall:     \t", rcl, "\n")
  cat("F-Measure:  \t", fmes, "\n")
  cat("G-mean:     \t", gmean, "\n")
  #cat("False Posit:\t", fpos, "\n")
  cat("False Alarm:\t", falr, "\n")
  cat("Det Rate:   \t", dtrt ,"\n")
  
}

#Rprof(filename = "Rprof.out", memory.profiling = TRUE )
tprd.cmb <- cbind(tprd.cmb, tri_pred(subset(dt.tst, select = -type), h_i))
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



