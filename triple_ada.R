library("ada")
library("data.table")

#Rprof(filename = "Rprof.out", memory.profiling = TRUE )
set.seed(123456)
var_names <- c("duration","protocol_type","service","flag","src_bytes","dst_bytes","land","wrong_fragment","urgent","hot","num_failed_logins","logged_in","num_compromised","root_shell","su_attempted","num_root","num_file_creations","num_shells","num_access_files","num_outbound_cmds","is_host_login","is_guest_login","count","srv_count","serror_rate","srv_serror_rate","rerror_rate","srv_rerror_rate","same_srv_rate","diff_srv_rate","srv_diff_host_rate","dst_host_count","dst_host_srv_count","dst_host_same_srv_rate","dst_host_diff_srv_rate","dst_host_same_src_port_rate","dst_host_srv_diff_host_rate","dst_host_serror_rate","dst_host_srv_serror_rate","dst_host_rerror_rate","dst_host_srv_rerror_rate")

##############################################
# Load the labeled training set and its labels
train_labeled      <- data.table(read.csv("../data/Train_LabeledSet.txt", header = FALSE, col.names = var_names))
train_labeled_lbls <- data.table(read.csv("../data/Train_LabeledSet_Label.txt", header = FALSE , col.names = c("Type")))

# Convert to data.tables
#train_labeled      <- as.data.table(train_labeled)
#train_labeled_lbls <- as.data.table(train_labeled_lbls)

# Create a label that shows if it's an attack or not.
train_labeled_lbls[, "y" := ifelse(get("Type") == 1, 1, 2)]

# Combine the objects
train_cmb <- cbind(train_labeled,train_labeled_lbls[,.(y)])
ss1 <- subset(train_cmb, y==1)
ss2 <- subset(train_cmb, y==2)

train.ada1 <- rbind(ss1[1:1000], ss2[1:333])
train.ada2 <- rbind(ss1[1001:2000], ss2[334:666])
train.ada3 <- rbind(ss1[2001:3000], ss2[667:999])

#train.ada1 <- rbind(ss1[1:333], ss2[1:333])
#train.ada2 <- rbind(ss1[334:666], ss2[334:666])
#train.ada3 <- rbind(ss1[667:999], ss2[667:999])

train_dt <- list(train.ada1, train.ada2, train.ada3)
type_i <- c("discrete", "real", "gentle")

#################################################
# Load the un labeled training set and its labels
train.unlabeled      <- data.table(read.csv("../data/Train_UnlabeledSet.txt", header = FALSE, col.names = var_names))
#train_unlabeled_lbls <- data.table(read.csv("../data/Train_UnlabeledSet_Label.txt", header = FALSE, col.names = c("Type")))

# Convert to data.tables
#train_unlabeled <- as.data.table(train_unlabeled)
#train_unlabeled_lbls <- as.data.table(train_unlabeled_lbls)

# Create an label that shows if it's an attack or not.
#train_unlabeled_lbls[, "y" := ifelse(get("Type") == 1, 1, 2)]

#train_unl_cmb <- cbind(train_unlabeled,train_unlabeled_lbls[,.(y)])

#################################################
# Load the testing set and its labels
test.set      <- data.table(read.csv("../data/Testing_LabeledSet.txt", header = FALSE, col.names = var_names))
test.set.lbls <- data.table(read.csv("../data/Testing_LabeledSet_Label.txt", header = FALSE, col.names = c("Type")))

# Convert to data.tables
#test_set <- as.data.table(test_set)
#test_set_lbls <- as.data.table(test_set_lbls)

# Create an label that shows if it's an attack or not.
test.set.lbls[, "y" := ifelse(get("Type") == 1, 1, 2)]

test.cmb <- cbind(test.set,test.set.lbls[,.(y)])

#################################################
# Set the tree depth to 1 (stumps)
# default <- rpart.control()

h_i <- list(0, 0, 0)

stump <- rpart.control(cp = -1, maxdepth = 1, minsplit = 0)

for (i in 1:3) {
  h_i[[i]] <- ada(y~., data = train_dt[[i]], iter = 50, loss="e", type = type_i[i], control = stump )
}

#Create the discrete ada boost object
#ada1 <- ada(y~., data = train.ada1, iter = 50, loss = "e", type = "discrete", control = stump)

#Create the real ada boost object
#ada2 <- ada(y~., data = train.ada2, iter = 50, loss = "e", type = "real", control = stump)

#Create the gentle ada boost object
#ada3 <- ada(y~., data = train.ada3, iter = 50, loss = "e", type = "gentle", control = stump)

#Make the list of the objects


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
      y      <- predict(h_i[-i][[1]], train.unlabeled)
      pred_2 <- predict(h_i[-i][[2]], train.unlabeled)
      
      d_t <- data.table(cbind(train.unlabeled, y))
      agree <- y == pred_2
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
      cat("Dim, Train",i,"is:\t", dim(train_dt[[i]])[1], "\n")
      cat("Dim, L_",i," is:\t\t ", dim(L_i[[i]])[1], "\n", sep = "")
      cat("Dim, combined is: \t", dim(L_i[[i]])[1]+dim(train_dt[[i]])[1], "\n")
      h_i[[i]] <- ada(y~., data = rbind(train_dt[[i]],L_i[[i]]), iter = 50, loss="e", type = type_i[i], control = stump )
      cat("Re-trained classifier with", h_i[[i]]$dim[1], "data points. \n")
      cat("\n")
      e.n_i[[i]] <- e_i[[i]]
      l.n_i[[i]] <- dim(L_i[[i]])[1]
    } 
  }
#  cat("# of iterations:",iter, "\n")
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
  pres <- num.cor / length(test.lbls) * 100
#  rcl  <- num.cor / (num.cor + )
  fpos <- num.miss.norm / num.normal * 100
  dtrt <- num.det.att / num.attack * 100
  #print(c("Correct: ", crt ))
  #print(c("Wrong: ",   wrg ))
  cat("True Pos:  \t", true.pos, "\n")
  cat("True Neg:  \t", true.neg, "\n")
  cat("False Pos: \t", false.pos, "\n")
  cat("False Neg: \t", false.neg, "\n")
  cat("normal:    \t", num.normal, "\n")
  cat("attacks:   \t", num.attack, "\n")
  cat("correct:   \t", num.cor, "\n")
  cat("Precission: \t", pres, "\n")
#  cat("Recall:     \t", rcl, "\n")
  cat("False Posit:\t", fpos, "\n")
  cat("Det Rate:   \t", dtrt ,"\n")
  
}

#Rprof(filename = "Rprof.out", memory.profiling = TRUE )
tprd.cmb <- tri_pred(test.set, h_i)
cmp_lbls(tprd.cmb, test.set.lbls$y)
table(tprd.cmb,test.set.lbls$y)
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



