library("ada")
library("data.table")

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
  return(er_i)
}

# Start of repeat
repeat {
  print(c("This is iter", iter))
  for (i in 1:3) {
    L_i[[i]] <- 0
    update_i[i] <- FALSE
    e_i[i] <- MeasureError(h_i[-i], i)
    if (e_i[i] < e.n_i[i]){
      print(c("ei=",e_i[i],"en_i=",e.n_i[i]))
      # Here we name pred_1 = y, so that it is easier later in the rbind
      y <- predict(h_i[-i][[1]], train.unlabeled)
      pred_2 <- predict(h_i[-i][[2]], train.unlabeled)
      d_t <- data.table(cbind(train.unlabeled, y))
      agree <- y == pred_2
      L_i[[i]] <- d_t[agree,]
      if (l.n_i[i] == 0) {
        print("lni=0")
        l.n_i[i] <- floor( e_i[i] / ( e.n_i[i] - e_i[i] ) + 1 )
      }
      L.len <- dim(L_i[[i]])[1]
      print(c("L.len", i, "is", L.len))
      if (l.n_i[i] < L.len ) {
        print(c("ln_i:",l.n_i[i],"L.len:",L.len))
        print(c("e_i[i]*L.len", e_i[i]*L.len,"e.n_i[i]*l.n_i[i]",e.n_i[i]*l.n_i[i]))
        print(c("update",i,update_i[i]))
        print(c("l.n_i",i,"=", l.n_i[i]))
        print(c("(e_i[i] / (e.n_i[i] - e_i[i]))", (e_i[i] / (e.n_i[i] - e_i[i]))))
        if (e_i[i]*L.len < e.n_i[i]*l.n_i[i]) {
          update_i[i] <- TRUE
          print(c("update",i,update_i[i]))
        } else if ( l.n_i[i] > (e_i[i] / (e.n_i[i] - e_i[i])) ) {
            s.len <- ceiling( (e.n_i[i]*l.n_i[i]) / e_i[i] - 1)
            s.idx <- sample(1:L.len, s.len)
            L_i[[i]] <- L_i[[i]][s.idx,]
            update_i[i] <- TRUE
        }
      }
    }
  }
  print(update_i)
  for (i in 1:3) {
    if (update_i[i] == TRUE) {
      h_i[[i]] <- ada(y~., data = rbind(train_dt[[i]],L_i[[i]]), iter = 50, loss="e", type = type_i[i], control = stump )
      e.n_i[[i]] <- e_i[[i]]
      l.n_i[[i]] <- dim(L_i[[i]])[1]
    } 
  }
  print(iter)
  if (update_i[1] != TRUE & update_i[2] !=TRUE & update_i[3] != TRUE) {
    break
  }
  else if (iter > 10) {
    break
  }
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

tprd.cmb <- tri_pred(test.set, h_i)

cmp_lbls <- function(lbls.1, lbls.2) {
  crt <- sum(lbls.1 == lbls.2)
  wrg <- sum(lbls.1 != lbls.2)
  err <- wrg/crt
  print(c("Correct: ", crt ))
  print(c("Wrong: ",   wrg ))
  print(c("Error: ",   err ))
}

cmp_lbls(tprd.cmb, test.set.lbls$y)
cmp_lbls(tprd.1, test.set.lbls$y)
cmp_lbls(tprd.2, test.set.lbls$y)
cmp_lbls(tprd.3, test.set.lbls$y)



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



