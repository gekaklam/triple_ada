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

#################################################
# Load the un labeled training set and its labels
train_unlabeled      <- data.table(read.csv("../data/Train_UnlabeledSet.txt", header = FALSE, col.names = var_names))
train_unlabeled_lbls <- data.table(read.csv("../data/Train_UnlabeledSet_Label.txt", header = FALSE, col.names = c("Type")))

# Convert to data.tables
#train_unlabeled <- as.data.table(train_unlabeled)
#train_unlabeled_lbls <- as.data.table(train_unlabeled_lbls)

# Create an label that shows if it's an attack or not.
train_unlabeled_lbls[, "y" := ifelse(get("Type") == 1, 1, 2)]

train_unl_cmb <- cbind(train_unlabeled,train_unlabeled_lbls[,.(y)])

#################################################
# Load the testing set and its labels
test_set      <- data.table(read.csv("../data/Testing_LabeledSet.txt", header = FALSE, col.names = var_names))
test_set_lbls <- data.table(read.csv("../data/Testing_LabeledSet_Label.txt", header = FALSE, col.names = c("Type")))

# Convert to data.tables
#test_set <- as.data.table(test_set)
#test_set_lbls <- as.data.table(test_set_lbls)

# Create an label that shows if it's an attack or not.
test_set_lbls[, "y" := ifelse(get("Type") == 1, 1, 2)]

test_cmb <- cbind(test_set,test_set_lbls[,.(y)])

#################################################
# Set the tree depth to 1 (stumps)
# default <- rpart.control()

stump <- rpart.control(cp = -1, maxdepth = 1, minsplit = 0)

#Create the discrete ada boost object
ada1 <- ada(y~., data = train.ada1, iter = 50, loss = "e", type = "discrete", control = stump)

#Create the real ada boost object
ada2 <- ada(y~., data = train.ada2, iter = 50, loss = "e", type = "real", control = stump)

#Create the gentle ada boost object
ada3 <- ada(y~., data = train.ada3, iter = 50, loss = "e", type = "gentle", control = stump)

#Make the list of the objects
h_i <- list(ada1, ada2, ada3)

e.n_i    <- c(0.5, 0.5, 0.5)
l.n_i    <- c(0, 0, 0)
L_i      <- c(0, 0, 0)
update_i <- c( FALSE, FALSE, FALSE)

MeasureError <- function(h_i, i) {
  h_i.tmp <- h_i[-i]
  h_j <- h_i.tmp[[1]]
  h_k <- h_i.tmp[[2]]
  d_t <- data.table(cbind(h_j$actual, h_j$fit, h_k$actual, h_k$fit))
  n_inc <- sum(d_t$V1 != d_t$V2) + sum(d_t$V3 != d_t$V4)
  n_cor <- sum(d_t$V1 == d_t$V2) + sum(d_t$V3 == d_t$V4)
  er_i <- n_inc / n_cor
  return(er_i)
}














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


