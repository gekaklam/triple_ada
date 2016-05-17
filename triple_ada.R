library("ada")
library("data.table")

var_names <- c("duration","protocol_type","service","flag","src_bytes","dst_bytes","land","wrong_fragment","urgent","hot","num_failed_logins","logged_in","num_compromised","root_shell","su_attempted","num_root","num_file_creations","num_shells","num_access_files","num_outbound_cmds","is_host_login","is_guest_login","count","srv_count","serror_rate","srv_serror_rate","rerror_rate","srv_rerror_rate","same_srv_rate","diff_srv_rate","srv_diff_host_rate","dst_host_count","dst_host_srv_count","dst_host_same_srv_rate","dst_host_diff_srv_rate","dst_host_same_src_port_rate","dst_host_srv_diff_host_rate","dst_host_serror_rate","dst_host_srv_serror_rate","dst_host_rerror_rate","dst_host_srv_rerror_rate")

# Load the labeled training set and its labels
train_labeled <- read.csv("../data/Train_LabeledSet.txt", header = FALSE, col.names = var_names)
train_labeled_lbls <- read.csv("../data/Train_LabeledSet_Label.txt", header = FALSE , col.names = c("Type"))

# Convert to data.tables
train_labeled <- as.data.table(train_labeled)
train_labeled_lbls <- as.data.table(train_labeled_lbls)

# Create an label that shows if it's an attack or not.
train_labeled_lbls[, "y" := ifelse(get("Type") == 1, 1, 2)]

# Combine the objects
train_cmb <- cbind(train_labeled,train_labeled_lbls[,.(y)])


# Load the un labeled training set and its labels
train_unlabeled <- read.csv("../data/Train_UnlabeledSet.txt", header = FALSE, col.names = var_names)
train_unlabeled_lbls <- read.csv("../data/Train_UnlabeledSet_Label.txt", header = FALSE, col.names = c("Type"))

# Convert to data.tables
train_unlabeled <- as.data.table(train_unlabeled)
train_unlabeled_lbls <- as.data.table(train_unlabeled_lbls)

# Create an label that shows if it's an attack or not.
train_unlabeled_lbls[, "y" := ifelse(get("Type") == 1, 1, 2)]

train_unl_cmb <- cbind(train_unlabeled,train_unlabeled_lbls[,.(y)])

# Load the testing set and its labels
test_set <- read.csv("../data/Testing_LabeledSet.txt", header = FALSE, col.names = var_names)
test_set_lbls <- read.csv("../data/Testing_LabeledSet_Label.txt", header = FALSE, col.names = c("Type"))

# Convert to data.tables
test_set <- as.data.table(test_set)
test_set_lbls <- as.data.table(test_set_lbls)

# Create an label that shows if it's an attack or not.
test_set_lbls[, "y" := ifelse(get("Type") == 1, 1, 2)]

test_cmb <- cbind(test_set,test_set_lbls[,.(y)])

# Set the tree depth to 1 (stumps)
# default <- rpart.control()

stump <- rpart.control(cp = -1, maxdepth = 1, minsplit = 0)

#Create the discrete ada boost object

gdis <- ada(y~., data = train_cmb, iter = 50, loss = "e", type = "discrete", control = stump)

#Create the real ada boost object

greal <- ada(y~., data = train_cmb, iter = 50, loss = "e", type = "real", control = stump)

#Create the gentle ada boost object

ggen <- ada(y~., data = train_cmb, iter = 50, loss = "e", type = "gentle", control = stump)



