#!/bin/bash

# Download the KDD Cup 1999, 10% Data set
wget -nv http://kdd.ics.uci.edu/databases/kddcup99/kddcup.data_10_percent.gz

# Download the test data
wget -nv http://kdd.ics.uci.edu/databases/kddcup99/corrected.gz

# Extract the training data
gzip -d kddcup.data_10_percent.gz

# Extract the test data
gzip -d corrected.gz


# Remove the trailing . from the files
sed 's/.$//' kddcup.data_10_percent >> data_train.cleaned
sed 's/.$//' corrected >> data_test.cleaned
