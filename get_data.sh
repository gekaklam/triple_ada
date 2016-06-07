#!/bin/bash

# Download the KDD Cup 1999, 10% Data set
wget -nv http://kdd.ics.uci.edu/databases/kddcup99/kddcup.data_10_percent.gz

# Extract the file
gzip -d kddcup.data_10_percent.gz

