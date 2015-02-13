#!/usr/bin/env python

import datetime
import sys
import anomlib
import string

def main():
    # Load package
    anompkg = anomlib.load_package()
    # Load all data
    timestamps = []
    datapoints = []
    for line in sys.stdin:
        timestamp, point = line.split(string.whitespace)
        timestamps.append(datetime.datetime.strptime("%Y-%m-%dT%H:%M:%S"))
        datapoints.append(float(point))
    # Now, detect anomalies and output them as tab-delimited text
    anomlib.detect_time_series(anompkg, timestamps, points)

if __name__=="__main__":
    main()
