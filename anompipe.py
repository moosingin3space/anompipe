#!/usr/bin/env python

from dateutil import parser as dateparser
import pytz
import sys
import anomlib
import string
import tabulate

def main():
    anom_pct = 0.10
    if len(sys.argv) > 1:
        anom_pct = float(sys.argv[1])

    # Load package
    anompkg = anomlib.load_package()
    # Load all data
    timestamps = []
    datapoints = []
    for line in sys.stdin.readlines():
        timestamp, point = line.split()
        timestamps.append(dateparser.parse(timestamp).astimezone(pytz.utc))
        datapoints.append(float(point))

    anomalies = anomlib.detect_time_series(anompkg, timestamps, datapoints, max_anoms=anom_pct)

    if len(anomalies) == 0:
        print 'No anomalies detected'
    else:
        if len(anomalies) == 1:
            print '1 anomaly detected'
        else:
            print '%d anomalies detected' % len(anomalies)

        print '\n'
        print tabulate.tabulate(anomalies, headers=['Timestamp', 'Value', 'Reason'], tablefmt="orgtbl")

if __name__=="__main__":
    main()
