# Overview #

`anompipe` implements anomaly detection based on the S-H-ESD algorithm as a UNIX
utility - simple, composable, reusable.

`anompipe` takes tab-delimited text as an input. The first column is always
the timestamp, formatted in anything readable by 
(python-dateutil)[https://labix.org/python-dateutil].

    2015-02-13T14:50:56-0500    47
    2015-02-13T14:51:04-0500    89
    2015-02-13T14:51:06-0500    100

As a result, data can be fed from any source, using any programming language.

# Usage #

Basic usage:

    source | python anompipe.py <anomaly_rate> | target

