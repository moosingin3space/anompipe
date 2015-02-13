# Overview #

`anompipe` implements anomaly detection based on the S-H-ESD algorithm as a UNIX
utility - simple, composable, reusable.

`anompipe` takes tab-delimited text as an input. The first column is always
the timestamp in GMT, formatted as `YYYY-mm-ddTHH:MM:SS`:

    2015-02-13T14:50:56    47
    2015-02-13T14:51:04    89
    2015-02-13T14:51:06    100

As a result, data can be fed from any source, using any programming language.

# Usage #

Basic usage:

    source | python anompipe.py <num_anomalies> | target

