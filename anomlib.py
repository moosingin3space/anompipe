import rpy2.robjects as R
import rpy2.rlike.container as rlc
from rpy2.robjects.packages import SignatureTranslatedAnonymousPackage
import os
import datetime

def load_package():
    # start by loading R source into R
    libfilename = os.path.join(os.path.dirname(__file__), 'detect_anoms.R')
    with open(libfilename) as f:
        data = f.read()
        return SignatureTranslatedAnonymousPackage(data, "anomlib")
    return None
    
def detect_time_series(pkg, timestamps, points, max_anoms=0.10, alpha=0.10):
    data = R.DataFrame(rlc.OrdDict([('timestamp', R.vectors.POSIXct(timestamps)), ('count', R.vectors.FloatVector(points))]))
    res = pkg.AnomalyDetectionTs(data, max_anoms=max_anoms, alpha=alpha)
    if res:
        anomalies = res.rx('anoms')[0]
        stl = res.rx('trendline')[0]

        anoms = []
        for i, ts in enumerate(anomalies.rx('timestamp')[0]):
            value = anomalies.rx('count')[0][i]
            anom = [datetime.datetime.utcfromtimestamp(ts),
                    value,
                    'unknown']

            stl_value = 0
            for i, x in enumerate(stl.rx('timestamp')[0]):
                if abs(ts - x) < 0.1:
                    stl_value = stl.rx('count')[0][i]

            if value > stl_value:
                anom[2] = 'above seasonal/trend line (typical value %d)' % stl_value
            elif value < stl_value:
                anom[2] = 'below seasonal/trend line (typical value %d)' % stl_value

            anoms.append(anom)
    else:
        anoms = []

    return anoms
