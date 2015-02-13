import rpy2.robjects as R
import rpy2.rlike.container as rlc
from rpy2.robjects.packages import SignatureTranslatedAnonymousPackage
import os

def load_package():
    # start by loading R source into R
    libfilename = os.path.join(os.path.dirname(__file__), 'detect_anoms.R')
    with open(libfilename) as f:
        data = f.read()
        return SignatureTranslatedAnonymousPackage(data, "anomlib")
    return None
    
def detect_time_series(pkg, timestamps, points, max_anoms=0.10, alpha=0.05):
    data = R.DataFrame(rlc.OrdDict([('timestamp', R.vectors.POSIXct(timestamps)), ('count', R.vectors.FloatVector(points))]))
    res = pkg.AnomalyDetectionTs(data, max_anoms, alpha)
    print res
