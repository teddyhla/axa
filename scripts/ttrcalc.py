def ttrcalc(v1, v2, upper=3.00001, lower=1.9999):
    # formula for time in therapeutic range calculation using linear interpol
    # variables upper and lower are target ranges.
    # this calculator should be able to handle 9 possible scenarios
    upper = upper
    lower = lower
    vdiff = abs(v2 - v1)
    # vdiff allows absolute difference between two values.
    res = 0
    # result object.

    if v1 < lower and v2 < lower:
        # situation 1 where both vals are lower than range.
        return "low"

    elif v1 < lower and lower < v2 < upper:
        # situation 2
        res = (v2 - lower) / vdiff
        return res

    elif v1 < lower and v2 > upper:
        # situation 3
        res = (upper - lower) / vdiff
        return res

    elif lower < v1 < upper and v2 < lower:
        # situation 4
        res = (v1 - lower) / vdiff
        return res

    elif lower < v1 < upper and lower < v2 < upper:
        # situation 5 where both vals are in range.
        return 1

    elif lower < v1 < upper and v2 > upper:
        # situation 6
        res = (upper - v1) / vdiff
        return res

    elif v1 > upper and v2 < lower:
        # situation 7
        res = (upper - lower) / vdiff
        return res

    elif v1 > upper and lower < v2 < upper:
        # situation 8
        res = (upper - v2) / vdiff
        return res

    elif v1 > upper and v2 > upper:
        # situation 9 where both vals are higher than range.
        return " higher"
