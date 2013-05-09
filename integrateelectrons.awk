#!/usr/bin/awk -f

BEGIN {
}
/^eedf / && NF > 0 {
    for (i=2;i<=4;i++) {
        if (!($2 in integrals)) {
            integrals[$2] = 0
        }
        integrals[$2] += $4
        #print $2, $4
    }
}
END {
    for (t in integrals) {
        print t, integrals[t]
    }
}