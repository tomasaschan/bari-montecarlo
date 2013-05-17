#!/usr/bin/awk -f

BEGIN {
    counts[2] = 0;
    counts[3] = 0;
    counts[4] = 0;
}
/^[^#]/ {
    for (i=2;i<=4;i++) {
        counts[i] += $i
    }
}
END {
    for (i=2;i<=4;i++) {
        print counts[i]/counts[2]
    }
}