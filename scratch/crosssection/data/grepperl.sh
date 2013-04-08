grep -e For | perl -pe 's|^.*?at ([\d\.]*) eV.*?([\d\.]*) \&.*|\1 \2|g'
