reset
set terminal pngcairo enhanced #size 40cm,25cm

srcdir = '../'.srcdir.'/'

infile   = srcdir."input.in"
datafile = srcdir."simulation.out"

e0 = system("head -n 4 ".infile." | tail -n 1")
p = system("head -n 5 ".infile." | tail -n 1")
tfin = system("head -n 2 ".infile." | tail -n 1")
energytitle = "Beam energy: ".gprintf("%.1s %c",e0+0)."eV"
timetitle = "Time: ".gprintf("%.1s %c",tfin+0)."s"
pressurettitle = "Pressure: ".gprintf("%.1s %c",p)."Torr"

rundatatitle = energytitle.",  ".timetitle.",  ".pressurettitle

set title rundatatitle
