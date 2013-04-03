clc, clf, close all

x = logspace(0.01,log10(300));

loglog(x, 1./(1+(x.^2./13)));