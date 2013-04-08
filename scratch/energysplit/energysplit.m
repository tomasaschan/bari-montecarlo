clc, 

E = 13;
format long

q = @(es) E^2./(E^2+es.^2);

ess = logspace(-1,3);

semilogx(ess, q(ess))
