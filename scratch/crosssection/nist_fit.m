clc, clear all
clf, close all

nistdata = load('data/nist_clean.dat');

edata = nistdata(:,1);
csdata = nistdata(:,2)*1e-20;

e0idx = find(edata >= 160, 1);

%%
plot(edata, csdata, 'k+'), hold on
plot(edata(e0idx)*[1 1], 3e-20*[0 1], '--g')

%%
clc

pol = @(k, degs, x) sum(repmat(k', size(x)).*(repmat(x, size(degs)).^repmat(degs, size(x))),2);
kfit = @(degs, xdata, ydata) (repmat(xdata, size(degs)).^repmat(degs, size(xdata)))\ydata;

e0idx_poly = e0idx+6;
polydegs = 0:6;
kpoly = kfit(polydegs, edata(1:e0idx_poly), csdata(1:e0idx_poly));
e_cont_poly = linspace(edata(1), edata(e0idx_poly))';
cs_cont_poly = pol(kpoly, polydegs, e_cont_poly);


e0idx_th = e0idx+12;
thdegs = [0 -1];
kth = kfit(thdegs, edata(e0idx_th:end), csdata(e0idx_th:end));
e_cont_th = linspace(edata(e0idx_th), edata(end))';
cs_cont_th = pol(kth, thdegs, e_cont_th);

plot(e_cont_poly, cs_cont_poly)
plot(e_cont_th, cs_cont_th, 'r')
