clc, clear all
clf, close all

%%

lxcatdata = load('data/lxcat_clean2.dat');
format long

energydata = lxcatdata(:,1);
crossectiondata = lxcatdata(:,2);

nistdata = load('data/nist_clean.dat');

nistenergies = nistdata(:,1);
nistcrossections = nistdata(:,2)*1e-20;

%plot(energydata, crossectiondata, 'k+'), hold on
plot(nistenergies, nistcrossections, 'b+')
legend('lxcat', 'nist')
xlim([0 2e2])
ylim([0 3e-20])

%%
