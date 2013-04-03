clc, clear all
clf, close all

Es = [2e1 5e1 2e2]';
sigmas = [1e-17 2e-16 2e-16]';
loglog(Es, sigmas, 'k+'), hold on

A = [ones(length(Es),1) log(Es) log(Es).^2];
K = A\log(sigmas);

xlim([1e1 1e3])

sigma = @(E) exp(K(1)+K(2)*log(E)+K(3)*log(E).^2);

ylim([7e-18 1e-14])

allE = logspace(1,3);
allsigma = sigma(allE);

loglog(allE, allsigma)
xlabel('Energy, eV')
ylabel('Ionization cross section, cm^2')

figure

energies = linspace(100, 300, 1000);
mert=sqrt(9.11E-31);
ni = 1e25;
sigmas = exp(K(1)+K(2)*log(energies)+K(3)*log(energies).^2)*1e-4;
e = 1.6022e-19;

tcs = -log(rand(size(energies))).*mert./(ni*sigmas.*sqrt(2*energies*e))*1e12;
clen = 100;
plot(energies, conv(tcs/clen, ones(1,clen), 'same'))
xlabel('Energy, eV')
ylabel('Typical collision time, ps')