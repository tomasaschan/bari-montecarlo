clc, clear all
clf, close all

Bdata = load('data/georgio/N2+B_1.txt');
C1data = load('data/georgio/N2C_1.txt');
C2data = load('data/georgio/N2C_2.txt');


figure, hold on
plot(Bdata(:,1), Bdata(:,2)*1e-4,'k+')
plot(C1data(:,1), C1data(:,2)*1e-4, 'b+')
plot(C2data(:,1), C2data(:,2)*1e-4, 'g+')

save 'data/georgio/N2+B_1-cleaned.txt' 'Bdata' -ascii -double