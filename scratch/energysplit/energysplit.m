clc, clear all
clf, close all

a = 13;
xs = linspace(0,300);

f = @(x) a^2./(a^2+x.^2);

N = quad(f, 0, 300);


plot(xs,1/N*f(xs));

