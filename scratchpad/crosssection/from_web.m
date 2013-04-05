clc
clf, close all
format long

%% Extract data and plot the data points

raw_data = load('from_web_data_cleaned.txt');

energies = raw_data(:,1);
crossections = raw_data(:,2);

n = 12;
if plots
plot(energies(1:n+1), crossections(1:n+1), 'k+'), hold on
plot(energies(n-1:end), crossections(n-1:end), 'ro')
%     axis([0 350 0 4.5e-20])
xlabel('Energy, eV')
ylabel('Cross-section, m^2')
end
%% Fit polynomial models to data

% 4-th degree polynomial fit for first part of series

qidx = 1:n+1;
eq = energies(qidx);
csq = crossections(qidx);

degsq = 0:4;

Aq = repmat(eq, 1, length(degsq)).^repmat(degsq, length(eq), 1);
kq = Aq\csq

all_eq = linspace(0, energies(n+2))';
all_csq = sum(repmat(kq', size(all_eq)).*repmat(all_eq, size(kq')).^repmat(degsq, length(all_eq), 1), 2);
if plots
    plot(all_eq, all_csq, 'g--'), hold on
end

% Linear fit for second part of series

lidx = n-1:length(energies);
el = energies(lidx);
csl = crossections(lidx);

degsl = 0:-1:-1;

Al = repmat(el, 1, length(degsl)).^repmat(degsl, length(el), 1);
kl = Al\csl

all_el = linspace(70, 350)';
all_csl = sum(repmat(kl', size(all_el)).*repmat(all_el, size(kl')).^repmat(degsl, size(all_el)),2);
if plots
    plot(all_el, all_csl, '-.'), hold on
    plot([1 1]*energies(n), [0 1], '--k')
    plot([1 1]*energies(qidx(end)), [0, 1], '--g')
    plot([1 1]*energies(lidx(1)), [0, 1], '--b')
    ylim([0 4e-20])
end

