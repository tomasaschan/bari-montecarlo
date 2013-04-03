clc
clf, close all

%% Extract data and plot the data points

raw_data = load('from_web_data_cleaned.txt');

energies = raw_data(:,1);
crossections = raw_data(:,2);

n = 10;
if plots
for i = 1:2
    subplot(1,3,i)
    plot(energies(1:n+2), crossections(1:n+2), 'k+'), hold on
    plot(energies(n:end), crossections(n:end), 'ro')
%     axis([0 350 0 4.5e-20])
    xlabel('Energy, eV')
    ylabel('Cross-section, m^2')
end
end
%% Fit polynomial models to data

if plots
    subplot(1,3,2)
end
% 4-th degree polynomial fit for first part of series

qidx = 1:n+2;
eq = energies(qidx);
csq = crossections(qidx);

degsq = 0:4;

Aq = repmat(eq, 1, length(degsq)).^repmat(degsq, length(eq), 1);
kq = Aq\csq;

all_eq = linspace(0, energies(n+2))';
all_csq = sum(repmat(kq', size(all_eq)).*repmat(all_eq, size(kq')).^repmat(degsq, length(all_eq), 1), 2);
if plots
    plot(all_eq, all_csq, 'g--')
end

% Linear fit for second part of series

qidx = n:length(energies);
el = energies(qidx);
csl = crossections(qidx);

degsl = 0:1;

Al = repmat(el, 1, length(degsl)).^repmat(degsl, length(el), 1);
kl = Al\csl;

all_el = linspace(70, 350)';
all_csl = sum(repmat(kl', size(all_el)).*repmat(all_el, size(kl')).^repmat(degsl, size(all_el)),2);
if plots
    plot(all_el, all_csl, '-.')
end
%% Plot collision time vs energy
if plots
subplot(1,3,3)

n = 1e25;
end
