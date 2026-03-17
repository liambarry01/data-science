1;

clc
clear all
close all

pkg load io

X1_data = dlmread('data.csv','',['A7..A28']);
Y1_data = dlmread('data.csv','',['B7..B28']);
X1 = X1_data.';
Y1 = Y1_data.';

X2_data = dlmread('data.csv','',['D7..D28']);
Y2_data = dlmread('data.csv','',['E7..E28']);
X2 = X2_data.';
Y2 = Y2_data.';

X3_data = dlmread('data.csv','',['G7..G28']);
Y3_data = dlmread('data.csv','',['H7..H28']);
X3 = X3_data.';
Y3 = Y3_data.';

function dydx = derive(x, y)

% Derive function calculates the derivative of a function that is given by a set of data points. 
% The derivatives at the first and last points are calculated by using the forward and backward finite difference formula, respectively.
% The derivative at all the other points is calculated by the central finite difference formula.

% Input variables:
% x     A vector with the x values of the data points.
% y     A vector with the y values of the data points.

% Output variable:
% dydx    A vector with the value of the derivative at each point.

n = length(x);

dydx = zeros(1, n);

for i = 1:n
    
    % forward difference
    if i == 1
        dydx(i)=(y(i+1)-y(i))/(x(i+1)-x(i));

    % central difference
    for j = 2:n-1
        dydx(j)=(y(j+1)-y(j-1))/(x(j+1)-x(j-1));
    end
        
    % backward difference
    elseif i == n 
        dydx(i)=(y(i)-y(i-1))/(x(i)-x(i-1));
        
    end
end

end


% Calling Derive function and producing relevant output for young player - Caleb Serong
fprintf('----------Caleb Serong----------\n')
fprintf('Initial Data:\n')
disp(Y1.')
plot(X1, Y1, 'color', [0 0.5 1]); grid
xlabel('Games Played During Season');
ylabel('Player Rating');
title("Player Rating throughout Games in Season 2021 for Caleb Serong");
figure
fprintf('Rate of Change Results:\n')
dydx1 = derive(X1, Y1).'
plot(X1, dydx1, 'color', [0 0.5 1]); grid
xlabel('Games Played During Season');
ylabel('Rate of Change of Player Rating');
title({"Rate of Change of Player Rating throughout Games in Season 2021"; "for Caleb Serong"});
figure
fprintf('Moving Average Rate of Change Results:\n')
M1 = movmean(dydx1, 22)
plot(X1, M1, 'color', [0 0.5 1]); grid
xlabel('Games Played During Season');
ylabel('Moving Average of Rate of Change of Player Rating');
title({"Moving Average of Rate of Change of Player Rating throughout"; "Games in Season 2021 for Caleb Serong"});
figure

% Calling Derive function and producing relevant output for peak player - Ollie Wines
fprintf('----------Ollie Wines----------\n')
fprintf('Initial Data:\n')
disp(Y2.')
plot(X2, Y2, 'color', [0 0.6 0.3]); grid
xlabel('Games Played During Season');
ylabel('Player Rating');
title('Player Rating throughout Games in Season 2021 for Ollie Wines');
figure
fprintf('Rate of Change Results:\n')
dydx2 = derive(X2, Y2).'
plot(X2, dydx2, 'color', [0 0.6 0.3]); grid
xlabel('Games Played During Season');
ylabel('Rate of Change of Player Rating');
title({"Rate of Change of Player Rating throughout Games in Season 2021"; "for Ollie Wines"});
figure
fprintf('Moving Average Rate of Change Results:\n')
M2 = movmean(dydx2, 22)
plot(X2, M2, 'color', [0 0.6 0.3]); grid
xlabel('Games Played During Season');
ylabel('Moving Average of Rate of Change of Player Rating');
title({"Moving Average of Rate of Change of Player Rating throughout"; "Games in Season 2021 for Ollie Wines"});
figure

% Calling Derive function and producing relevant output for old player - Joel Selwood
fprintf('----------Joel Selwood----------\n')
fprintf('Initial Data:\n')
disp(Y3.')
plot(X3, Y3, 'color', [1 0.2 0.2]); grid
xlabel('Games Played During Season');
ylabel('Player Rating');
title('Player Rating throughout Games in Season 2021 for Joel Selwood');
figure
fprintf('Rate of Change Results:\n')
dydx3 = derive(X3, Y3).'
plot(X3, dydx3, 'color', [1 0.2 0.2]); grid
xlabel('Games Played During Season');
ylabel('Rate of Change of Player Rating');
title({"Rate of Change of Player Rating throughout Games in Season 2021"; "for Joel Selwood"});
figure
fprintf('Moving Average Rate of Change Results:\n')
M3 = movmean(dydx3, 22)
plot(X3, M3, 'color', [1 0.2 0.2]); grid
xlabel('Games Played During Season');
ylabel('Moving Average of Rate of Change of Player Rating');
title({"Moving Average of Rate of Change of Player Rating throughout"; "Games in Season 2021 for Joel Selwood"});
figure

% Combining the plots of the 3 players on the same axes for the initial tabulated data
fprintf('----------Combined Results for the Three Players----------\n')
plot(X1, Y1, 'color', [0 0.5 1]); grid
hold on
plot(X2, Y2, 'color', [0 0.6 0.3]); grid
hold on
plot(X3, Y3, 'color', [1 0.2 0.2]); grid
xlabel('Games Played During Season');
ylabel('Player Rating');
title({"Player Rating throughout Games in Season 2021"; "for Three Differing Age Demographics"});
legend("Young", "Peak", "Old");
figure

% Combining the plots of the 3 players on the same axes for the rate of change results
plot(X1, dydx1, 'color', [0 0.5 1]); grid
hold on
plot(X2, dydx2, 'color', [0 0.6 0.3]); grid
hold on
plot(X3, dydx3, 'color', [1 0.2 0.2]); grid
xlabel('Games Played During Season');
ylabel('Rate of Change of Player Rating');
title({"Rate of Change of Player Rating throughout Games"; "in Season 2021 for Three Differing Age Demographics"});
legend("Young", "Peak", "Old");
figure

% Combining the plots of the 3 players on the same axes for the moving average rate of change results
plot(X1, M1, 'color', [0 0.5 1]); grid
hold on
plot(X2, M2, 'color', [0 0.6 0.3]); grid
hold on
plot(X3, M3, 'color', [1 0.2 0.2]); grid
xlabel('Games Played During Season');
ylabel('Moving Average of Rate of Change of Player Rating');
title({"Moving Average of Rate of Change of Player Rating throughout"; "Games in Season 2021 for Three Differing Age Demographics"});
legend("Young", "Peak", "Old");