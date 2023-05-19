close all
clear variables
clc

M = [0 1 0 1 1;
     1 0 1 0 0;
     0 1 0 1 0;
     1 0 1 0 0;
     1 0 0 0 0];

G = graph(M);
p = plot(G);
labelnode(p, [1 2 3 4 5], {'Age', 'RestingBP', 'Cholesterol', 'MaxHR', 'Oldpeak'})
saveas(gcf,'igraph.png')