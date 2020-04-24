% I think this is just written for the fMRI bandit data
% that or it is outdated and needs to be updated

load data/bandit_data.mat

smooth_factor = 5; 

smean = smooth(mean(ball.goodchoice,2),smooth_factor);
sstd  = smooth(std(ball.goodchoice,[],2),smooth_factor);

subplot(2,1,1);
x = 1:300;
plot(x,smean,'-b',x,smean+sstd*1.96/10,'--r',x,smean-sstd*1.96/10,'--r');

subplot(2,1,2);
imagesc(ball.goodchoice');