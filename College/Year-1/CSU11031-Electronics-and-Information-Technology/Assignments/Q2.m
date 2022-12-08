t=0:0.1:1;% using time domain t from question 1
sum=zeros(1,length(t));% sum same length as t 
k=1; % we adjust this value of k to represent the frequency of summation 

sum=sum+4/pi*sin(2*pi*k*t)/k; % sum equation

plot(t,sum,'-');% we plot our graph 
hold on; 
c=square(2*pi*t); % we plot our square wave
plot(t,c,'-');% we plot our square wave in red for reference












