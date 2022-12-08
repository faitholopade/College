n = 500; % change based on number of sine waves

frequency=1:1:2*n; % frequecny domain
AMP=(4/pi)./(1:2:2*n); % amplitude of sine wave
even = zeros(size(AMP).*[1,2]);
even(1:2:end) = AMP;

stem(frequency,even) % we plot our graph














