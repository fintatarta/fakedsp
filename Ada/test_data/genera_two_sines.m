Fc=44000;
t=0:(1/Fc):0.01;
f0=3500; f1=1e4;
x=cos(2*pi*f0*t)+cos(2*pi*f1*t);
plot(t, x)
wavwrite(x, Fc, 'two-sines.wav');

r=0.99;

num = [1, -2*cos(2*pi*f0/Fc), 1]
den = num * (r.^(0:2));

