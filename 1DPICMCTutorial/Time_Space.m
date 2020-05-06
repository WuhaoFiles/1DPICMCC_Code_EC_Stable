clc;clear all;close all;format long;

y=importdata('Grid2D       21001 .dat',' ',0);
n=65;T=101;
a=linspace(0,0.02,n);%right boundary of 1 dimension £¨0,0.02£©
b=linspace(0,2,T);
film=char('1Electron Density','1Current','1Electron Temperature','1Electronic Heating Rate',...
          '1Ar+Density','1Ar+Current','1Ar+temperature','1Ar+Heating Rate',...
          '1Electric Field','1Potential','1Charge');


m=0;
for i0=1:13;
    for k=1:101;
        for i=1:65;
            cc(k,i,i0)=y(i+m,i0);
        end
        m=m+i;
            if abs(m-n*T)==0;
                break;
            end
    end
    m=0;
end

for i=1:11;
    figure(i),surf(cc(:,:,1),cc(:,:,2),cc(:,:,i+2)),grid off,...
        title(film(i,:)),xlabel('space'),ylabel('time'),colorbar
end
