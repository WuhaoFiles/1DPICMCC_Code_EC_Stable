%y=importdata('Particle_Trajectory_Information.dat');

y1 = textread('Particle_Trajectory_Information.dat','','headerlines',0);
y2 = textread('Diag Particle In Field.dat','','headerlines',0);
film1=char('e1','e2','Ar+1','Ar+2');
film2=char('X','Vx','Vy','Vz', 'Ax','Energy');
X1= y1(:,1);
X2= y2(:,1);
for i=2:5
  fname1=film1(i-1,:);
  figure(i),plot(X1,y1(:,i)),title(fname1),xlabel('time'),ylabel('Position');
  % saveas(figure(i),char(fname),'jpg')
end
for i=2:7
 fname2=film2(i-1,:);
 figure(i+4),plot(X2,y2(:,i)),title(fname2),xlabel('time'),ylabel('fname2');
% saveas(figure(i),char(fname),'jpg')
end
