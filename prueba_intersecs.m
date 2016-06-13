coord=zeros(12,3);
elem=[1,2,3;4,5,6;7,8,9;10,11,12];

x1s=0.4;y1s=0.3;z1s=0.2;
x2s=1;y2s=1;z2s=1;
x3s=2;y3s=1.5;z3s=0.6;
x1f=0.45;y1f=0.35;z1f=0.18;
x2f=1;y2f=0.97;z2f=1.01;
x3f=2.03;y3f=1.48;z3f=0.61;

xs(1)=x1s; xs(2)=x2s; xs(3)=x3s;
ys(1)=y1s; ys(2)=y2s; ys(3)=y3s;
zs(1)=z1s; zs(2)=z2s; zs(3)=z3s;
xf(1)=x1f; xf(2)=x2f; xf(3)=x3f;
yf(1)=y1f; yf(2)=y2f; yf(3)=y3f;
zf(1)=z1f; zf(2)=z2f; zf(3)=z3f;

coord(1:3,:)=[xs' ys' zs'];
coord(4:6,:)=[xf' yf' zf'];

%     Calculo las normales de los 2 triangulos 
nus(1)=(y2s-y1s)*(z3s-z1s)-(z2s-z1s)*(y3s-y1s);
nus(2)=(z2s-z1s)*(x3s-x1s)-(x2s-x1s)*(z3s-z1s);
nus(3)=(x2s-x1s)*(y3s-y1s)-(y2s-y1s)*(x3s-x1s);
norms =sqrt(nus(1)^2+nus(2)^2+nus(3)^2);
nuf(1)=(y2f-y1f)*(z3f-z1f)-(z2f-z1f)*(y3f-y1f);
nuf(2)=(z2f-z1f)*(x3f-x1f)-(x2f-x1f)*(z3f-z1f);
nuf(3)=(x2f-x1f)*(y3f-y1f)-(y2f-y1f)*(x3f-x1f);
normf =sqrt(nuf(1)^2+nuf(2)^2+nuf(3)^2);
norm_max=norms;
if(normf>norm_max) 
norm_max=normf;
endif

eq_n=0;
max_dir=1;
for i=1:3
nus(i)=nus(i)/norms;
nuf(i)=nuf(i)/normf;
eq_n=eq_n+nus(i)*nuf(i);
if(nuf(i)>nuf(max_dir)) 
max_dir=i;
endif
endfor
if(eq_n>0.95) i=1:100000

for i=1:3
nup(i)=(nus(i)+nuf(i))/2.d0;
endfor

Dp = -(nup(1)*(x1s+x2s+x3s+x1f+x2f+x3f)+nup(2)*(y1s+y2s+y3s+y1f+y2f+y3f)+nup(3)*(z1s+z2s+z3s+z1f+z2f+z3f))/6;

h = nup(1)+nup(2)+nup(3);
for i=1:3
rho = (nup(1)*xs(i)+nup(2)*ys(i)+nup(3)*zs(i));
xs(i)=xs(i)-rho*nup(1);
ys(i)=ys(i)-rho*nup(2);
zs(i)=zs(i)-rho*nup(3);
rho = (nup(1)*xf(i)+nup(2)*yf(i)+nup(3)*zf(i));
xf(i)=xf(i)-rho*nup(1);
yf(i)=yf(i)-rho*nup(2);
zf(i)=zf(i)-rho*nup(3);
endfor

coord(7:9,:)=[xs' ys' zs'];
coord(10:12,:)=[xf' yf' zf'];

figure(1);
trimesh(elem(1:2,:),coord(:,1),coord(:,2),coord(:,3));
figure(2);
trimesh(elem(3:4,:),coord(:,1),coord(:,2),coord(:,3));
figure(3);
trimesh(elem(1:4,:),coord(:,1),coord(:,2),coord(:,3));



dd=Dp+0.001;(nup(1)*x1s+nup(2)*y1s+nup(3)*z1s+dd)^2+(nup(1)*x2s+nup(2)*y2s+nup(3)*z2s+dd)^2+(nup(1)*x3s+nup(2)*y3s+nup(3)*z3s+dd)^2+(nup(1)*x1f+nup(2)*y1f+nup(3)*z1f+dd)^2+(nup(1)*x2f+nup(2)*y2f+nup(3)*z2f+dd)^2+(nup(1)*x3f+nup(2)*y3f+nup(3)*z3f+dd)^2
dd=Dp+0.0;(nup(1)*x1s+nup(2)*y1s+nup(3)*z1s+dd)^2+(nup(1)*x2s+nup(2)*y2s+nup(3)*z2s+dd)^2+(nup(1)*x3s+nup(2)*y3s+nup(3)*z3s+dd)^2+(nup(1)*x1f+nup(2)*y1f+nup(3)*z1f+dd)^2+(nup(1)*x2f+nup(2)*y2f+nup(3)*z2f+dd)^2+(nup(1)*x3f+nup(2)*y3f+nup(3)*z3f+dd)^2
dd=Dp-0.001;(nup(1)*x1s+nup(2)*y1s+nup(3)*z1s+dd)^2+(nup(1)*x2s+nup(2)*y2s+nup(3)*z2s+dd)^2+(nup(1)*x3s+nup(2)*y3s+nup(3)*z3s+dd)^2+(nup(1)*x1f+nup(2)*y1f+nup(3)*z1f+dd)^2+(nup(1)*x2f+nup(2)*y2f+nup(3)*z2f+dd)^2+(nup(1)*x3f+nup(2)*y3f+nup(3)*z3f+dd)^2

