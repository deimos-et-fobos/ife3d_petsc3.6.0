load fort.89
nt=size(fort,1);
a=fort(1:nt,2:7);
tri=zeros(nt,3);
coords=zeros(nt*3,3);
for i=1:nt
 tri(i,1:3)=[(i-1)*3+1 (i-1)*3+2 (i-1)*3+3];
 coords((i-1)*3+1:i*3,1:2)=[a(i,1:2);a(i,3:4);a(i,5:6)];
endfor
triplot(tri,coords(:,1),coords(:,2))
axis([-0.1,1.1,-0.1,1.1])
for i=1:nt
      area(i)=((a(i,3)-a(i,1))*(a(i,6)-a(i,2))-(a(i,5)-a(i,1))*(a(i,4)-a(i,2)))/2;
      %sum=sum+area(i);
end
sum(area)

load fort.87
nt=size(fort,1);
a=fort(1:nt,2:10);
tri=zeros(nt,3);
coords=zeros(nt*3,3);
for i=1:nt
 tri(i,1:3)=[(i-1)*3+1 (i-1)*3+2 (i-1)*3+3];
 coords((i-1)*3+1:i*3,1:3)=[a(i,1:3);a(i,4:6);a(i,7:9)];
endfor
trimesh(tri,coords(:,1),coords(:,2),coords(:,3))
axis([-1.1,1.1,-1.1,1.1,-0.1,3.6])
v1=a(:,4:6)-a(:,1:3);
v2=a(:,7:9)-a(:,1:3);
for i=1:nt
      area(i)=norm(cross(v1(i,:),v2(i,:)))/2;
      %sum=sum+area(i);
end
sum(area)

cont1=0;cont2=0;cont3=0;cont4=0;sum1=0;sum2=0;sum3=0;sum4=0;
for i=1:nt
   if(a(i,1)>=0.5&&a(i,3)>=0.5&&a(i,5)>=0.5)
   	if(a(i,2)>=0.5&&a(i,4)>=0.5&&a(i,6)>=0.5)
         cont3=cont3+1;
         area3(cont3)=((a(i,3)-a(i,1))*(a(i,6)-a(i,2))-(a(i,5)-a(i,1))*(a(i,4)-a(i,2)))/2;
         sum3=sum3+area3(cont3);
      else
         cont4=cont4+1;
         area4(cont4)=((a(i,3)-a(i,1))*(a(i,6)-a(i,2))-(a(i,5)-a(i,1))*(a(i,4)-a(i,2)))/2;
         sum4=sum4+area4(cont4);
      endif
   else
   	if(a(i,2)>=0.5&&a(i,4)>=0.5&&a(i,6)>=0.5)
   	   cont2=cont2+1;
         area2(cont2)=((a(i,3)-a(i,1))*(a(i,6)-a(i,2))-(a(i,5)-a(i,1))*(a(i,4)-a(i,2)))/2;
         sum2=sum2+area2(cont2);
         if (i>163)
%         axis([-0.1,1.1,-0.1,1.1])
%         triplot(tri(i,:),coords(:,1),coords(:,2));
%         hold on;
%         pause(0.5);
         endif
      else
         cont1=cont1+1;
         area1(cont1)=((a(i,3)-a(i,1))*(a(i,6)-a(i,2))-(a(i,5)-a(i,1))*(a(i,4)-a(i,2)))/2;
         sum1=sum1+area1(cont1);
      endif
   endif
end
sum1
sum2
sum3
sum4



