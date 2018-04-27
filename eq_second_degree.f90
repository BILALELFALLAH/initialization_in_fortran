 program       secoonnd_deg
        implicit none 
        real :: x0,x1,x2,x,delta,b,a,c
 3 print *, ' enter a value of b '
   read*,a
read *,b  
   print *, ' enter a value of c '
   read *, c
  delta= (b**2)-(4*a*c)  
 if (a==0) then 
 x= -c/b 
 print *, ' la solution de cette equation est ',x
 else if  (delta==0) then
x0=-b/(2*a)
 print *, ' la solution de cette equation est ',x0
else if (delta>0)  then
x1=(-b-sqrt(delta))/2*a
  x2=(-b+sqrt(delta))/2*a
 print*, ' cette equation possede deux solutions ',x1,x2
  else
 print *, ' la solution de cette equation est complexe ' 
 end if
 PRINT*, '=================================================================='
 go to 3
 end 
 
  
