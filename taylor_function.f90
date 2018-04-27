program Taylor_fonction
implicit none 
  real ::x
 real :: cosx,Cosc,delta,epsilo
integer :: i,n,fact
print*,'introduire le valeur de  de n '
read*,n
x=0.5
epsilo=1.E-05
Cosc=cos(0.5)
cosx=1.
      do i=1,n
	cosx=cosx+((((-1)**i)*((x)**(2*i)))/fact(i))
	enddo
	delta=abs(cosx-cos(x))
print*,' x                    Coscal            cosx             delta      iteratio  '	
write(*,*)x,Cosc,cosx,delta
    
   
   end program 
!------------------------function pour calculer le factoriel-------------------
 integer Function fact(n)
 implicit none 
 integer :: n,i,j  

 
 if (n==0) then 
 fact=1
 else
  fact=1 ;  j=1  
do i=1,2*n
fact=fact*j  
j=j+1  
 end do   
 endif
!print*,fact
endfunction 
