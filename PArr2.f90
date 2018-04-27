Program pvr_arret
Implicit none
Character(LEN=2),dimension(14) :: Sym
Real,dimension(14) :: M,Z 
Integer :: T,j ; Real :: PA
 Open (unit=2,file='masseZcible.txt')
   Do j=1,10
   Read(2,*)Sym(j),M(j),Z(j)
   Enddo ; close(2)
   Open(unit=3,file='resultatsPa1.txt')

Do T=2,10
Write(6,*)'Pour l energie des alph T = ',T, '   MeV' 
Write(6,*)'----------------------------------------------'
Write(6,*)'|   Symbole  |     Masse     |      PA       |'
Write(3,*)'Pour energie d alpha T = ',T, '  MeV' 
Write(3,*)'----------------------------------------------'
Write(3,*)'|   Symbole  |     Masse     |      PA       |'
  Do j=1,10
 
Call g(T,Z,M,j,PA)
Write(6,*)'----------------------------------------------'
Write(6,10)sym(j),M(j),PA
Write(3,*)'----------------------------------------------'
Write(3,10)sym(j),M(j),PA
 Enddo
Enddo
10 format(1X,'|',1X,A10,1X,'|',1X,F13.2,1X,'|',1X,F13.4,1X,'|')
close(3) ; end
      !---------------------------------------------------------------------
Subroutine g(T,Z,M,j,PA)
Implicit none
Real,dimension(14) :: M,Z
Real I,Te,N,e,A,B,C
real, intent(out) :: PA
Integer :: z1,T,j
Te=0.511              !en MeV m0*c^2
z1=2             
N=6.022E23
e=(197E-13)/137      !en (Mev.cm)

  A=4*3.14*(z1**2)*(e**2)*N*Z(j)
  B=Te*(1-((4*931.5)/(T+4*931.5))**2)
  C=log(2*B*10**(6)/I(Z(j)))
  PA=(A/(B*M(j)))*C       ; end 
      !---------------------------------------------------------------------
Real function I(Zc)
Implicit none
Real :: Zc

   if(Zc<=13) then ; I=(12+7/Zc)*Zc
    else  ;  I=Zc*(9.76+(58*Zc**(-1.19))) ; end if
End function
