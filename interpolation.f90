program interpolation ; implicit none
real::C,Tc ; real,dimension(40)::T,Tp ; integer::i
open(unit=1,file="energie.txt")
do i=1,40 ; read(1,*)T(i),Tp(i) ; enddo ; close(1)
1 print*,'entrer la valeur de lenergie compris entre 0,5 et 20 ' ; read*,C
do i=1,40
if(C>=T(i) .and. C<=T(i+1))then
Tc=(C-Tp(i))*(Tp(i+1)-Tp(i))/(T(i+1)-T(i)) + Tp(i)
end if ; enddo
write(6,2)'le calcule de votre energie est : ',Tc
write(6,*)' |||||||||||||<veuillez encore une fois>||||||||||||||||||||||||||||||| '
2 format(2X,A34,2X,F9.5)
goto 1
end program

