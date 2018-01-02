program divcurl
	implicit none
	integer(kind=16) :: i
	real,dimension(32768) :: x,y,z,norm,xv,yv,zv,div,curl,Fx,Fy,Fz,P,Q,R
	real :: dx,dy,dz

	do i = 1,32768
	  open(unit=1,file='32data.txt')
	  read(1,*) x(i),y(i),z(i),norm(i),xv(i),yv(i),zv(i)
	end do

! Calculating divergence of the vector fields

	do i = 2,32767
	  dx = x(i) - x(i-1)
	  dy = y(i) - y(i-1)
	  dz = z(i) - z(i-1)
	  Fx(i) = (xv(i+1) - xv(i-1))/(2.0)*dx
	  Fy(i) = (yv(i+1) - yv(i-1))/(2.0)*dy
	  Fz(i) = (zv(i+1) - zv(i-1))/(2.0)*dz
	  div(i) = Fx(i)+Fy(i)+Fz(i)
	write(*,*)' The gradient of the vector fields at',i,'is',div(i)
	end do

! Calculating curl of the vector fields

	do i = 2,32767
	  dx = x(i) - x(i-1)
	  dy = y(i) - y(i-1)
	  dz = z(i) - z(i-1)
	
! Motivation : Curl of the vector field F (say) is defined as follows.
! F(xv,yv,zv),curl = i(d/dy(zv)-d/dz(yv)) - j(d/dx(zv)-d/dz(xv)) + k(d/x(yv)-d/dy(xv)

 ! Here the d/dx, d/dy, d/dz are partial derivatives as I couldn't find the appropriate symbol for the same haha.

	! Firstly the i part. Let that be P.
	  P(i) = (zv(i+1) - zv(i-1))/(2.0)*dy - (yv(i+1) - yv(i-1))/(2.0)*dz
	! Now the j part. Let that be Q.
	  Q(i) = (zv(i+1) - zv(i-1))/(2.0)*dx - (xv(i+1) - xv(i-1))/(2.0)*dz
	! Now the k part. Let that be R.
	  R(i) = (yv(i+1) - yv(i-1))/(2.0)*dx - (xv(i+1) - xv(i-1))/(2.0)*dy
	curl(i) = P(i) - Q(i) + R(i) 
	write(*,*) ' The Curl of the vector fields at i = ',i, ' is ', curl(i)	
	end do

end program divcurl













	  

