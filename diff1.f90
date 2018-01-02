! Note : The compiler only shows part of the output in the terminal due to the large number of lines. Inorder to observe each value of the output, we can co



program differentiation

	implicit none
	integer(kind=16) ::i
	real,dimension(32768) :: x,y,z,norm,xv,yv,zv,Fx,Fy,Fz,F2x,F2y,F2z
	real :: dx,dy,dz

	do i = 1,32768
	  open(unit=1,file='32data.txt')
	  read(1,*) x(i),y(i),z(i),norm(i),xv(i),yv(i),zv(i)
	end do
	
	 

	! Finding the partial derivative at each point (x,y,z) 

	do i = 2,32767
	  dx = x(i) - x(i-1)
	  Fx(i) = (norm(i+1) - norm(i-1))/(2.0)*dx	! neglecting error terms   
	end do

	do i = 2,32767
	  dy = y(i) - y(i-1)
	  Fy(i) = (norm(i+1) - norm(i-1))/(2.0)*dy
	end do 

	do i = 2,32767
	  dz = z(i) - z(i-1)
	  Fz(i) = (norm(i+1) - norm(i-1))/(2.0)*dz
	end do
	
	do i = 2,32767
	  dx = x(i) - x(i-1)
	  F2x(i) = (norm(i+1) - (2.0)*norm(i) + norm(i-1) ) / (dx**2)
	end do 


	do i = 2,32767
	  dy = y(i) - y(i-1)
	  F2y(i) = (norm(i+1) - (2.0)*norm(i) + norm(i-1) ) / (dy**2)
	end do

	do i = 2,32767
	  dz = z(i) - z(i-1)
	  F2z(i) = (norm(i+1) - (2.0)*norm(i) + norm(i-1) ) / (dz**2)
	end do

	do i = 2,32768
	
	write(*,*)'The first partial derivatives w.r.t x Fx = ',Fx(i)

	end do	
	
	do i = 2,32768
	
	write(*,*)'The second partial derivatives w.r.t x F2x = ',F2x(i)

	end do	

		
	do i = 2,32768
	
	write(*,*)'The first partial derivatives w.r.t y Fy = ',Fy(i)

	end do	

	do i = 2,32768
	
	write(*,*)'The second partial derivatives w.r.t y F2y = ',F2y(i)

	end do	

	do i = 2,32768
	
	write(*,*)'The second partial derivatives w.r.t z F2z = ',F2z(i)

	end do	
	
	do i = 2,32768
	
	write(*,*)'The first partial derivatives w.r.t z Fz = ',Fz(i)

	end do
	
	

end program differentiation





