! To solve the system of non-linear equations 
! Note that I've calculated the jacobians seperately as it's easy to do so here in
! the problem. However inorder to solve higher order nth dimensional problems, we 
! need to call subroutines for inverse and determinants. It might take time 
!  So here goes for nothing...

program newton_raphson2

	implicit none
	real,parameter :: epsilon = 1.0e-4
	real :: x1,y1,x0,y0,x,y,det,f1,f2
	integer :: i 
	
	x0 = 1.0
	y0 = 1.791
	i=0
	
	do 
		x1 = x0 - (( (-1.0)*f1(x0,y0)-(2.0)*y*f2(x0,y0)) / det(x0,y0) )

		y1 = y0 - (( (-2.0)*x*f1(x0,y0)+(2.0)*x*f2(x0,y0)) / det(x0,y0) )
		
		i= i+1
		if( abs(x1-x0) <= epsilon .and. abs(y1-y0) <= epsilon ) then
			exit
			end if
		y0 = y1

		x0 = x1		
			
	end do
	

	write(*,*) ' The value of the iteration is' ,i
	write(*,*) ' The value of the x and y are' ,x1,y1

end program newton_raphson2


real function f1(x,y)
	implicit none
	real,intent(in) :: x,y
	
	f1 = x*x + y*y - 4.0
end function f1

real function f2(x,y) 
	implicit none
	real,intent(in) :: x,y

	f2 = x*x - y + 1.0
end function f2

real function det(x,y)
	implicit none
	real,intent(in) :: x,y
	
	det = -(2.0)*x - (4.0)*x*y
end function det






