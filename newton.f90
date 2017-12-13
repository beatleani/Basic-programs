program newton_raphson
	implicit none
	real,parameter :: epsilon = 1.0e-5 
	integer :: i
	real :: x1,z,f,fd,x,y,x0 = 2.0
!x0 is the initial approximation as given in the snippet

	i=0
	
	do
		x1 = x0 - (f(x0))/(fd(x0))
		i = i+1 
		z = abs((x1-x0))
		if( z <= epsilon ) then
		exit
		end if
		x0 = x1
	end do

	write(*,*) ' Total iterations =' ,i
	write(*,*) ' Final root value =' ,x1
	
end program newton_raphson


real function f(x) 
	implicit none
	real,intent(in) :: x
	
	f = 2.0*sin(x) - x
end function f

real function fd(y)  
	implicit none
	real, intent(in) :: y
	
	fd = 2.0*cos(y) - 1
end function fd
	














