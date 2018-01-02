program differentiation

	implicit none
	real,allocatable :: y(:), dydx(:)
	integer ::n,i
	real ::x,dx,f
	
	write(*,*)'Enter number of grid points :'
	read(*,*) n
	allocate(y(n),dydx(n))
	
	dx = 10.0/(n-1)
	do i = 0,n
		x = (i)*dx
		y(i) = f(x)
	end do
	
	call derivative (y,n,dx,dydx)

	do i =0,n-1
	x = (i)*dx
	write(*,*) dydx(i), 2*x, 2*x - dydx(i)
	end do	
	
	deallocate(y,dydx)
contains

	subroutine derivative (a,np,h,aprime)
	  integer,intent(in) :: np
	  real ,intent(in) :: a(np),h
	  real ,intent(out) :: aprime(np)
	  integer :: i
	
	do i = 1,np-1
		aprime(i) = (a(i+1)-a(i-1))/2*h
	end do
	

	end subroutine derivative

end program differentiation

real function f(x)

	real,intent(in) :: x
	f = x*x             ! Could be anything 
end function






	
