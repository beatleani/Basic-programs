! To solve a system of linear equations. Here I used the concept of crammer's rule
! For simplicity's sake, I've used the entries of the matrix you've given, professor



program crammer
	
	implicit none
	integer,parameter :: n=3
	integer ::i,j
	real,dimension(n,n,n+1) :: a
        real,dimension(n,1) :: b,x
	
	a(:,:,1) = reshape((/-1,3,-1,1,-1,3,2,1,4/),(/n,n/))
	b = reshape((/2,6,4/),(/n,1/))


	call cram(x,a,b,n)
	
	write(*,*) 'Matrix A|b:'
	do i=1,n
		write(*, '("|")',advance='no')
	do j=1,n
		write(*,'(f8.2,t2)',advance='no'),a(i,j,1)
	end do
	write(*,'(" | ",f8.2)'),b(i,1)
	end do
	
	write(*,*)'Matrix Xa:'
	do i=1,n
		do j=1,n
		  write(*,'(f8.2,t2)',advance='no'),a(i,j,2) 
		end do
		write(*,*)
	end do
	
	write(*,*)'Matrix Xb:'
	do i=1,n
		do j=1,n
		  write(*,'(f8.2,t2)',advance='no'),a(i,j,3) 
		end do
		write(*,*)
	end do


	write(*,*)'Matrix Xc:'
	do i=1,n
		do j=1,n
		  write(*,'(f8.2,t2)',advance='no'),a(i,j,4) 
		end do
		write(*,*)
	end do

	write(*,*) ' The solution is:'
	do i=1,n
	  write(*,*) x(i,1)
	end do
end program crammer

subroutine cram(x,a,b,n)

	implicit none
	integer,intent(in) :: n
	real,dimension(n,n,n+1), intent(inout) :: a
	real,intent(in),dimension(n) :: b
	real,external :: detf
	integer :: i
	real,dimension(n,1),intent(out) :: x

	do i=1,n
		a(:,:,i+1) = a(:,:,1)
		a(:,i,i+1) = b
		x(i,1) = detf(a(:,:,i+1),n)/detf(a(:,:,1),n)
	end do
end subroutine cram

recursive real function detf(mat,n) result(det)

	implicit none
	integer,intent(in) :: n
	real,intent(in),dimension(n,n) :: mat
	real,dimension(n-1,n-1) :: sl
	integer :: i

	det = 0
	
	if(n==1) then
	det = mat(1,1)
	return
	else
		do i=1,n
		 call slicef(sl,mat,n,1,i)
		 det = det + ((-1.0)**(1+i))*mat(1,i)*detf(sl,n-1)
		end do
		return
	end if

end function detf


subroutine slicef(sl,mat,n,row,column)

	implicit none
	integer,intent(in) :: n,row,column
	real,dimension(n,n),intent(in) :: mat
	real,dimension(n-1,n-1),intent(out) :: sl
	logical,dimension(n,n) :: mask

	mask = .true.
	mask(row,:) = .false.
	mask(:,column) = .false.

	sl = reshape(pack(mat,mask),(/n-1,n-1/))

end subroutine slicef


	









