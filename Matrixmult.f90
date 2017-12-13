program matrix_multiplication
	implicit none
	real,allocatable,dimension(:,:) :: a,b,c
	integer :: m1,n1,m2,n2,i,j,k

	   write(*,*)' Enter the number of rows and columns of the matrix A and B respectively'
	   read(*,*) m1,n1,m2,n2
	   	
		if(n1==m2) then
		write(*,*)'Matrices can be multiplied'

           allocate(a(m1,n1))
	write(*,*) 'Enter the entries for the matrix A'
	do i=1,m1
	  do j=1,n1
		
		read(*,*) a(i,j)
	  end do
	end do
	   
           allocate(b(m2,n2))
	write(*,*) 'Enter the entries for the matrix B'
	do i=1,m2
	  do j=1,n2
		
		read(*,*) b(i,j)
	  end do
	end do
			else
				write(*,*)" Matrices can't be multiplied! "
		end if

	   allocate(c(m1,n2))
		do i=1,m1
			do j=1,n2
			c(i,j)=0
			  do k=1,n1
			  c(i,j) = c(i,j) + (a(i,k)*b(k,j))
			  end do
			end do
		end do

	write(*,*) 'Matrix C is'
	do i=1,m1
		do j=1,n2
		write(*,*) c(i,j)
		end do
	end do
     deallocate(a)
     deallocate(b)

end program matrix_multiplication






			









