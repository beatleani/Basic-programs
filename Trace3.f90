program trace
	implicit none
	real ,dimension(3,3) :: mat
	integer :: i=3, j=3
	real:: trac=0.0
	
		
		write(*,*) ' Enter any 9 numbers for the matrix Mat '
		  do i = 1,3
			do j=1,3
			
			read(*,*) mat(i,j)
			end do
		  end do
		 call trace3(mat,trac)
end program trace

subroutine trace3(matrix,tr)
	implicit none
	real, dimension(3,3), intent(in) :: matrix
	integer :: i,j
	real,intent(out) ::tr 
		tr=0.0
		do i=1,3
		  do j=1,3
		  if(i==j) then
		tr = tr + matrix(i,j)
		  end if
		  end do
		end do
	write(*,*) 'The trace of the matrix is' ,tr
end subroutine



