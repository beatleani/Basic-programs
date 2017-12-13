! To calculate trace by opening a file and reading the values and using DO loops

program trace

	implicit none
	integer, dimension(3,3) :: mat
	integer :: i=3,j=3,trace2=0
	open(unit=1,file='file.txt')
		
		do i=1,3
		  do j=1,3
		  read(1,*) mat(i,j)
		  end do
		end do 
	close(unit=1)
	do i=1,3
	  do j=1,3
	  if( i==j) then
	trace2 = trace2 + mat(i,j)
	  end if
	  end do
	end do
      write(*,*) ' The trace of the matrix is' ,trace2

end program trace
