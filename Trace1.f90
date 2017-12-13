!Program to calculate the trace of a 3x3 matrix by hardcoding components

program trace

 implicit none
 integer,dimension(3,3) :: mat
 integer :: i,j,trace1 =0

   mat = reshape((/1,4,7,2,5,8,3,6,9/),(/3,3/))

  trace1 = mat(1,1) + mat(2,2) + mat(3,3)
  write(*,*) ' The trace of the matrix Mat is' , trace1

end program trace
