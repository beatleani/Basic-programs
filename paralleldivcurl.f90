program divcurlparallel
	
	use mpi
	integer(kind = 16) :: i
	integer :: myid,nprocs,chunksize
	real,dimension(134217728) :: x,y,z,norm,xv,yv,zv,div,curl,Fx,Fy,Fz,P,Q,R
	real :: dx,dy,dz
	double precision :: t1,t2,t,Time
	character*50  :: file_num
	character*50  :: filename

	call cpu_time(t1)       ! Starts Computation time
	call MPI_INIT(ierr)      ! Initialize MPI
	call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)

	Time = 2.00
	t = 0.0
	chunksize = 1000
	
	do i = 1,134217727
	  open(unit=1,file='512data.txt')
	  read(1,*) x(i),y(i),z(i),norm(i),xv(i),yv(i),zv(i)
	  dx = x(i) - x(i-1)
	  dy = y(i) - y(i-1)
	  dz = z(i) - z(i-1)
	end do

	! Time Loop begins

	do while(t <= Time)

	! Processor 0
	
	if(myid == 0) then
	
	
	if(t == 0.0) then

	  Fx(i) = (xv(i+1) - xv(i-1))/(2.0)*dx
	  Fy(i) = (yv(i+1) - yv(i-1))/(2.0)*dy
	  Fz(i) = (zv(i+1) - zv(i-1))/(2.0)*dz
	  div(i) = Fx(i)+Fy(i)+Fz(i)	
	
	! Motivation : Curl of the vector field F (say) is defined as follows.
! F(xv,yv,zv),curl = i(d/dy(zv)-d/dz(yv)) - j(d/dx(zv)-d/dz(xv)) + k(d/x(yv)-d/dy(xv)

 ! Here the d/dx, d/dy, d/dz are partial derivatives as I couldn't find the appropriate symbol for the same.

	! Firstly the i part. Let that be P.
	  P(i) = (zv(i+1) - zv(i-1))/(2.0)*dy - (yv(i+1) - yv(i-1))/(2.0)*dz
	! Now the j part. Let that be Q.
	  Q(i) = (zv(i+1) - zv(i-1))/(2.0)*dx - (xv(i+1) - xv(i-1))/(2.0)*dz
	! Now the k part. Let that be R.
	  R(i) = (yv(i+1) - yv(i-1))/(2.0)*dx - (xv(i+1) - xv(i-1))/(2.0)*dy
	curl(i) = P(i) - Q(i) + R(i) 
	end if 
	end if
	
	end do
	
	if((0.0<=t - 2e-04 .AND. t - 2e-04 <= 1e-06).OR.(0.0 <= t - 1.9995e-03)) then
	write (file_num,*) t
	filename= 'solution_0'//trim(file_num)//'.dat'
	open (unit = 12, file = filename ,status='replace')
	write(12,*) 'TITLE="Solution for curl and divergence"'
	write(12,*) "ZONE I=",ChunkSize,"F=Point"

	do i = 1, chunksize  
	write(12,*) curl(i),div(i)
	end do
	close (12)								
	end if
 
	

	! Processor 1

	if (myid .eq. 1) then
	if(t == 0.0) then
  
	do i=1,ChunkSize
	
	Fx(i) = (xv(i+1) - xv(i-1))/(2.0)*dx
	  Fy(i) = (yv(i+1) - yv(i-1))/(2.0)*dy
	  Fz(i) = (zv(i+1) - zv(i-1))/(2.0)*dz
	  div(i) = Fx(i)+Fy(i)+Fz(i)	
	
	! Motivation : Curl of the vector field F (say) is defined as follows.
! F(xv,yv,zv),curl = i(d/dy(zv)-d/dz(yv)) - j(d/dx(zv)-d/dz(xv)) + k(d/x(yv)-d/dy(xv)

 ! Here the d/dx, d/dy, d/dz are partial derivatives as I couldn't find the appropriate symbol for the same.

	! Firstly the i part. Let that be P.
	  P(i) = (zv(i+1) - zv(i-1))/(2.0)*dy - (yv(i+1) - yv(i-1))/(2.0)*dz
	! Now the j part. Let that be Q.
	  Q(i) = (zv(i+1) - zv(i-1))/(2.0)*dx - (xv(i+1) - xv(i-1))/(2.0)*dz
	! Now the k part. Let that be R.
	  R(i) = (yv(i+1) - yv(i-1))/(2.0)*dx - (xv(i+1) - xv(i-1))/(2.0)*dy
	curl(i) = P(i) - Q(i) + R(i) 

	end do
	end if

	
	
	
	if((0.0<=t - 2e-04 .AND. t - 2e-04 <= 1e-06).OR.(0.0 <= t - 1.9995e-03)) then
	write (file_num,*) t
	filename= 'solution_0'//trim(file_num)//'.dat'
	open (unit = 13, file = filename ,status='replace')
	write(13,*) 'TITLE="Solution for curl and divergence"'
	write(13,*) "ZONE I=",ChunkSize,"F=Point"

	do i = 1, chunksize  
	write(13,*) curl(i),div(i)
	end do
	close (13)								
	end if
 	end if
	

	write(*,*) time, nprocs, myid ! Validation for Parallel Coding

	


	call MPI_FINALIZE(ierr)
	call cpu_time(t2) ! End Computation Time
	write(*,*) 'Start time', t1, 'End time', t2, 'time Taken', t2-t1, myid ! CPU 	time of all 3 Processors

end program divcurlparallel



















