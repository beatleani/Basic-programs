program start
	
	!! MPI formalities
	
	use mpi
	implicit none
      integer ierr, pro_num, no_pro
      integer   ARRAYSIZE, MASTER
      parameter (ARRAYSIZE = 32768)
      parameter (MASTER = 0)

      integer i, mi,  source, chunksize , start1, end1, stop1
      real*8 , allocatable, dimension (:) :: Fx, Fy, Fz, temp_div, P, Q, R, temp_curl,temp_div2,temp_curl2
      real*8 , dimension(1:ARRAYSIZE) ::   div,x,y,z,norm,xv,yv,zv,curl
      real*8  dx,dy,dz
      integer  status(MPI_STATUS_SIZE)
	
    ! use mpi is used for invoking the MPI subroutines
    ! ierr is an error indicator in MPI processes.
    ! ierr is to be zero. If non-zero, then there is an error.
    ! pro_num is the processor number
    ! no_pro is the number of processors

    call MPI_INIT(ierr)
    ! mpi_init starts the MPI processes
    
    call MPI_COMM_SIZE(MPI_COMM_WORLD, no_pro, ierr)
    ! mpi_comm_size finds the number of processes under use
    
    call MPI_COMM_RANK(MPI_COMM_WORLD, pro_num, ierr)

    chunksize = (ARRAYSIZE / no_pro)
	!***** Master task only ******
      if (pro_num .eq. MASTER) then
		print *,'From master processor, chunksize =',chunksize
		print *,'From master processor, no_pro =',no_pro
		print *, 'Master processor is reading data'
      ! Reading the file 
        open( unit= 21,file='32data.txt')
        do i=1, ARRAYSIZE
          read(21,*) x(i) ,y(i) ,z(i) ,norm(i), xv(i), yv(i), zv(i)
        end do
		print *, 'Master processor finished reading data'
	! broadcast x, y, z, xv, yv,zv from master
	call MPI_BCAST(x(1), 1023, MPI_DOUBLE_PRECISION, 0 ,MPI_COMM_WORLD,ierr)
	!	MPI_Send(void* data,int count,MPI_Datatype datatype, int destination,int tag,MPI_Comm communicator)




!	print *, 'Master processor broadcasted x'
!	call MPI_BCAST(y, ARRAYSIZE, MPI_DOUBLE_PRECISION, 0 ,MPI_COMM_WORLD,ierr)
!	call MPI_BCAST(z, ARRAYSIZE, MPI_DOUBLE_PRECISION, 0 ,MPI_COMM_WORLD,ierr)
!	call MPI_BCAST(xv, ARRAYSIZE, MPI_DOUBLE_PRECISION, 0 ,MPI_COMM_WORLD,ierr)
!	call MPI_BCAST(yv, ARRAYSIZE, MPI_DOUBLE_PRECISION, 0 ,MPI_COMM_WORLD,ierr)
!	call MPI_BCAST(zv, ARRAYSIZE, MPI_DOUBLE_PRECISION, 0 ,MPI_COMM_WORLD,ierr)
	write(*,*) ' Message from master processor - broadcast successful!' 
	! Receive Divergence

	 ! call MPI_RECV(start, count, datatype, source, tag, comm, status, ierr)

!	chunksize = (ARRAYSIZE / no_pro)
!     allocate(Fx(1:chunksize), Fy(1:chunksize), Fz(1:chunksize))
!     allocate(temp_div(1:chunksize), P(1:chunksize), Q(1:chunksize), R(1:chunksize))
!	 allocate(temp_curl(1:chunksize),temp_div2(1:chunksize),temp_curl2(1:chunksize))   
   
!	 open(unit= 1, file='Results.txt', form='formatted')

!	do i = 1, no_pro-1
!		start1 = (i - 1)*chunksize + 1
!		end1 = i*chunksize
!		call MPI_RECV(temp_div2, chunksize, MPI_DOUBLE_PRECISION, i, i, MPI_COMM_WORLD, status, ierr)
!		call MPI_RECV(temp_curl2, chunksize, MPI_DOUBLE_PRECISION, i, i+no_pro, MPI_COMM_WORLD, status, ierr)
!		div(start1:end1) = temp_div2
!		curl(start1:end1) = temp_curl2
!	
!	write(1,*) ' The divergences of the vector fields calculated by the',i,'th processor are', div(start1:end1)
!	write(1,*) ' The curls of the vector fields calculated by the',i,'th processor are', curl(start1:end1)
!	end do
!	deallocate(Fx, Fy, Fz, temp_div, P, Q, R, temp_curl,temp_div2,temp_curl2)
	end if
	
	! Non master process

!	if (pro_num .gt. MASTER) then

!	!! Received data from BCAST from Master

!	
!	! calculate divergence
!	
!	
!	
!	if(pro_num == 1 ) then
!		temp_div(1) = 0.0
!		temp_curl(1) = 0.0
!		start1 = 2
!		stop1 = chunksize	
!	else if(pro_num == no_pro -  1) then
!		temp_div(chunksize) = 0.0
!		temp_curl(chunksize) = 0.0
!		start1 = 1
!		stop1 = chunksize - 1
!	else
!		start1 = 1
!		stop1 = chunksize
!	end if
!	
!	do i = start1, stop1
!		mi = (pro_num-1)*chunksize + i
!		dx = x(mi) - x(mi-1)
!		dy = y(mi) - y(mi-1)
!		dz = z(mi) - z(mi-1)
!		Fx(i) = (xv(mi+1) - xv(mi-1))/((2.0)*dx)
!		Fy(i) = (yv(mi+1) - yv(mi-1))/((2.0)*dy)
!		Fz(i) = (zv(mi+1) - zv(mi-1))/((2.0)*dz)
!		P(i) = (zv(mi+1) - zv(mi-1))/((2.0)*dy) - (yv(mi+1) - yv(mi-1))/((2.0)*dz)
!		Q(i) = (zv(mi+1) - zv(mi-1))/((2.0)*dx) - (xv(mi+1) - xv(mi-1))/((2.0)*dz)
!		R(i) = (yv(mi+1) - yv(mi-1))/((2.0)*dx) - (xv(mi+1) - xv(mi-1))/((2.0)*dy)
!		temp_div(i) = Fx(i)+Fy(i)+Fz(i)
!		temp_curl(i) = P(i) - Q(i) + R(i)
!	end do
!	
!	! Send Temp_Divergence to master process
!	! call MPI_SEND(start, count, datatype, dest, tag, comm, ierr)
!	call MPI_SEND(temp_div, chunksize, MPI_DOUBLE_PRECISION, 0,pro_num, MPI_COMM_WORLD, ierr)
!	call MPI_SEND(temp_curl, chunksize, MPI_DOUBLE_PRECISION, 0, pro_num +no_pro, MPI_COMM_WORLD, ierr)

!	end if

 call MPI_FINALIZE(ierr)
end program start

