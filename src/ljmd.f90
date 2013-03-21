MODULE ljmd
  USE kinds
  USE io
  USE utils
  USE mdsys
  USE cell
  INTEGER :: nprint
 CONTAINS
 
SUBROUTINE lljmd
IMPLICIT NONE
  
<<<<<<< HEAD
  INTEGER :: i, j
  INTEGER, EXTERNAL :: omp_get_num_threads
!  CHARACTER*(*) :: fname!restfile, trajfile, ergfile
!  CALL openfiles(pyfname)
=======
  INTEGER :: nprint, i, j
 ! REAL:: natomspcsrl
  INTEGER, EXTERNAL :: omp_get_num_threads
  CHARACTER(len=sln) :: restfile, trajfile, ergfile
  
>>>>>>> learn
  nthreads = 1
  !$OMP parallel shared(nthreads)
  !$OMP master
  !$  nthreads = omp_get_num_threads()
  !$  WRITE(stdout,'(A,I2,A)') 'Running OpenMP version using ',nthreads,' thread(s).'
  !$OMP end master
  !$OMP end parallel

<<<<<<< HEAD
!!  READ(stdin,*) natoms
!!  READ(stdin,*) mass
!!  READ(stdin,*) epsilon
!!  READ(stdin,*) sigma
!!  READ(stdin,*) rcut
!!  READ(stdin,*) box
!  CALL getline(stdin,restfile)
!  CALL getline(stdin,trajfile)
!  CALL getline(stdin,ergfile)
!!  READ(stdin,*) nsteps
!!  READ(stdin,*) dt
!!  READ(stdin,*) nprint



!    READ(11,*) natoms
!    READ(11,*) mass
!    READ(11,*) epsilon
!    READ(11,*) sigma
!    READ(11,*) rcut
!    READ(11,*) box
!    READ(11,*) nsteps
!    READ(11,*) dt
!    READ(11,*) nprint
!END SUBROUTINE
  
  ! allocate storage for simulation data.
  ALLOCATE(pos(natoms,3),vel(natoms,3),frc(natoms,3,nthreads))

=======
  READ(stdin,*) natoms
 !READ(stdin,*) mass
 !READ(stdin,*) epsilon
 !READ(stdin,*) sigma
  READ(stdin,*) rcut
  READ(stdin,*) box
  CALL getline(stdin,restfile)
  CALL getline(stdin,trajfile)
  CALL getline(stdin,ergfile)
  READ(stdin,*) nsteps
  READ(stdin,*) dt
  READ(stdin,*) nprint

 ! allocate storage for simulation data.
 ALLOCATE(pos(natoms,6),vel(natoms,3),frc(natoms,3,nthreads))
>>>>>>> learn

  ! read restart 
!  OPEN(UNIT=33, FILE=restfile, FORM='FORMATTED', STATUS='OLD')
  DO i=1,natoms
<<<<<<< HEAD
     READ(12,*) (pos(i,j),j=1,3)
  END DO
=======
     READ(33,*) (pos(i,j),j=1,6)
   END DO
>>>>>>> learn
  DO i=1,natoms
     READ(12,*) (vel(i,j),j=1,3)
  END DO
  CLOSE(12)

  ! set up cell list
  CALL mkcell
  CALL updcell
  
  ! initialize forces and energies
  nfi=0
  frc(:,:,:) = 0.0_dbl
  CALL force
  CALL getekin
    
!  CALL ioopen(ergfile, trajfile)

  WRITE(stdout, *) 'Starting simulation with ', natoms, ' atoms for', nsteps, ' steps'
  WRITE(stdout, *) '    NFI           TEMP                 EKIN                  EPOT&
       &                ETOT'
  CALL output

! main MD loop 
  DO nfi=1, nsteps
      
     ! write output, if requested
     IF (mod(nfi,nprint) == 0) THEN
        CALL output
     END IF

     ! propagate system and recompute energies
     CALL updcell
     CALL velverlet
     CALL getekin
  END DO

  ! clean up: close files, free memory
  WRITE(stdout,'(A)') 'Simulation Done.'
  CALL rmcell
  CALL ioclose

  DEALLOCATE(pos,vel,frc)
<<<<<<< HEAD
END SUBROUTINE lljmd

SUBROUTINE openfiles(pyfname)
  IMPLICIT NONE
  CHARACTER(LEN=30) :: ifname,restfname, trajfname, ergfname, logfname,fname
  CHARACTER*(*) :: pyfname 
  INTEGER :: iosI,iosR, length!, nprint
    
    fname = TRIM( pyfname )   
    length = LEN_TRIM( fname )
    ifname = TRIM(fname)
    ifname( length+1:length+4 ) = ".inp"
    OPEN(UNIT=11,FILE=ifname,IOSTAT=iosI,STATUS='OLD')
    IF( iosI /= 0 ) THEN
      WRITE(6,*) " "
      WRITE(6,*) "No input file ", ifname, " found."
      WRITE(6,*) " "
    ELSE
!      CALL readfile
       READ(11,*) natoms
       READ(11,*) mass
       READ(11,*) epsilon
       READ(11,*) sigma
       READ(11,*) rcut
       READ(11,*) box
       READ(11,*) nsteps
       READ(11,*) dt
       READ(11,*) nprint
       rewind(11)       
    END IF
  
    restfname = TRIM(fname)
    restfname( length+1:length+5 ) = ".rest"
    OPEN( UNIT=12, FILE=restfname, IOSTAT=iosR, STATUS="UNKNOWN" )
    IF( iosR /= 0 ) THEN
      WRITE(6,*) " "
      WRITE(6,*) "No restart file ", restfname, " found."
      WRITE(6,*) "Program STOP "
      WRITE(6,*) " "
      STOP
    END IF
    
!    DO i=1,natoms
!     READ(12,*) (pos(i,j),j=1,3)
!    END DO
!    DO i=1,natoms
!    READ(12,*) (vel(i,j),j=1,3)
!    END DO
!    CLOSE(12) 
           
    trajfname = TRIM(fname)
    trajfname( length+1:length+4 ) = ".xyz"
    OPEN( UNIT=13, FILE=trajfname, STATUS="UNKNOWN" )
    
    ergfname = TRIM(fname)
    ergfname( length+1:length+4 ) = ".dat"
    OPEN( UNIT=14, FILE=ergfname, STATUS="UNKNOWN" )
    
    logfname = TRIM(fname)
    logfname( length+1:length+4 ) = ".log"
    OPEN( UNIT=15, FILE=logfname, STATUS="UNKNOWN" )
    
  END SUBROUTINE openfiles
  
  END MODULE ljmd
=======
END PROGRAM LJMD
>>>>>>> learn
