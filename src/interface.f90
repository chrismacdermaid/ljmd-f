MODULE interface
  
  USE ljmd
!  USE kinds
!  USE io
!  USE utils
!  USE mdsys
!  USE cell
!  USE toread
IMPLICIT NONE
 CONTAINS
  SUBROUTINE run
  IMPLICIT NONE
    CALL lljmd
  END SUBROUTINE

  SUBROUTINE callopen(pyfname)
  IMPLICIT NONE
  CHARACTER*(*) :: pyfname
    CALL openfiles(pyfname)
  END SUBROUTINE

  
!  SUBROUTINE openfiles(fname)
!  IMPLICIT NONE
!  CHARACTER(LEN=100) :: ifname,restfname, trajfname, ergfname, logfname
! CHARACTER*(*) :: fname
!  INTEGER :: iosI,iosR, length, nprint
!   
!    length = LEN_TRIM( fname )
!    ifname = fname
!    ifname( length+1:length+4 ) = ".inp"
!    
!    OPEN(UNIT=11,FILE=ifname,IOSTAT=iosI,STATUS='OLD')
!   IF( iosI /= 0 ) THEN
!      WRITE(6,*) " "
!      WRITE(6,*) "No input file ", ifname, " found."
!     WRITE(6,*) " "
!   ELSE
!!      CALL readfile
!    READ(11,*) natoms
!    READ(11,*) mass
!    READ(11,*) epsilon
!    READ(11,*) sigma
!    READ(11,*) rcut
!    READ(11,*) box
!   READ(11,*) nsteps
!    READ(11,*) dt
!    READ(11,*) nprint
!    rewind(11)
!    END IF
!  
!    restfname = fname
!    ifname( length+1:length+5 ) = ".rest"
!    OPEN( UNIT=12, FILE=restfname, IOSTAT=iosR, STATUS="UNKNOWN" )
!    IF( iosR /= 0 ) THEN
!      WRITE(6,*) " "
!      WRITE(6,*) "No restart file ", restfname, " found."
!      WRITE(6,*) "Program STOP "
!      WRITE(6,*) " "
!     STOP
!    END IF
!    
!!    DO i=1,natoms
!!     READ(12,*) (pos(i,j),j=1,3)
!!    END DO
!!    DO i=1,natoms
!!    READ(12,*) (vel(i,j),j=1,3)
!!    END DO
!!    CLOSE(12) 
!           
!    trajfname = fname
!    ifname( length+1:length+4 ) = ".xyz"
!    OPEN( UNIT=13, FILE=trajfname, STATUS="UNKNOWN" )
!    
!    ergfname = fname
!    ifname( length+1:length+4 ) = ".dat"
!    OPEN( UNIT=14, FILE=ergfname, STATUS="UNKNOWN" )
!    
!    logfname = fname
!    ifname( length+1:length+4 ) = ".log"
!    OPEN( UNIT=15, FILE=logfname, STATUS="UNKNOWN" )
!    
!  END SUBROUTINE openfiles
  
!  SUBROUTINE readfile
!  IMPICIT NONE
!    READ(11,*) natoms
!    READ(11,*) mass
!    READ(11,*) epsilon
!    READ(11,*) sigma
!    READ(11,*) rcut
!    READ(11,*) box
!    READ(11,*) nsteps
!    READ(11,*) dt
!    READ(11,*) nprint
!  END SUBROUTINE
  
  
  SUBROUTINE set_natoms(myatoms)
    IMPLICIT NONE
    INTEGER :: myatoms
    natoms = myatoms  
  END SUBROUTINE set_natoms

  SUBROUTINE set_mass(mymass)
    IMPLICIT NONE
    INTEGER :: mymass
    mass = mymass  
  END SUBROUTINE set_mass

  SUBROUTINE set_epsilon(myepsilon)
    IMPLICIT NONE
    INTEGER :: myepsilon
    epsilon = myepsilon   
  END SUBROUTINE set_epsilon

  SUBROUTINE set_sigma(mysigma)
    IMPLICIT NONE
    INTEGER :: mysigma
    sigma = mysigma   
  END SUBROUTINE set_sigma

  SUBROUTINE set_rcut(myrcut)
    IMPLICIT NONE
    INTEGER :: myrcut
    rcut = myrcut   
  END SUBROUTINE set_rcut
  
  SUBROUTINE set_box(mybox)
    IMPLICIT NONE
    INTEGER :: mybox
    box = mybox   
  END SUBROUTINE set_box
  
  SUBROUTINE set_nsteps(mysteps)
    IMPLICIT NONE
    INTEGER :: mysteps
    nsteps = mysteps   
  END SUBROUTINE set_nsteps
  
  SUBROUTINE set_dt(mydt)
    IMPLICIT NONE
    INTEGER :: mydt
    dt = mydt   
  END SUBROUTINE set_dt
  
  SUBROUTINE set_nprint(myprint)
    IMPLICIT NONE
    INTEGER :: myprint!, nprint
    nprint = myprint   
  END SUBROUTINE set_nprint
  
END MODULE interface