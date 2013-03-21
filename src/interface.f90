MODULE interface
implicit none

  SUBROUTINE run
  IMPLICIT NONE
    CALL ljmd
  END SUBROUTINE
  
  SUBROUTINE readfile
  IMPICIT NONE
    READ(11,*) natoms
    READ(11,*) mass
    READ(11,*) epsilon
    READ(11,*) sigma
    READ(11,*) rcut
    READ(11,*) box
    CALL getline(stdin,restfile)
    CALL getline(stdin,trajfile)
    CALL getline(stdin,ergfile)
    READ(stdin,*) nsteps
    READ(stdin,*) dt
    READ(stdin,*) nprint
  END SUBROUTINE
  
  
  SUBROUTINE set_natoms(myatoms)
    IMPLICIT NONE
    INTEGER*4 :: myatoms
    natoms = myatoms  
  END SUBROUTINE set_natoms

  SUBROUTINE set_mass(mymass)
    IMPLICIT NONE
    INTEGER*4 :: mymass
    mass = mymass  
  END SUBROUTINE set_mass

  SUBROUTINE set_epsilon(myepsilon)
    IMPLICIT NONE
    INTEGER*4 :: myepsilon
    epsilon = myepsilon   
  END SUBROUTINE set_epsilon

  SUBROUTINE set_sigma(mysigma)
    IMPLICIT NONE
    INTEGER*4 :: mysigma
    sigma = mysigma   
  END SUBROUTINE set_sigma

  SUBROUTINE set_rcut(myrcut)
    IMPLICIT NONE
    INTEGER*4 :: myrcut
    rcut = myrcut   
  END SUBROUTINE set_rcut
  
  SUBROUTINE set_box(mybox)
    IMPLICIT NONE
    INTEGER*4 :: mybox
    box = mybox   
  END SUBROUTINE set_box
  
  SUBROUTINE set_nsteps(mysteps)
    IMPLICIT NONE
    INTEGER*4 :: mysteps
    nsteps = mysteps   
  END SUBROUTINE set_nsteps
  
  SUBROUTINE set_dt(mydt)
    IMPLICIT NONE
    INTEGER*4 :: mydt
    dt = mydt   
  END SUBROUTINE set_dt
  
  SUBROUTINE set_nprint(myprint)
    IMPLICIT NONE
    INTEGER*4 :: myprint
    nprint = myprint   
  END SUBROUTINE set_nprint
  
END MODULE interface