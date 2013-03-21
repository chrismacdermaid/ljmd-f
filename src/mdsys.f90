MODULE mdsys
  USE kinds
  IMPLICIT NONE
  INTEGER :: natoms,nfi,nsteps,nthreads
  REAL(kind=dbl) dt, box, rcut, epsilon, sigma 
  REAL(kind=dbl) ekin, epot, temp
  REAL(kind=dbl), POINTER, DIMENSION (:,:) :: pos, vel
  REAL(kind=dbl), POINTER, DIMENSION (:,:,:) :: frc
END MODULE mdsys
