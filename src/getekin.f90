! compute kinetic energy
SUBROUTINE getekin
  USE kinds
  USE mdsys, ONLY: natoms,temp, ekin, vel, pos
  USE physconst
  IMPLICIT NONE

  INTEGER :: i

  ekin = 0.0_dbl
  DO i=1,natoms
     ekin = ekin + 0.5_dbl * mvsq2e * pos(i,4) * dot_product(vel(i,:),vel(i,:))
  END DO
  temp = 2.0_dbl * ekin/(3.0_dbl*DBLE(natoms-1))/kboltz
END SUBROUTINE getekin
