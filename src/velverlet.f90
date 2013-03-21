! velocity verlet
SUBROUTINE velverlet
  USE kinds
  USE mdsys
  USE physconst
  IMPLICIT NONE

!  REAL(kind=dbl) :: vfac
INTEGER :: i

REAL(kind=dbl) :: vfac

Do i = 1, natoms

  vfac = 0.5_dbl * dt / mvsq2e / pos(i,4)
  ! first part: propagate velocities by half and positions by full step
  vel(i,:) = vel(i,:) + vfac*frc(i,:,1)
  pos(i,1:3) = pos(i,1:3) + dt*vel(i,:)

  ! compute forces and potential energy 
  CALL force

  ! second part: propagate velocities by another half step */
  vel(i,:) = vel(i,:) + vfac*frc(i,:,1) 

END DO
END SUBROUTINE velverlet








