! Program: Wind Power
!
! wp25.f90 incorporates use ..., only : var,var in a few places
!
! Description: Exercise from https://www.eoas.ubc.ca/courses/atsc212
! for FORTRAN 95 practise. Code estimates power output from a wind
! turbine used for electric power generation.
!
! Tested with large existing and conceived wind turbines of hub heights
! 185 m and blade radius 155 m against porthardy.txt sounding. Output
! total power in hundreds of megawatts frm this example.
!
! Estimate wind power

! Libraries required checked via ldd:
!
! linux-vdso.so.1
! libgfortran.so.5
! libm.so.6
! libgcc_s.so.1
! libquadmath.so.0
! libc.so.6
! /lib64/ld-linux-x86-64.so.2


!========= main program =============
program windpowermain

 !use mod_wind_power
  use procedures
! declare variables
  implicit none (type, external)              !enforce strong typing
  real :: start, finish       ! cpu time variables
  call cpu_time(start)

!set up
  call welcome
  call getturbinespecs
  call getsounding

!compute wind power
  call findpower

!save results
  call saveresults

  call cpu_time(finish)
  ! Inform user of performance time
  print '(" CPU Time = ",ES12.3," seconds.")',finish-start
  !print '("CPU Time = ",f9.4," seconds.")',finish-start
  write(*,*) "**********PROGRAM Wind Power FINISHED**********"
  write(*,*) " "

end program windpowermain

!======end main program windpower
 

