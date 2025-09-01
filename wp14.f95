! Program: Wind Power
! 
! Description: Exercise from https://www.eoas.ubc.ca/courses/atsc212
! for FORTRAN 95 practise. Code estimates power output from a wind 
! turbine used for electric power generation. 
! 
! Estimate wind power 

!========= module soundmod =============
module wind_power_mod
! Single module following best practise. Combines tutorial's 
! MODULEs soundmod and turbinemod.
! HOLDS:
!       1) Atmospheric sounding and related variables (soundmod)
!       2) Turbine specifications (turbinemod)
  
  implicit none
  ! Turbine specification variables
  real :: zhub    !hub height of turbine (m) AGL
  real :: r       !radius of turbine blade (m)
  ! Atmospheric sounding variables 
  integer, parameter :: maxlines = 120  !max sounding lines that can be captured
  real, dimension(maxlines) :: zmsl   !array of heights MSL (m)
  real, dimension(maxlines) :: speed_array  !array of wind speed (knots)
  character (len=100), dimension(maxlines) :: sounding     !holds the whole sounding


end module wind_power_mod

!========= main program =============
program windpowermain

! 
  use wind_power_mod
! declare variables
  implicit none               !enforce strong typing
  real :: start, finish       ! cpu time variables
  real :: power = 0.0         !power (W) outout from turbine
  
  call cpu_time(start)

!set up
  call welcome
  call getturbinespecs
  call getsounding

!compute wind power
  call findpower

!save results
  call saveresults
  write(*,*) "power = ", power
  
  call cpu_time(finish)
  ! Inform user of performance time
  print '("CPU Time = ",ES12.3," seconds.")',finish-start
  !print '("CPU Time = ",f9.4," seconds.")',finish-start

end program windpowermain

!=======================================
subroutine welcome
  implicit none
  write(*,*)
  write(*,*) "Welcome to Wind Power"
end subroutine welcome


!=======================================
subroutine getturbinespecs

  use wind_power_mod
  implicit none           !enforce strong typing
  
  write(*,*)
  write(*,*) "getturbinespecs: Get specifications of the wind turbine."
  write(*,"(a)", advance="no") "   Enter hub height (m) AGL: "
  read(*,*) zhub
  write(*,"(a)", advance="no") "   Enter turbine radius (m): "
  read(*,*) r
  
  do while ( r .ge. zhub )   !give user multiple chances to enter correct radius
    write(*,*) "    ERROR: turbine radius must be less than the hub height AGL."
    write(*,"(a)", advance="no") "   Enter turbine radius (m): "
    read(*,*) r
  enddo
  
  write(*,*) "  OK: hub height (m) AGL = ",zhub,"   and radius (m) = ",r
  
end subroutine getturbinespecs


!=======================================
subroutine getsounding

  use wind_power_mod
  implicit none                         !enforce strong typing

  character (len=50) :: filename        !holds name of sounding file
  character (len=30) :: fmt = "(T8,F7.0,35X,F7.0)"
  character (len=30) :: fmt2 = "(F8.1,5X,F8.1)"
  character (len=100) :: line           !one line in the sounding file
  character (len=100) :: title          !to hold first line of sounding file
  integer :: ero          !error flag for opening a file
  integer :: err          !error flag for reading a file
  integer :: i            !dummy counter variable
  real :: speedMetersPerSec           ! wind speed in meters/sec


!Get the file name from user  
  write(*,*)
  write(*,*) "getsounding:  Get the file holding the input sounding"
  write(*,"(a)",advance="no") "   Enter the name of the sounding file: "
  read(*,*) filename
  write(*,*) "    OK: Sounding file name is: ",filename

!Open the file  
  open(1, file=filename, status="old", action="read", iostat=ero) !open file holding the sounding
!   write(*,*) "    Error flag = ", ero				!display the file-opening error flag (0 = no error)
  if (ero .ne. 0) then         !can't open the file
    write(*,*) "  Sorry.  Can't find the file: ", filename
    write(*,*) "    Don't forget to add a suffix .txt if needed."
    stop "Bye."
  else              !successfully opened the file
    write(*,*) "  Good.  Successfully opened file: ", filename
    write(*,*)
  endif

!Make first pass thru file, and echo contents to screen
  write(*,*) "======================================="
  do             
                          !read all lines in the file
    read(1,"(a)", iostat=err) line     !try to read a line
    if ( err .ne. 0  )  exit    !couldn't read the line, perhaps because reached end of file
    write(*,"(a)") line         !successfully read the line, so echo on screen
  enddo
  write(*,*) "======================================="
  write(*,*)

!Make second pass thru file, to read the data
  rewind(1)                   !reset file to the beginning
  
  write(*,*)
  write(*,*) "  Skipping past the 5 header lines."
  do i = 1,5                  !skip the first 5 header lines in the file
    read(1,"(a)", iostat=err) line     !try to read a line
    if ( err .ne. 0  )  stop "Bye.  Unable to read past the header lines."
    if ( i == 1) title = line ! first line saved to title
    write(*,"(a)") line       !successfully read the line, so echo on screen
  enddo
  
  write(*,*)
  write(*,*) "***** The title of the sounding file is: *****"
  write(*,"(a)") title
  write(*,*) "**********************************************"
  write(*,*)

  ! Formatted read in of first 20 height and speed values.
  ! Convert speed on knots -> m/sec and write to screen in 
  ! a nicely formatted manner. 
  write(*,"(a)") "    H(m)        S(m/s) "
  write(*,"(a)") "-----------------------"
  do i = 1,20
    read(1,fmt) zmsl(i), speed_array(i)
    speedMetersPerSec = 0.5144*speed_array(i) 
    write(*,fmt2) zmsl(i), speedMetersPerSec
  enddo

  write(*,*)
  close(1)                     !close the sounding file
    
end subroutine getsounding


!=======================================
subroutine findpower
  implicit none
  write(*,*)
  write(*,*) "findpower:  Calculate the wind power."
end subroutine findpower


!=======================================
subroutine saveresults
  implicit none
  write(*,*)
  write(*,*) "saveresults:  Write to disk and screen the wind power potential"
  write(*,*)
end subroutine saveresults


!======end