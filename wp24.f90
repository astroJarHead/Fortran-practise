! Program: Wind Power
!
! wp20.f90 incorporates use ..., only : var,var in a few places
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

!========= module soundmod =============
module wind_power_mod
! Single module following best practise. Combines tutorial's
! MODULEs soundmod and turbinemod.
! HOLDS:
!       1) Atmospheric sounding and related variables (soundmod)
!       2) Turbine specifications (turbinemod)
!       3) real function M(z) for wind speed interpolation
!       4) real function density to determine density at any height

    implicit none
    ! Turbine specification variables
    real :: zhub    !hub height of turbine (m) AGL
    real :: r       !radius of turbine blade (m)
    ! Atmospheric sounding variables
    integer, parameter :: maxlines = 120  !max sounding lines that can be captured
    integer :: iters = 20 ! number of iterations (rectangles) across turbine
    real, dimension(maxlines) :: zmsl   !array of heights MSL (m)
    real, dimension(maxlines) :: speed_array  !array of wind speed (knots)
    real, dimension(maxlines) :: speed_msec  !array of wind speed (m/sec)
    real, dimension(10,2) :: first_ten_sound ! first ten soundings (m; m/sec)
    real, dimension(:,:), allocatable :: power_out ! array for output power rectangles
    real :: total_power = 0.0 ! total wind power generated in Watts, initialized to 0.0
    character (len=30), parameter :: fmt2 = "(F8.1,5X,F8.1)" ! For writing 
              !Height (m) and wind speed (m/s)
    character (len=30) :: sounding_file        !holds name of sounding file
    character (len=100) :: title          !holds first line of sounding file
    character (len=100), dimension(maxlines) :: sounding     !holds the whole sounding
    ! define a CHARACTER PARAMETER variable for the output header string
    character (LEN=*), parameter :: header1 = " height AGL (m)  wind speed (m/s)  rho (kg/m^3)  chord_len (m)   delpower (W)"

  end module wind_power_mod

!========= main program =============
program windpowermain

! declare variables
  implicit none               !enforce strong typing
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
 
!=====begin procedures: subroutines==============
    subroutine welcome
        implicit none
          write(*,*)
          write(*,*) "Welcome to Wind Power"
    end subroutine welcome
!=======================================
    subroutine getturbinespecs

    use wind_power_mod, only : r,zhub
    implicit none           !enforce strong typing
    ! no var declarations, carried from module use ... only : 

    write(*,*)
    write(*,*) "getturbinespecs: Get specifications of the wind turbine."
    write(*,"(a)", advance="no") "   Enter hub height (m) AGL: "
    read(*,*) zhub
    write(*,"(a)", advance="no") "   Enter turbine radius (m): "
    read(*,*) r

    do while ( r >= zhub )   !give user multiple chances to enter correct radius
        write(*,*) "    ERROR: turbine radius must be less than the hub height AGL."
        write(*,"(a)", advance="no") "   Enter turbine radius (m): "
        read(*,*) r
    enddo

    write(*,*) "**********Hub and radius parameters**********"
    write(*,'(A,F7.2)') " hub height (m) AGL = ",zhub
    write(*,'(A,F7.2)') " and radius (m) = ",r

    end subroutine getturbinespecs

!=======================================
    subroutine getsounding

    use wind_power_mod, only : sounding_file,title,zmsl,speed_msec,speed_array,  & 
                               first_ten_sound,fmt2
    implicit none                         !enforce strong typing 

    character (len=30), parameter :: fmt = "(T8,F7.0,35X,F7.0)"
    character (len=100) :: line           !one line in the sounding file
    integer :: ero          !error flag for opening a file
    integer :: err          !error flag for reading a file
    integer :: i            !dummy counter variable
    real :: speedMetersPerSec           ! wind speed in meters/sec

    !Get the file name from user
    write(*,*)
    write(*,*) "getsounding:  Get the file holding the input sounding"
    write(*,"(a)",advance="no") "   Enter the name of the sounding file: "
    read(*,*) sounding_file
    write(*,*) "    OK: Sounding file name is: ",sounding_file

    !Open the file
    open(unit=1, file=sounding_file, status="old", action="read", iostat=ero) 
                !open file holding the sounding
    if (ero /= 0) then         !can't open the file
        write(*,*) "  Sorry.  Can't find the file: ", sounding_file
        write(*,*) "    Don't forget to add a suffix .txt if needed."
        stop "Bye."
    else              !successfully opened the file
        write(*,*) "  Good.  Successfully opened file: ", sounding_file
        write(*,*)
    endif

    !Make first pass thru file, and echo contents to screen
    write(*,*) "======================================="
    do
                            !read all lines in the file
        read(1,"(a)", iostat=err) line     !try to read a line
        if ( err /= 0  )  exit    !couldn't read the line, end of file?
        write(*,"(a)") line         !successfully read the line, so echo 
                                    ! on screen
    enddo
    write(*,*) "======================================="
    write(*,*)

    !Make second pass thru file, to read the data
    rewind(1)                   !reset file to the beginning

    write(*,*)
    write(*,*) "  Skipping past the 5 header lines."
    do i = 1,5                  !skip the first 5 header lines in the file
        read(1,"(a)", iostat=err) line     !try to read a line
        if ( err /= 0  )  stop "Bye.  Unable to read past the header lines."
        if ( i == 1) title = line ! first line saved to title
        write(*,"(a)") line   !successfully read the line, so echo on screen
    enddo

    write(*,*)
    write(*,*) "***** The title of the sounding file is: *****"
    write(*,"(a)") title
    write(*,*) "**********************************************"
    write(*,*)

    ! Formatted read in of first 10 height and speed values.
    ! Convert speed on knots -> m/sec and write to screen in
    ! a nicely formatted manner.
    write(*,"(a)") "    H(m)        S(m/s) "
    write(*,"(a)") "-----------------------"
    do i = 1,10
        read(1,fmt) zmsl(i), speed_array(i)
        speedMetersPerSec = 0.5144*speed_array(i)
        write(*,fmt2) zmsl(i), speedMetersPerSec
        ! place these values into array first_ten_sound for use in saveresults
        first_ten_sound(i,1) = zmsl(i)
        first_ten_sound(i,2) = speedMetersPerSec
    enddo

    write(*,*)
    close(unit=1)  !close the sounding file

    ! Create the array of wind speeds (m/sec)
    speed_msec = 0.5144*speed_array

    end subroutine getsounding

!=======================================
    subroutine findpower

    use wind_power_mod, only : zhub, r, iters, power_out, header1,total_power 
         

    implicit none
    real :: delz ! height increment across turbine
    real :: zref ! reference height below turbine blade
    real :: zz   ! arbitrary height along turbine blade
    real :: M    ! function to interpolate wind speed
    real :: density ! function to estimate density at height zr = zz
    real :: speedz ! interpolated wind speed at height z
    real :: rho ! estimated density at height z (MSL)
    real :: chord_len ! chord length parallel to ground across diameter of blades
    real :: B ! distance from hub to chord (m) AGL = zhub - zr [B < 0 is OK]
    real :: rect_area ! area of the rectangle = chord_len * B
    real :: delpower ! an increment of windpower for an area of rect_area
    integer :: i   ! counting index

    write(*,*)
    write(*,*) "findpower:  Calculate the wind power."
    write(*,'(A,i4,A)') " Dividing turbine into ",iters," rectangles."
    write(*,*)

    ! With rectangle count set and user informed of this count,
    ! allocate the array size to hold the data from the rectangles
    allocate(power_out(iters,5))

    delz = 2.0*r/iters ! divide diameter of turbine into 20 parts
    zref = zhub - r - 0.5*delz ! one-half delta-z below turbine blade

    ! Prepare the output with titles at the top of the screen
    write(*,*) header1
    ! Loop along turbine blade from TOP to BOTTOM
    do i = iters,1,-1
        zz = zref + real(i)*delz
        ! pass height in AGL and conversion -> MSL done
        ! in function M(zz)
        speedz = M(zz)
        ! Estimate air density rho at height zz (AGL, meters)
        ! with function density
        rho = density(zz)
        ! determine distance B from hub to horizontal chord
        B = zhub - zz
        ! calculate chord length via Pythagorean theorem
        chord_len = 2.0*sqrt(r**2 - B**2)
        ! area of rectangle for power equation = chord_len*B
        ! absolute vlue of B used here as B could be < 0
        rect_area = chord_len*abs(B)
        delpower = 0.5*rho*rect_area*(speedz**3)
        ! sum up the power generated in Watts
        total_power = total_power + delpower
        write(*,'(3X,F7.3,10X,F7.3,9X,F9.4,7X,F7.2,7X,F12.2)') zz,speedz, & 
                rho,chord_len,delpower
        ! Fill in the data for power_out to use in saveresults
        power_out(i,1) = zz
        power_out(i,2) = speedz
        power_out(i,3) = rho
        power_out(i,4) = chord_len
        power_out(i,5) = delpower
    enddo

    end subroutine findpower

!=======================================
    subroutine saveresults

    use wind_power_mod, only : sounding_file,zhub,r,fmt2,first_ten_sound, & 
        header1,power_out,iters,title,total_power

    implicit none
    real :: power = 0.0    ! power (W) outout from turbine
    real :: kiloWattPower  ! power in kilowatts
    real :: megaWattPower  ! power in megawatts
    integer :: i ! loop counter
    integer :: sounding_charlen ! length of sounding filename
    integer :: root_end_index ! Index of last character root of sounding filename
    character (len=30) :: sounding_rootname ! rootname of the sounding filename
    character (len=30) :: output_file ! output wind power file

    write(*,*)
    write(*,*) "saveresults:  Write to disk and screen the wind power potential"
    write(*,*)

    ! Take sounding filename and remove '.txt' extension and print to
    ! screen. Remove last 4 characters
    ! Length of the trimmed string for the sounding filename
    sounding_charlen = len_trim(sounding_file)
    ! Get rootname
    root_end_index = sounding_charlen-4
    sounding_rootname = sounding_file(1:root_end_index)
    write(*,*)
    write(*,*) " Root of sounding file is: ",sounding_rootname
    write(*,*)
    ! create output file name via concatenation AND trim
    output_file = trim(sounding_rootname)//".out.txt"
    write(*,*) " Output filename will be: ",output_file
    write(*,*)
    ! open the file for output
    open(unit=10,FILE=output_file,FORM="FORMATTED",STATUS="REPLACE", & 
        ACTION="READWRITE")
    write(10,*)
    write(10,*) title
    write(10,*)
    write(10,'(A,F6.2)') " Turbine hub height AGL (m): ",zhub
    write(10,'(A,F6.2)') " Turbine blade radius (r)  : ",r
    write(10,*)

    ! write the first ten soundings converted using the array first_ten_sound
    ! and the header line we already wrote to the screen in getsounding
    write(10,"(a)") "    H(m)        S(m/s) "
    write(10,"(a)") "-----------------------"
    do i = 1,10
        write(10,fmt2) first_ten_sound(i,1), first_ten_sound(i,2)
    enddo
    write(10,*)

    ! write the header string to the output file
    write(10,*) header1
    write(10,*)

    ! write the output data computed across the turbine
    do i = iters,1,-1
        write(10,'(3X,F7.3,10X,F7.3,9X,F9.4,7X,F7.2,7X,F12.2)') &
        power_out(i,1),power_out(i,2),power_out(i,3), &
        power_out(i,4),power_out(i,5)
    enddo

    write(10,*)

    ! Now get power from wind_power_mod and convert to kilowatts and megawatts
    power = total_power
    kiloWattPower = power/1000.0
    megaWattPower = kiloWattPower/1000.0

    ! Format string accomodates up to 100 Gigawatts, output as Watts
    write(*,'(A,F15.2,A)') " power = ", power, " Watts"
    write(10,'(A,F15.2,A)') " power = ", power, " Watts"
    write(*,'(A,F12.2,A)') " power = ", kiloWattPower, " kWatts"
    write(10,'(A,F12.2,A)') " power = ", kiloWattPower, " kWatts"
    ! If more than 1 MegaWatts of power, output power in MW
    if (megaWattPower >= 1.0) then
        write(*,'(A,F9.2,A)') " power = ", megaWattPower, " MegaWatts"
        write(10,'(A,F9.2,A)') " power = ", megaWattPower, " MegaWatts"
    endif

    ! close output file
    write(10,*)
    close(unit=10)

    end subroutine saveresults


!====last subroutine==============================

!=====begin contained functions==================
    real function M(zr)

    ! Function to interpolate wind speed sounding values at any height zr.
    ! Interpolation is linear between heights.
    ! If the height zr requested is outside the sounding range M is returned
    ! with physically unrealistic negative values.
    !
    ! zr is less then minimum height in sounding: M = -10000.0
    ! zr is greater than maximum height in sounding: M = -12000.0

    use wind_power_mod, only : speed_msec,zmsl
    implicit none

    ! declare variables
    real, intent(in) :: zr ! input argument - height at which to interpolate 
                           ! wind speed
    real :: min_height ! the minimum height in zmsl array
    real, dimension(1) :: min_loc ! location in array zmsl of the minimum
    real :: max_height ! the maximum height in zmsl array
    real :: zrmsl        ! height above msl
    real :: ratio        ! relative distance of zr between two adjacent heights
    integer :: i         ! dummy height index
    integer :: itop      ! index of sounding height just above zr

    ! Check that the height requested is within the height range of the 
    ! sounding data.
    ! Note the use of the test range indices [1:20]. Eventually we need
    ! to fix this issue.
    M = 0

    ! convert hub height zr to MSL using first entry in sounding file
    ! First entry is station elevation
    zrmsl = zr + zmsl(1)

    min_height = MINVAL(zmsl(1:20))
    min_loc = MINLOC(zmsl(1:20))
    max_height = MAXVAL(zmsl(1:20))
    if (zrmsl < min_height) then
        M = -5.0
    else if (zrmsl > max_height) then
        M = -20.0
    endif

    ! find array indices of heights that bracket the height zr
    do i = 1,20   ! Still using first twenty
        itop = i    ! reset itop to current index
        if (zmsl(i) >= zrmsl) exit    ! found it
    enddo

    ! We have the index, now do the interpolation while checking
    ! if the interpolation is done from the first value

    if (itop > 1) then
        ratio = (zrmsl -zmsl(itop - 1))/zmsl(itop)
        M = speed_msec(itop - 1) + ratio*(speed_msec(itop) - & 
                                          speed_msec(itop - 1))
    else
        ratio = zrmsl/zmsl(itop)  ! Assumption is z at bottom = 0
        M = ratio*speed_msec(itop) ! Boundary condition is speed at 0 = 0
    endif

    ! write(*,*) "********************"
    ! write(*,*) " In function M(zr): "
    ! write(*,*) " At height (AGL,m) = ",zrmsl," wind speed (m/s) = ",M

    end function M

!=======================================
    real function density(zr)

    ! This function takes an input height in meters AGL,
    ! converts this to a height in meters above Mean Sea Level
    ! (MSL) and calculates the approximate density at the height
    ! using a standard reference sea level density and atmospheric
    ! scale height

    use wind_power_mod, only : zmsl
    implicit none

    ! declare variables
    real, intent(in) :: zr ! input argument - height AGL at which density 
                           ! is desired
    real :: zrmsl ! height (m) above Mean Sea Level (MSL)
    real, parameter :: denref = 1.225 ! sea level reference density  (kg/m^3)
    real, parameter :: H = 8550.0 ! exponential scale height (m) above MSL

    ! convert input height zr (AGL) to MSL using the station elevation
    ! in the sounding data accessed via wind_power_mod module
    zrmsl = zr + zmsl(1)

    density = denref*exp(-1.0*zrmsl/H)

    end function density

!=====end functions====================