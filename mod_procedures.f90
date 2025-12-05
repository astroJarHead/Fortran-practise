module procedures
    ! Procedures (subroutines and functions) used for the 
    ! windpower project
    implicit none (type, external)
    private
    public :: welcome,getturbinespecs,getsounding,findpower, & 
              saveresults 
    
contains
!=====begin procedures: subroutines==============
    subroutine welcome
          write(*,*)
          write(*,*) "Welcome to Wind Power"
    end subroutine welcome
!=======================================
    subroutine getturbinespecs

    use wind_power, only : r,zhub
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

    use wind_power, only : sounding_file,title,zmsl,speed_msec,speed_array,  & 
                               first_ten_sound,fmt2

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

    use wind_power, only : zhub, r, iters, power_out, header1,total_power 
    use meteo, only : M, density     

    real :: delz ! height increment across turbine
    real :: zref ! reference height below turbine blade
    real :: zz   ! arbitrary height along turbine blade
    !real :: M    ! function to interpolate wind speed
    !real :: density ! function to estimate density at height zr = zz
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

    use wind_power, only : sounding_file,zhub,r,fmt2,first_ten_sound, & 
        header1,power_out,iters,title,total_power

    real, save :: power = 0.0    ! power (W) outout from turbine
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

    ! Now get power from wind_power and convert to kilowatts and megawatts
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


end module procedures