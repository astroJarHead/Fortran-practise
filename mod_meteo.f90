module meteo
    ! Module meteo contains meteorological functions 
    !
    implicit none (type, external)
    private
    public M,density
    
contains
!=====begin contained functions==================
    real function M(zr)

    ! Function to interpolate wind speed sounding values at any height zr.
    ! Interpolation is linear between heights.
    ! If the height zr requested is outside the sounding range M is returned
    ! with physically unrealistic negative values.
    !
    ! zr is less then minimum height in sounding: M = -10000.0
    ! zr is greater than maximum height in sounding: M = -12000.0

    use wind_power, only : speed_msec,zmsl

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

    use wind_power, only : zmsl
 
    ! declare variables
    real, intent(in) :: zr ! input argument - height AGL at which density 
                           ! is desired
    real :: zrmsl ! height (m) above Mean Sea Level (MSL)
    real, parameter :: denref = 1.225 ! sea level reference density  (kg/m^3)
    real, parameter :: H = 8550.0 ! exponential scale height (m) above MSL

    ! convert input height zr (AGL) to MSL using the station elevation
    ! in the sounding data accessed via mod_wind_power module
    zrmsl = zr + zmsl(1)

    density = denref*exp(-1.0*zrmsl/H)

    end function density

!=====end functions====================
        
end module meteo