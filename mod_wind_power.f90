!========= module soundmod =============
module wind_power
! Module contains commonly used variables 
! HOLDS:
!       1) Atmospheric sounding and related variables
!       2) Turbine specifications 
!       3) Format statements
!       4) Array definitions

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

  end module wind_power
