function apply_linear_acceleration(time_delta, velocity, acceleration) result(r)
    integer, intent(in) :: time_delta   ! input, ms
    integer, intent(in) :: velocity     ! input, mm/s
    integer, intent(in) :: acceleration ! input, mm/s/s
    integer             :: r            ! output, mm/s
    ! velocity = initial_velocty + (dt * acceleration)
    r = (time_delta * (acceleration/1000)) + velocity
end function

program calculate_velocity
    implicit none
    integer :: apply_linear_acceleration
    integer :: initial_velocty = 10000
    integer :: current_velocity = 10000
    integer :: time = 0
    do while (abs(current_velocity) < initial_velocty .OR. time .EQ. 0)
        current_velocity = apply_linear_acceleration(1, current_velocity, -9810)
        time = time + 1
        print*, "time: ", time, " current_velocity: ", current_velocity
    end do
end program calculate_velocity
