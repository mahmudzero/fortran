function apply_linear_acceleration(time_delta, velocity, acceleration) result(r)
    integer, intent(in) :: time_delta   ! input, ms
    integer, intent(in) :: velocity     ! input, mm/s
    integer, intent(in) :: acceleration ! input, mm/s/s
    integer             :: r            ! output, mm/s
    ! velocity = initial_velocty + (dt * acceleration)
    r = (time_delta * (acceleration/1000)) + velocity
end function

function calculate_position(time_delta, initial_position, velocity) result(r)
    integer, intent(in) :: time_delta       ! input, ms
    integer, intent(in) :: initial_position ! input, mm
    integer, intent(in) :: velocity         ! input, mm/s
    integer             :: r                ! output, mm
    ! position = (velocity * dt) + intitial_position
    r = ((velocity/1000) * time_delta) + initial_position
end function

program calculate_velocity
    implicit none
    integer :: apply_linear_acceleration
    integer :: calculate_position
    integer :: initial_velocty = 10000
    integer :: current_velocity = 10000
    ! integer :: initial_position = 0
    integer :: current_position = 0
    integer :: time = 0
    open (9, file = "velocity.output", status = "new")
    open (10, file = "position.output", status = "new")
    write(9, '(A)') "time, velocity"
    write(10, '(A)') "time, position"
    write(10, '(I0, A, I0)') 0, ", ", 0
    write(9, '(I0, A, I0)') 0, ", ", initial_velocty
    do while (abs(current_velocity) < initial_velocty .OR. time .EQ. 0)
        current_velocity = apply_linear_acceleration(1, current_velocity, -9810)
        current_position = calculate_position(1, current_position, current_velocity)
        time = time + 1
        print*, "time: ", time, " current_velocity: ", current_velocity, " current_position: ", current_position
        write(9,'(I0, A, I0)') time, ", ", current_velocity
        write(10,'(I0, A, I0)') time, ", ", current_position
    end do
    close(9)
end program calculate_velocity
