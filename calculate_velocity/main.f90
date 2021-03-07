!------------------------------------------------------------------------------
! FUNCTION: apply_linear_acceleration
!------------------------------------------------------------------------------
!> Apply linear acceleration to a velocity, given a time delta
!!
!! @note returns mm/s, example of fortran documentation
!!
!! @param[in] time_delta   dt, in ms
!! @param[in] velocity     mm/s
!! @param[in] acceleration mm/s/s
!------------------------------------------------------------------------------
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

function calculate_potential_energy(mass, position, acceleration_to_rest) result(r)
    integer, intent(in) :: mass                 ! input, mg
    integer, intent(in) :: position             ! input, mm (?, I think, haven't actually thought about this one)
    integer, intent(in) :: acceleration_to_rest ! input, mm/s/s
    integer             :: r                    ! output, newtons
    ! potential_energy = mass * acceleration_to_rest * distance_from_rest
    r = mass * (acceleration_to_rest/1000) * position
end function

function calculate_kinetic_energy(mass, velocity) result(r)
    integer, intent(in) :: mass     ! input, mg
    integer, intent(in) :: velocity ! input, mm/s
    integer             :: r        ! output, newtons
    ! kinetic_energy = (1/2) mass * (velocity * velocity)
    ! for now, we will assume (1/2) ≈ 1,
    ! when we support double precision we will worry about this
    r = (2/2) * mass * ((velocity/1000) ** 2)
end function

function calculate_total_energy(potential_energy, kinetic_energy) result(r)
    integer, intent(in) :: potential_energy ! input, newtons
    integer, intent(in) :: kinetic_energy   ! input, newtons
    integer             :: r                ! output, newtons
    r = potential_energy + kinetic_energy
 end function

program calculate_velocity
    implicit none
    integer :: apply_linear_acceleration
    integer :: calculate_position
    integer :: calculate_potential_energy
    integer :: calculate_kinetic_energy
    integer :: calculate_total_energy

    integer :: ACCELERATION     = -9810 ! mm/s/s
    integer :: mass             = 1000  ! mg
    integer :: initial_velocty  = 10000 ! mm/s
    integer :: current_velocity = 10000 ! mm/s
    integer :: initial_position = 200   ! mm (?, I think, haven't actually thought about this one)
    integer :: current_position = 0     ! mm (?, I think, haven't actually thought about this one)
    integer :: time             = 0     ! ms
    integer :: potential_energy = 0     ! newtons
    integer :: kinetic_energy   = 0     ! newtons
    integer :: total_energy     = 0     ! newtons

    potential_energy = calculate_potential_energy(mass, initial_position, -ACCELERATION)
    kinetic_energy   = calculate_kinetic_energy(mass, initial_velocty)
    total_energy     = calculate_total_energy(potential_energy, kinetic_energy)

    open (9, file = "velocity.output", status = "new")
    open (10, file = "position.output", status = "new")
    write(9, '(A)') "time, velocity"
    write(10, '(A)') "time, position"
    write(9, '(I0, A, I0)') 0, ", ", initial_velocty
    write(10, '(I0, A, I0)') 0, ", ", initial_position

    do while (total_energy > 0 .OR. time .EQ. 0)
        current_velocity = apply_linear_acceleration(1, current_velocity, ACCELERATION)
        current_position = calculate_position(1, current_position, current_velocity)
        potential_energy = calculate_potential_energy(mass, current_position, -ACCELERATION)
        kinetic_energy   = calculate_kinetic_energy(mass, current_velocity)
        total_energy     = calculate_total_energy(potential_energy, kinetic_energy)

        time = time + 1

        print*, "time: ", time, " current_velocity: ", current_velocity, " current_position: ", current_position, &
                " current_potential_energy: ", potential_energy, " current_kinetic_energy: ", kinetic_energy,     &
                " total_energy: ", total_energy

        write(9,'(I0, A, I0)') time, ", ", current_velocity
        write(10,'(I0, A, I0)') time, ", ", current_position
    end do

    close(9)
    close(10)

end program calculate_velocity
