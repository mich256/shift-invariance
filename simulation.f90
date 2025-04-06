! simulation.f90
program simulation
  use, intrinsic :: iso_fortran_env, only: dp => real64
  implicit none

  integer, parameter :: m = 10, n = 10
  double precision :: lpp_val, logZ
  double precision, parameter :: rate_exp = 1.0d0, rate_inv = 1.0d0

  call random_seed()  ! Initialize the random number generator

  ! Run the LPP simulation on an m x n grid with exponential weights.
  call simulate_lpp(m, n, rate_exp, lpp_val)
  print*, "LPP value for a", m, "x", n, "grid (exponential weights):", lpp_val

  ! Run the polymer simulation on an m x n grid with inverse gamma weights.
  call simulate_polymer(m, n, rate_inv, logZ)
  print*, "Log partition function (polymer) for a", m, "x", n, "grid:", logZ

end program simulation

!---------------------------------------------------------------------
module random_distributions
  use, intrinsic :: iso_fortran_env, only: dp => real64
  implicit none
contains

  !-----------------------------------------------------
  ! Function: rexp
  ! Generates an exponential random number with given rate.
  ! If U ~ Uniform(0,1) then X = -log(U)/rate.
  !-----------------------------------------------------
  function rexp(lambda) result(x)
    double precision, intent(in) :: lambda
    double precision :: x, u
    call random_number(u)
    if (u == 0.0d0) u = 1.0d-10
    x = -log(u) / lambda
  end function rexp

  !-----------------------------------------------------
  ! Function: rinvgamma
  ! Generates a random number from an inverse gamma distribution.
  ! We define rinvgamma(shape, rate) = 1 / (gamma sample)
  ! For shape = 1, gamma(1,rate) is just exponential.
  ! For shape = 2, gamma(2,rate) = sum of two independent exponential(rate) samples.
  ! Only shapes 1 and 2 are supported in this simple implementation.
  !-----------------------------------------------------
  function rinvgamma(shape, rate) result(x)
    double precision, intent(in) :: shape, rate
    double precision :: x, gamma_sample
    if (abs(shape - 1.0d0) < 1e-6_dp) then
      gamma_sample = rexp(rate)
    else if (abs(shape - 2.0d0) < 1e-6_dp) then
      gamma_sample = rexp(rate) + rexp(rate)
    else
      print*, "rinvgamma: Unsupported shape parameter:", shape
      stop 1
    end if
    x = 1.0d0 / gamma_sample
  end function rinvgamma

end module random_distributions

!---------------------------------------------------------------------
subroutine simulate_lpp(m, n, rate, lpp_value)
  use, intrinsic :: iso_fortran_env, only: dp => real64
  use random_distributions
  implicit none
  integer, intent(in) :: m, n
  double precision, intent(in) :: rate
  double precision, intent(out) :: lpp_value
  double precision, allocatable :: grid(:,:), L(:,:)
  integer :: i, j

  allocate(grid(m, n))
  allocate(L(m, n))

  ! Generate an m x n grid of exponential weights.
  do i = 1, m
     do j = 1, n
        grid(i, j) = rexp(rate)
     end do
  end do

  ! Compute the last-passage percolation value via dynamic programming.
  L(1,1) = grid(1,1)
  do j = 2, n
     L(1,j) = L(1, j-1) + grid(1,j)
  end do
  do i = 2, m
     L(i,1) = L(i-1, 1) + grid(i,1)
  end do
  do i = 2, m
     do j = 2, n
        L(i,j) = grid(i,j) + max(L(i-1,j), L(i,j-1))
     end do
  end do

  lpp_value = L(m, n)
  deallocate(grid)
  deallocate(L)
end subroutine simulate_lpp

!---------------------------------------------------------------------
subroutine simulate_polymer(m, n, rate, logZ)
  use, intrinsic :: iso_fortran_env, only: dp => real64
  use random_distributions
  implicit none
  integer, intent(in) :: m, n
  double precision, intent(in) :: rate
  double precision, intent(out) :: logZ
  double precision, allocatable :: grid(:,:), L(:,:)
  integer :: i, j
  double precision :: temp

  allocate(grid(m, n))
  allocate(L(m, n))

  ! Generate an m x n grid of inverse gamma weights.
  ! Here we use shape = 1 for simplicity.
  do i = 1, m
     do j = 1, n
        grid(i, j) = rinvgamma(1.0d0, rate)
     end do
  end do

  ! Compute the log partition function using dynamic programming in log-space.
  ! We define L(i,j) = log(grid(i,j)) + log(sum of exponentials from predecessors).
  L(1,1) = log(grid(1,1))
  do j = 2, n
     L(1,j) = log(grid(1,j)) + L(1, j-1)
  end do
  do i = 2, m
     L(i,1) = log(grid(i,1)) + L(i-1, 1)
  end do
  do i = 2, m
     do j = 2, n
        ! For numerical stability, use the log-sum-exp trick.
        temp = max(L(i-1,j), L(i,j-1))
        L(i,j) = log(grid(i,j)) + ( temp + log( exp(L(i-1,j)-temp) + exp(L(i,j-1)-temp) ) )
     end do
  end do

  logZ = L(m, n)
  deallocate(grid)
  deallocate(L)
end subroutine simulate_polymer
