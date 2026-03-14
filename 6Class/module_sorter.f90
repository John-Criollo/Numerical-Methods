module module_sorter
  implicit none
  private

  public :: quicksort

  contains

  recursive subroutine quicksort(a)
    real, dimension(:), intent(inout) :: a
    integer :: p
    if (size(a) == 0) return
    p = partition(a)
    call quicksort(a(:p-1))
    call quicksort(a(p+1:))
  end subroutine quicksort

  integer function partition(a) result(i)
    real, dimension(:), intent(inout) :: a
    integer :: j
    i = 1
    do j = 1, size(a)
        if (a(j) < a(size(a))) then
            call swap(a(i), a(j))
            i = i + 1
        end if
    end do
    call swap(a(i), a(size(a)))
  end function partition

  elemental subroutine swap(a, b)
    real, intent(inout) :: a, b
    real :: temp
    temp = a
    a = b
    b = temp
  end subroutine swap

end module module_sorter
