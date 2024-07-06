program abc358b
    implicit none
    integer N, A
    integer tmp, i
    integer, allocatable::T(:)

    !入力
    read (*, *) N, A
    allocate (T(N))
    read (*, *) T(:)
    tmp = 0

    !待ち列の処理
    do i = 1, N
        if (tmp > T(i)) then
            tmp = tmp + A
        else
            tmp = T(i) + A
        end if
        write (*, *) tmp
    end do

end program abc358b
