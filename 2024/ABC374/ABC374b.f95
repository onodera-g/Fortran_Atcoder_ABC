program abc374b
    ! S : 文字列S
    ! T : 文字列T
    implicit none
    integer i
    character(100) S, T

    !入力
    read (*, *) S
    read (*, *) T

    ! 完全一致の場合
    if (S == T) then
        print *, 0
        stop
    end if

    !
    do i = 1, max(len_trim(S), len_trim(T))
        if (S(i:i) == T(i:i)) then
            cycle
        else
            write (*, *) i
            stop
        end if

    end do
end program abc374b
