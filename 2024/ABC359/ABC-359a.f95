program abc358a
    implicit none
    character(10), allocatable:: S(:)
    integer N, i, cnt

    !入力
    read (*, *) N
    allocate (S(N))
    do i = 1, N
        read (*, *) S(i)
    end do
    cnt = 0

    do i = 1, N
        if (trim(S(i)) == "Takahashi") then
            cnt = cnt + 1
        end if
    end do

    write (*, *) cnt

end program abc358a
