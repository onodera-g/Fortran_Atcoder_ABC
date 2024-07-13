program abc357a
    !N：宇宙人の数
    !M：消毒できる手の合計数
    !H：各宇宙人の持っている腕の数
    implicit none
    integer i
    integer N, M, tmp
    integer, allocatable::H(:)

    !入力
    read (*, *) N, M
    allocate (H(N))
    read (*, *) H(:)

    !消毒液の消費
    tmp = M
    do i = 1, N
        if (tmp < H(i)) then
            write (*, *) i - 1
            stop
        else
            tmp = tmp - H(i)
        end if
    end do
    write (*, *) N
end program abc357a
