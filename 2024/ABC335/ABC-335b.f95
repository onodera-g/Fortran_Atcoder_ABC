program abc335b
    !N：整数N
    implicit none
    integer N, i, j, k
    integer arr(4, 4)

    !入力
    read (*, *) N

    !N以下の非負整数の組 (x,y,z)の計算
    do i = 0, N
        do j = 0, N
            do k = 0, N
                if (i + j + k <= N) then
                    write (*, *) i, j, k
                end if
            end do
        end do
    end do
end program abc335b
