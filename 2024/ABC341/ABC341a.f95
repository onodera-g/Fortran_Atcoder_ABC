program abc341a
    !N   ：正整数
    implicit none
    integer N, i

    !入力
    read (*, *) N

    !0と1の出力
    do i = 1, N
        write (*, '(i0)', advance='no') 10
    end do
    write (*, '(i0)', advance='no') 1
end program abc341a
