program abc348a
    !N：ペナルティキック回数
    implicit none
    integer i
    integer N

    !入力
    read (*, *) N

    !結果の出力
    do i = 1, N
        if (mod(i, 3) == 0) then
            write (*, '(a)', advance='no') 'x'
        else
            write (*, '(a)', advance='no') 'o'
        end if
    end do
end program abc348a
