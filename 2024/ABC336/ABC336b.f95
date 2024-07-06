program abc336b
    !N  ：整数N
    !cnt：末尾に連続する0の数
    implicit none
    integer N
    integer cnt, i

    !入力
    read (*, *) N
    cnt = 0
    do i = 0, 40
        if (btest(N, i)) then
            exit
        else
            cnt = cnt + 1
        end if
    end do

    !結果の出力
    write (*, *) cnt
end program abc336b
