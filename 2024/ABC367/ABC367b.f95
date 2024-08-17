program abc367b
    !X    ：実数
    !x_end：末尾が0でなくなる位置
    implicit none
    integer i, x_end
    character(1000) X

    !入力
    read (*, *) X

    !末尾0の検索
    do i = len_trim(X), 1, -1 !末尾が0でなくなる位置を検索
        if (X(i:i) == '.') then
            x_end = i - 1
            exit
        end if
        if (X(i:i) /= '0') then
            x_end = i
            exit
        end if
    end do

    !結果の出力
    do i = 1, x_end
        write (*, '(a1)', advance='no') X(i:i)
    end do
end program
