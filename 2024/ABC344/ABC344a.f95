program abc344a
    !S：文字列
    implicit none
    integer i
    character(1000) S

    !入力
    read (*, *) S

    !結果の出力
    i = 1
    do while (i <= len_trim(S))
        if (S(i:i) == '|') then
            do
                i = i + 1
                if (S(i:i) == '|') exit
            end do
        else
            write (*, '(a1)', advance='no') S(i:i)
        end if
        i = i + 1
    end do
end program abc344a
