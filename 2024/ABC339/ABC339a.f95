program abc339a
    !Ans：.より末尾の文字列
    !S  ：文字列S
    implicit none
    integer i, cnt
    character(100) S
    character(1) Ans(100)

    !入力
    read (*, *) S
    cnt = 0

    !.を含む文字列の分割
    do i = len_trim(S), 1, -1
        if (S(i:i) /= ".") then
            cnt = cnt + 1
            Ans(cnt) = S(i:i)
        else
            exit
        end if
    end do

    !結果の出力
    write (*, *) (Ans(i), i=cnt, 1, -1)
end program abc339a
