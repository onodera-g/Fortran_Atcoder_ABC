program abc342a
    !S   ：英小文字からなる文字列
    !mozi：Sの英小文字の出現回数記録用
    implicit none
    integer i, j, mozi(127)
    character(100) S

    !入力
    read (*, *) S
    mozi = 0

    !出現回数のカウント
    do i = 1, len_trim(S)
        mozi(ichar(S(i:i))) = mozi(ichar(S(i:i))) + 1
    end do

    !結果の出力
    do i = 1, 127
        if (mozi(i) == 1) then
            do j = 1, len_trim(S)
                if (S(j:j) == char(i)) then
                    write (*, *) j
                    stop
                end if
            end do
        end if
    end do
end program abc342a
