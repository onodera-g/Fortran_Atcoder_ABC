program abc345b
    !X  ：整数
    !ans：X/10の結果
    implicit none
    integer(16) X, ans

    !入力
    read (*, *) X

    !X/10の計算
    if (X >= 0) then
        ans = (X + 9)/10
    else
        ans = X/10
    end if

    !結果出力
    write (*, *) ans

end program abc345b
