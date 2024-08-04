program abc363a
    !R  ：レートの値
    !Ans：^を増やすために必要なレート
    implicit none
    integer R, Ans

    !入力
    read (*, *) R

    !必要レートの計算
    if (1 <= R .and. R <= 99) then
        Ans = 100 - R
    elseif (100 <= R .and. R <= 199) then
        Ans = 200 - R
    elseif (200 <= R .and. R <= 299) then
        Ans = 300 - R
    else
        stop
    end if

    !結果の出力
    write (*, *) Ans

end program abc363a
