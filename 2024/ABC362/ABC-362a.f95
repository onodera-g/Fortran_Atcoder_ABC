program abc362a
    !price：各ペンの値段(1赤、2緑、3青)
    !Cn   ：嫌いなペンの色に対応する値段の参照用
    !C    ：嫌いなペンの色
    !X    ：購入するペンの値段
    implicit none
    integer i
    integer price(3), Cn
    character(1) C
    integer X

    !入力
    read (*, *) price(:)
    read (*, *) C

    !嫌いなペンの色の分類
    if (C == "R") Cn = 1
    if (C == "G") Cn = 2
    if (C == "B") Cn = 3

    !ペンの値段の最小値検索
    X = 101
    do i = 1, 3
        if (i /= Cn) then
            X = min(X, price(i))
        end if
    end do

    !結果の出力
    write (*, *) X

end program abc362a
