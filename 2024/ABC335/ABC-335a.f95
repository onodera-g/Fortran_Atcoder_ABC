program abc335a
    !S：文字列S
    implicit none
    integer i
    character(100) S

    !入力
    read (*, *) S

    !送料の計算
    do i = 100, 1, -1
        if (S(i:i) == "3") then
            S(i:i) = "4"
            exit
        end if
    end do

    !結果の出力
    write (*, *) S
end program abc335a
