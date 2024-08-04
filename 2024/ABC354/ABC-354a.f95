program abc354a
    !H   ：高橋の身長
    !tree：植物の身長
    implicit none
    integer i
    integer H, tree

    !入力
    read (*, *) H

    !背くらべ
    i = 0; tree = 0
    do
        tree = tree + 2**i
        if (tree > H) then
            i = i + 1
            exit
        end if
        i = i + 1
    end do

    !結果の出力
    write (*, *) i

end program abc354a
