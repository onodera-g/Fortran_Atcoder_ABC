program test
    implicit none
    integer(16) Sx, Sy, Tx, Ty
    integer(16) Ans, tmp

    !入力
    read (*, *) Sx, Sy
    read (*, *) Tx, Ty

    !
    tmp = mod(Sy - Sx, 2)
    Sx = Sx - tmp
    tmp = mod(Ty - Tx, 2)
    Tx = Tx - tmp

    Tx = abs(Tx - Sx)
    Ty = abs(Ty - Sy)

    Ans = (Ty + max(0, Tx - Ty))/2
    write (*, *) Ans
end
