program abc338c
    !x     ：料理Aの作成個数
    !y     ：材料B(i)のみ作成できる料理Bの数
    !min_y ： 料理Bの作成個数
    !max_xy：料理ABの最大個数
    !Q     ：初期の材料在庫
    !A,B   ：料理A,Bを作成するのに必要な材料個数
    !Qr    ：料理A作成後に余ったの材料在庫
    implicit none
    integer i, j, x, y, min_y, max_xy
    integer N
    integer, allocatable::Q(:), A(:), B(:), Qr(:)

    !入力
    read (*, *) N
    allocate (Q(N), A(N), B(N), Qr(N))
    read (*, *) (Q(i), i=1, N)
    read (*, *) (A(i), i=1, N)
    read (*, *) (B(i), i=1, N)

    !料理作成数の組み合わせ検証
    min_y = 1000000000
    max_xy = 0; x = 0
    L1: do
        L2: do i = 1, N
            Qr(i) = Q(i) - A(i)*x
            if (Qr(i) < 0) exit L1
            if (B(i) == 0) cycle
            y = Qr(i)/B(i)
            min_y = min(y, min_y)
        end do L2
        max_xy = max(max_xy, min_y + x)
        x = x + 1
    end do L1

    !結果の出力
    write (*, *) max_xy
end program abc338c
