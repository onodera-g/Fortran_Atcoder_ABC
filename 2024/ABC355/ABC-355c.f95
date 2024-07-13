program abc355c
    !A：各ターンででできた数字
    !N：ビンゴカードのサイズ
    !x：穴をあけるマス(x座標)
    !y：穴をあけるマス(y座標)
    !tate：ビンゴカード各列で空いている穴の数
    !yoko：ビンゴカード各行で空いている穴の数
    !naname1,2：ビンゴカードの斜めで空いている穴の数
    !cnt：ビンゴ判定用(cnt=Nならビンゴ)
    implicit none
    integer, allocatable::A(:), tate(:), yoko(:)
    integer N, T, x, y, i, cnt, naname1, naname2

    !入力
    read (*, *) N, T
    allocate (A(T), tate(N), yoko(N))
    read (*, *) A(:)

    !ビンゴの実行
    tate = 0; yoko = 0; naname1 = 0; naname2 = 0
    do i = 1, T
        !マス位置の特定
        if (mod(A(i), N) == 0) then
            y = A(i)/N
            x = N
        else
            y = A(i)/N + 1
            x = mod(A(i), N)
        end if
        !縦横斜めの空いてるますの数
        tate(x) = tate(x) + 1
        yoko(y) = yoko(y) + 1
        if (x == y) naname1 = naname1 + 1
        if (x + y == N + 1) naname2 = naname2 + 1
        !ビンゴ判定
        cnt = max(maxval(tate), maxval(yoko), naname1, naname2)
        if (cnt == N) then
            write (*, *) i
            stop
        end if
    end do
    write (*, *) - 1
end program abc355c
