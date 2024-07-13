program abc356a
    !N：数列Aの長さ
    !A：数列
    !L：逆順に並べるスタート位置
    !R：逆順に並べるゴール位置
    implicit none
    integer N, L, R, i
    integer, allocatable:: A(:)

    !入力
    read (*, *) N, L, R
    allocate (A(N))

    !A初期化
    do i = 1, N
        A(i) = i
    end do

    !入れ替え
    do i = L, R
        A(i) = L + (R - i)
    end do

    !結果の出力
    write (*, *) A
end program abc356a
