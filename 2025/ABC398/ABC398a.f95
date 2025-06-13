program ABC398a
    ! N    : 文字列の長さ (1 ≤ N ≤ 100)
    ! half : 両端の '-' の個数
    ! S    : 結果文字列 (固定長 100, 上位 N 文字を使用)
    ! i    : ループ用インデックス

    implicit none
    integer :: N, half, i
    character(len=100) :: S

    ! 入力
    read (*, *) N

    if (mod(N, 2) == 0) then
        ! 偶数長: 中央に "==" を置く
        half = (N - 2)/2
        ! 左側の '-' を設定
        do i = 1, half
            S(i:i) = '-'
        end do
        ! 中央の "=="
        S(half + 1:half + 2) = '=='
        ! 右側の '-'
        do i = half + 3, N
            S(i:i) = '-'
        end do
    else
        ! 奇数長: 中央に '=' を置く
        half = (N - 1)/2
        ! 左側の '-'
        do i = 1, half
            S(i:i) = '-'
        end do
        ! 中央の '='
        S(half + 1:half + 1) = '='
        ! 右側の '-'
        do i = half + 2, N
            S(i:i) = '-'
        end do
    end if

    ! 結果の出力
    write (*, '(A)') S(1:N)

end program ABC398a
