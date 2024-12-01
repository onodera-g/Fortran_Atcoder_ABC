program abc382a
    ! N    : 箱の総数
    ! D    : 高橋君がクッキーを食べる日数
    ! cnt  : 初期の空き箱（'.'）の数をカウントする変数
    ! S    : 各箱の状態を表す文字列（'@' はクッキーが入っている、'.' は空き箱）
    implicit none
    integer i
    integer N, D, cnt
    character(100) S
    ! 入力
    read (*, *) N, D
    read (*, *) S(1:N)

    ! 空き箱を数える
    cnt = 0
    do i = 1, N
        if (S(i:i) == ".") cnt = cnt + 1
    end do

    ! 結果の出力
    print *, cnt + D
end program abc382a
