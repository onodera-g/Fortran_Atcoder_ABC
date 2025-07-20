program abc415b
    ! S                : 倉庫の状態を表す文字列
    ! pos(1000)        : 荷物がある区画番号を格納する配列
    ! i                : ループ変数
    ! m                : 荷物の個数
    ! len_s            : S の長さ
    ! count            : pos のインデックスを管理する変数
    implicit none
    character(len=1000) :: S
    integer :: pos(1000)
    integer :: i, m, len_s, count

    ! 入力
    read (*, *) S

    ! 荷物の位置を記録
    len_s = len_trim(S)
    count = 0
    do i = 1, len_s
        if (S(i:i) == '#') then
            count = count + 1
            pos(count) = i
        end if
    end do

    m = count

    ! 荷物の個数は偶数かつ2以上
    ! 2個ずつ取り出して出力
    do i = 1, m, 2
        write (*, '(i0,a1,i0)') pos(i), ",", pos(i + 1)
    end do

end program abc415b
