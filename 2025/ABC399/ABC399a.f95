program ABC399a
    ! N       : 文字列の長さ
    ! S_raw   : 入力用の固定長文字列バッファ
    ! T_raw   : 同上
    ! S, T    : 実際に比較する文字列（長さ N）
    ! i       : ループカウンタ
    ! dist    : ハミング距離（異なる文字の数）

    implicit none
    integer :: N, i, dist
    character(len=100) :: S_raw, T_raw
    character(len=:), allocatable :: S, T

    ! 入力
    read (*, *) N
    read (*, '(A)') S_raw
    read (*, '(A)') T_raw

    ! 実際の長さに切り詰め
    S = S_raw(1:N)
    T = T_raw(1:N)

    ! ハミング距離を計算
    dist = 0
    do i = 1, N
        if (S(i:i) /= T(i:i)) then
            dist = dist + 1
        end if
    end do

    ! 結果の出力
    write (*, '(I0)') dist

end program ABC399a
