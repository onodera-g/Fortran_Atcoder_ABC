program ABC407c
    ! S       : 目標とする文字列 (長さ N, 各文字は '0'～'9')
    ! N       : 文字列 S の長さ
    ! j       : ループカウンタ (1 ≤ j ≤ N)
    ! digit   : 現在の文字 S(j) を数値化した値 (0 ～ 9)
    ! f_next  : 次の位置 j+1 に対応する f_{j+1} の値
    ! f_curr  : 現在計算中の f_{j} の値
    ! diff    : f_next - digit の差
    ! k       : ceil(diff / 10) を計算するための値
    ! answer  : ボタン A を N 回、B を f_1 回押す合計回数 (N + f_1)
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none

    character(len=500000) :: S
    integer(int32) :: N, j, digit, f_next, f_curr, diff, k, answer

    ! 入力
    ! 入力文字列を行全体で読み込む
    read (*, '(A)') S
    N = len_trim(S)

    ! 後ろから f_j を貪欲に計算
    f_next = 0_int32
    do j = N, 1, -1
        digit = ichar(S(j:j)) - ichar('0')
        diff = f_next - digit
        if (diff <= 0) then
            f_curr = digit
        else
            k = (diff + 9_int32)/10_int32
            f_curr = digit + k*10_int32
        end if
        f_next = f_curr
    end do

    ! 結果を出力
    answer = N + f_curr
    print *, answer

end program ABC407c
