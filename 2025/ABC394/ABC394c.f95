program ABC394c
    ! n : 入力される文字列 S の有効長 (1 ≤ n ≤ 300000)
    ! i : ループ変数。右端から左方向にインデックスを移動させる (i = n-1, …, 1)
    ! S : 固定長バッファ。入力された文字列を左詰めで格納するための配列 (長さ 300000)

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    integer(int32) :: n, i
    character(len=300000) :: S

    ! 入力
    read (*, '(A)') S
    n = len_trim(S)

    ! 右端から左方向に走査し、"WA" → "AC" に置換
    do i = n - 1, 1, -1
        if (S(i:i) == 'W' .and. S(i + 1:i + 1) == 'A') then
            S(i:i) = 'A'
            S(i + 1:i + 1) = 'C'
        end if
    end do

    !　結果の出力
    write (*, '(A)') S(1:n)

end program ABC394c
