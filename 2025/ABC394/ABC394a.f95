program ABC394a
    ! S      : 入力される数字文字列 (長さ 1～100, 必ず '2' を含む)
    ! n      : 文字列 S の有効長
    ! i      : ループ変数

    implicit none
    character(len=100) :: S
    integer :: n, i

    !　入力
    read (*, *) S
    n = len_trim(S)

    ! '2' のみを順序を保って結果を出力
    do i = 1, n
        if (S(i:i) == '2') then
            write (*, '(A)', advance='no') '2'
        end if
    end do
end program ABC394a
