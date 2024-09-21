program abc372a
    ! S : 入力文字列（最大100文字）
    implicit none
    integer :: i
    character(len=100) :: S

    ! 入力
    read (*, *) S

    ! 結果の出力
    do i = 1, len_trim(S)
        if (S(i:i) /= '.') then
            write (*, '(A)', advance='no') S(i:i)
        end if
    end do
end program abc372a
