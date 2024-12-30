program abc384a
    ! N : 文字列 S の長さ
    ! C1 : 置き換えの基準となる文字
    ! C2 : 置き換え後の文字
    ! S : 入力される文字列
    implicit none
    integer i
    integer N
    character(1) C1, C2
    character(:), allocatable :: S

    ! 入力
    read (*, *) N, C1, C2
    allocate (character(N)::S)
    read (*, *) S

    !
    do i = 1, N
        if (S(i:i) == C1) then
            write (*, '(a1)', advance='no') C1
        else
            write (*, '(a1)', advance='no') C2
        end if
    end do

end program abc384a
