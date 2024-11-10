program abc379a
    ! a : 100の位
    ! b : 10の位
    ! c : 1の位
    implicit none
    integer(16) a, b, c

    ! 入力
    read (*, '(3i1)') a, b, c

    ! 出力
    write (*, '(3i1)') b, c, a
    write (*, '(3i1)') c, a, b

end program abc379a
