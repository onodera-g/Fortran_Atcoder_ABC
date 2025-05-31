program ABC392c
    ! N        : 人の数
    ! P(1:N)   : 各人 i が見つめている人の番号
    ! Q(1:N)   : 各人 i のゼッケン番号
    ! invQ(1:N): ゼッケン番号から人番号への逆写像
    ! S(1:N)   : 結果 S_i (i 番のゼッケンを着けた人が見つめている人のゼッケン)
    ! i        : ループ変数

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none

    integer(int32) :: N, i
    integer(int32), allocatable :: P(:), Q(:), invQ(:), S(:)

    ! 入力
    read (*, *) N
    allocate (P(N), Q(N), invQ(N), S(N))
    read (*, *) (P(i), i=1, N)
    read (*, *) (Q(i), i=1, N)

    ! Q の逆写像を構築
    do i = 1, N
        invQ(Q(i)) = i
    end do

    ! S_i を計算: i のゼッケンを着けた人 -> invQ(i) 番の人が見つめる人 P(invQ(i)) のゼッケン Q(...)
    do i = 1, N
        S(i) = Q(P(invQ(i)))
    end do

    ! 結果の出力
    write (*, '(*(I0,1X))') (S(i), i=1, N)

end program ABC392c
