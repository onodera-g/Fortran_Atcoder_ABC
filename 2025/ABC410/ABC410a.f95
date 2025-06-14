program ABC410a
    ! N        : レースの総数
    ! A(i)     : i 番目のレースに出場できる最大年齢
    ! K        : 出場させたい馬の年齢
    ! result   : K 歳の馬が出場可能なレースの数
    ! i        : ループカウンタ

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    integer(int32) :: N, K, result, i
    integer(int32), allocatable :: A(:)

    ! 入力
    read (*, *) N
    allocate (A(N))
    read (*, *) (A(i), i=1, N)
    read (*, *) K

    ! カウント
    result = 0
    do i = 1, N
        if (A(i) >= K) result = result + 1
    end do

    ! 結果の出力
    write (*, '(I0)') result

end program ABC410a
