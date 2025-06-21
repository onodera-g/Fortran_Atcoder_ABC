program ABC403_a
    ! N       : 整数列の長さ
    ! A       : 入力される長さ N の整数列 A(1…N)
    ! i       : ループ変数
    ! sum_odd : 奇数番目要素の合計を保持

    implicit none
    integer :: N
    integer, allocatable :: A(:)
    integer :: i
    integer :: sum_odd

    ! 入力
    read (*, *) N
    allocate (A(N))
    read (*, *) A

    ! 奇数番目の要素合計を計算
    sum_odd = 0
    do i = 1, N, 2
        sum_odd = sum_odd + A(i)
    end do

    ! 結果の出力
    print *, sum_odd

end program ABC403_a
