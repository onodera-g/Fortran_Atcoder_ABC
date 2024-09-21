program abc372b
    ! M      : 入力整数
    ! A      : 結果を保持する配列（最大20要素）
    ! M_mod : Mを3で割った余り
    ! len_A  : 配列Aの現在の長さ
    implicit none
    integer :: M
    integer, dimension(20) :: A
    integer :: k, M_mod, len_A, i

    ! 入力
    read (*, *) M

    ! kを0から10までループ
    len_A = 0; A = 0
    do k = 0, 10
        M_mod = mod(M, 3)
        do i = 1, M_mod
            len_A = len_A + 1
            A(len_A) = k
        end do
        ! Mを3で整数除算
        M = M/3
    end do

    ! 結果の出力
    write (*, '(i0)') len_A
    write (*, '(*(i0,1x))', advance='no') (A(i), i=1, len_A)
end program abc372b
