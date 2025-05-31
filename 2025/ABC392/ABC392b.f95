program ABC392b
    ! N    : 全体の整数の上限 (1 ≤ M ≤ N ≤ 1000)
    ! M    : 与えられる異なる整数の個数
    ! A(1:M)       : 与えられる M 個の整数（互いに異なる）
    ! present(1:N) : 1～N の整数が A に含まれるかどうかを示す論理配列
    ! cnt          : A に含まれない整数の個数
    ! i            : ループ変数

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none

    integer(int32) :: N, M, i, cnt
    integer(int32), allocatable :: A(:)
    logical, allocatable      :: present(:)

    ! 入力
    read (*, *) N, M
    allocate (A(M), present(N))
    read (*, *) (A(i), i=1, M)

    ! どの整数が含まれるかをマーク
    present = .false.
    do i = 1, M
        present(A(i)) = .true.
    end do

    ! 含まれない整数を数え上げ
    cnt = count(.not. present)

    ! 結果の出力
    print *, cnt
    if (cnt > 0) then
        ! 昇順でスペース区切りに出力
        do i = 1, N
            if (.not. present(i)) then
                write (*, '(I0,1X)', advance='no') i
            end if
        end do
        write (*, *) ! 改行
    end if

end program ABC392b
