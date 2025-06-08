program ABC395b
    ! N      : 正方形の一辺の長さ (奇数/偶数 問わず)
    ! i, j   : ループ用行・列インデックス (1 ≤ i, j ≤ N)
    ! d      : (i,j) のセルが属する同心正方形の深さ、外側から 0,1,2,... を表す

    use, intrinsic :: iso_fortran_env
    implicit none

    integer(int32) :: N, i, j, depth

    ! 入力
    read (*, *) N

    ! N×N グリッドを出力
    do i = 1, N
        do j = 1, N
            ! 各セルの環津の深さは外側からの最小距離
            depth = min(i - 1, j - 1)
            depth = min(depth, N - i)
            depth = min(depth, N - j)
            if (mod(depth, 2) == 0) then
                write (*, '(A)', advance='no') '#'
            else
                write (*, '(A)', advance='no') '.'
            end if
        end do
        write (*, *) ! 行末の改行
    end do

end program ABC395b
