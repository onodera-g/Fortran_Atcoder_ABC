program ABC391b
    ! N       : S グリッドの行数・列数 (N×N)
    ! M       : T グリッドの行数・列数 (M×M)
    ! S(1:N)  : N×N グリッド S を文字列配列で保持
    ! T(1:M)  : M×M グリッド T を文字列配列で保持
    ! i, j    : S 内での探索開始位置のループカウンタ
    ! di, dj  : グリッド T 内の走査用ループカウンタ
    ! match   : サブマトリクスが一致したかを示す論理変数
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none

    integer(int32) :: N, M
    integer(int32) :: i, j, di, dj
    character(len=:), allocatable :: S(:), T(:)
    logical :: match

    ! 入力
    read (*, *) N, M
    allocate (character(len=N) :: S(N))
    allocate (character(len=M) :: T(M))
    do i = 1, N
        read (*, '(A)') S(i)
    end do
    do i = 1, M
        read (*, '(A)') T(i)
    end do

    ! 探索 (上から i 行目, 左から j 列目 を左上とする M×M サブマトリクス)
    do i = 1, N - M + 1
        do j = 1, N - M + 1
            match = .true.
            do di = 1, M
                do dj = 1, M
                    if (S(i + di - 1) (j + dj - 1:j + dj - 1) /= T(di) (dj:dj)) then
                        match = .false.
                        exit
                    end if
                end do
                if (.not. match) exit
            end do
            if (match) then
                ! 見つかったら結果を出力してプログラム終了
                print *, i, j
                stop
            end if
        end do
    end do

end program ABC391b
