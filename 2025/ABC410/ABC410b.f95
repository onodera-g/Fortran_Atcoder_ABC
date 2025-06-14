program ABC410b
    ! N      : 箱の数
    ! Q      : ボールの総数
    ! i,j    : ループ用カウンタ
    ! x      : 現在のクエリ値
    ! target : ボールを入れる箱番号
    ! best_idx, best_cnt : 最小のボール数を持つ箱の探索用
    ! Xarr   : クエリ入力配列
    ! Barr   : 各ボールの入れた箱番号を保持
    ! cnt    : 箱ごとの現在のボール数

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    integer(int32) :: N, Q, i, j
    integer(int32) :: x, target, best_idx, best_cnt
    integer(int32), allocatable :: Xarr(:), Barr(:), cnt(:)

    ! 入力
    read (*, *) N, Q
    allocate (Xarr(Q), Barr(Q), cnt(N))
    cnt = 0
    read (*, *) (Xarr(i), i=1, Q)

    ! ボール処理
    do i = 1, Q
        x = Xarr(i)
        if (x == 0) then
            best_idx = 1
            best_cnt = cnt(1)
            do j = 2, N
                if (cnt(j) < best_cnt) then
                    best_cnt = cnt(j)
                    best_idx = j
                end if
            end do
            target = best_idx
        else
            target = x
        end if
        ! ボールを箱に入れ、カウント更新
        cnt(target) = cnt(target) + 1
        Barr(i) = target
    end do

    ! 結果の出力
    write (*, '(I0)', advance='no') Barr(1)
    do i = 2, Q
        write (*, '(1X,I0)', advance='no') Barr(i)
    end do

end program ABC410b
