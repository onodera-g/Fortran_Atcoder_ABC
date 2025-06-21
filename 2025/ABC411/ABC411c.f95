program ABC411c
    ! N        : マスの数
    ! Q        : クエリ数
    ! A        : クエリで与えられるマス番号の配列
    ! black    : 各マスが黒かどうかのフラグ配列
    ! ans      : 現在の黒区間の数
    ! i        : ループカウンタ
    ! pos      : 今回反転するマス番号

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    integer(int32) :: N, Q, i, pos
    integer(int32), allocatable :: A(:)
    logical, allocatable :: black(:)
    integer(int32) :: ans

    ! 入力
    read (*, *) N, Q
    allocate (A(Q))
    read (*, *) (A(i), i=1, Q)

    ! 各クエリを処理
    allocate (black(N)) ! 初期状態: 全マス白 (false), 黒区間は 0
    black = .false.
    ans = 0
    do i = 1, Q
        pos = A(i)
        if (.not. black(pos)) then
            ! 白→黒 に反転する場合
            ! 左右のマスが黒かどうかを調べる
            if (pos > 1 .and. black(pos - 1)) then
                if (pos < N .and. black(pos + 1)) then
                    ! 黒 ■■■ の間に挿入 → 2 区間が 1 つに統合
                    ans = ans - 1
                else
                    ! 左側とつながる → 区間数は変わらない
                end if
            else
                if (pos < N .and. black(pos + 1)) then
                    ! 右側とつながる → 区間数は変わらない
                else
                    ! 周囲白 → 新しく孤立区間ができる
                    ans = ans + 1
                end if
            end if
            black(pos) = .true.
        else
            ! 黒→白 に反転する場合
            if (pos > 1 .and. black(pos - 1)) then
                if (pos < N .and. black(pos + 1)) then
                    ! ■■■ の中央を消す → 1 区間が 2 区間に分裂
                    ans = ans + 1
                else
                    ! 左側とつながった末端を消す → 区間数は変わらない
                end if
            else
                if (pos < N .and. black(pos + 1)) then
                    ! 右側とつながった先頭を消す → 区間数は変わらない
                else
                    ! 孤立区間を消す → 区間が消滅
                    ans = ans - 1
                end if
            end if
            black(pos) = .false.
        end if

        ! 現在の黒区間数を出力
        write (*, '(I0)') ans
    end do

end program ABC411c
