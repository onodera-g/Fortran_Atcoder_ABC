program ABC407d
    ! H, W       : グリッドの行数・列数 (HW ≤ 20)
    ! A_flat(u)  : マス (1~n) に対応する値を一次元配列で保持
    ! visited(u) : マスが処理済み (未覆い or ドミノで覆い) かを管理
    ! best       : 最大スコア (未覆いマスの XOR 最大値)

    use, intrinsic :: iso_fortran_env, only: int32, int64
    implicit none

    integer(int32) :: H, W, n, i
    integer(int64), allocatable :: A_flat(:)
    logical, allocatable       :: visited(:)
    integer(int64)             :: best, curr_xor

    ! 入力読み込み
    read (*, *) H, W
    n = H*W
    allocate (A_flat(n), visited(n))
    ! 値をまとめて読み込み
    read (*, *) (A_flat(i), i=1, n)
    visited = .false.
    best = 0_int64

    ! 再帰 DFS 開始: 位置 1 から試す
    call dfs(1, 0_int64)

    ! 結果出力
    print *, best

contains

    recursive subroutine dfs(start_u, curr_xor)
        integer(int32), intent(in) :: start_u
        integer(int64), intent(in) :: curr_xor
        integer(int32) :: u, v, i0, j0

        ! 未処理のマスを start_u 以降で見つける
        u = -1
        do v = start_u, n
            if (.not. visited(v)) then
                u = v
                exit
            end if
        end do
        if (u == -1) then
            ! 全マス処理済み → スコア更新
            if (curr_xor > best) best = curr_xor
            return
        end if

        ! マス u を未覆いとして XOR に含める場合
        visited(u) = .true.
        call dfs(u + 1, ieor(curr_xor, A_flat(u)))
        visited(u) = .false.

        ! マス u にドミノを置く場合 (右 or 下)
        i0 = (u - 1)/W + 1 ! 行番号
        j0 = mod(u - 1, W) + 1 ! 列番号

        ! 右隣に置く
        if (j0 < W) then
            v = u + 1
            if (.not. visited(v)) then
                visited(u) = .true.
                visited(v) = .true.
                call dfs(u + 1, curr_xor)
                visited(u) = .false.
                visited(v) = .false.
            end if
        end if
        ! 下隣に置く
        if (i0 < H) then
            v = u + W
            if (.not. visited(v)) then
                visited(u) = .true.
                visited(v) = .true.
                call dfs(u + 1, curr_xor)
                visited(u) = .false.
                visited(v) = .false.
            end if
        end if

    end subroutine dfs

end program ABC407d
