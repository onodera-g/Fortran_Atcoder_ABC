program abc378d
    ! H       : グリッドの高さ（行数）
    ! W       : グリッドの幅（列数）
    ! K       : 移動回数の制限
    ! S       : グリッドの状態を表す配列 (. は空きマス、# は障害物 )
    ! cnt     : 有効な経路の総数
    ! visited : 現在の経路で訪問済みのセルを示す配列 (  true が訪問済み )
    ! DI_DIR  : 移動方向の行方向の変化量（上, 下, 左, 右）
    ! DJ_DIR  : 移動方向の列方向の変化量（上, 下, 左, 右）
    implicit none
    integer i, j
    integer H, W, K, cnt
    character(10) line
    character(1), allocatable :: S(:, :)
    logical, allocatable :: visited(:, :)
    integer :: DI_DIR(4) = [-1, 1, 0, 0]
    integer :: DJ_DIR(4) = [0, 0, -1, 1]

    ! 入力
    read (*, *) H, W, K ! 標準入力からグリッドの高さ、幅、移動回数を読み込む
    allocate (S(H, W), visited(H, W))
    do i = 1, H
        read (*, '(A)') line
        do j = 1, W
            S(i, j) = line(j:j) ! 文字列の各文字を2次元配列Sに割り当てる
        end do
    end do

    ! 経路の探索
    cnt = 0
    do i = 1, H
        do j = 1, W
            if (S(i, j) == '.') then ! 空きマスであれば出発点
                visited = .false. ! 訪問済み配列を初期化
                visited(i, j) = .true. ! 出発点を訪問済み
                ! DFS
                cnt = cnt + dfs(i, j, 0, H, W, K, S, visited)
            end if
        end do
    end do

    ! 結果の出力
    print *, cnt ! 計算された有効な経路の総数を出力

contains

    recursive function dfs(i_current, j_current, steps, H, W, K, S, visited) result(cnt)
        integer i_current, j_current, steps
        integer ni, nj, d, cnt, tmp
        integer H, W, K
        character(1), intent(in) :: S(:, :)
        logical visited(:, :)

        cnt = 0
        if (steps == K) then
            cnt = 1 ! 有効な経路を1つカウント
            return
        end if

        ! 4方向へ移動
        do d = 1, 4
            ni = i_current + DI_DIR(d)
            nj = j_current + DJ_DIR(d)
            ! 移動先がグリッド内にあり、空きマスで未訪問の場合
            if (ni >= 1 .and. ni <= H .and. nj >= 1 .and. nj <= W) then
                if (S(ni, nj) == '.' .and. .not. visited(ni, nj)) then
                    visited(ni, nj) = .true. ! 移動先を訪問済みにマーク
                    tmp = dfs(ni, nj, steps + 1, H, W, K, S, visited) ! 再帰呼び出し
                    cnt = cnt + tmp ! 得られた経路数を合計
                    visited(ni, nj) = .false. ! バックトラックとして訪問マークを解除
                end if
            end if
        end do
    end function dfs

end program abc378d
