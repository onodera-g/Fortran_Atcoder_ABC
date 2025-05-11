program abc387d
    ! H     : グリッドの行数
    ! W     : グリッドの列数
    ! S     : グリッドの行文字列配列
    ! si    : スタートマスの行番号
    ! sj    : スタートマスの列番号
    ! gi    : ゴールマスの行番号
    ! gj    : ゴールマスの列番号
    ! dist  : BFS用の距離配列(サイズ H×W×3)、-1は未訪問
    ! q_i   : BFSキューの行番号格納配列
    ! q_j   : BFSキューの列番号格納配列
    ! q_d   : BFSキューの last_dir インデックス配列(1:初期,2:縦,3:横)
    ! head  : キューの先頭インデックス
    ! tail  : キューの末尾インデックス
    ! nei_i : 隣接セルの行番号
    ! nei_j : 隣接セルの列番号
    ! nei_d : 隣接セルの new last_dir インデックス
    ! ld    : 現在のセルの last_dir インデックス
    ! ans   : 結果(最小移動回数)。到達不可時は -1
    implicit none
    integer :: H, W
    character(len=:), allocatable :: S(:)
    integer :: si, sj, gi, gj
    integer, allocatable :: dist(:, :, :)
    integer :: i, j, maxq
    integer, allocatable :: q_i(:), q_j(:), q_d(:)
    integer :: head, tail
    integer :: nei_i, nei_j, nei_d, ld
    integer :: ans

    ! 入力とグリッド読み込み
    read (*, *) H, W
    allocate (character(len=W) :: S(H))
    do i = 1, H
        read (*, *) S(i)
    end do

    ! スタート・ゴール位置の検出
    do i = 1, H
        do j = 1, W
            if (S(i) (j:j) == 'S') then
                si = i; sj = j
            else if (S(i) (j:j) == 'G') then
                gi = i; gj = j
            end if
        end do
    end do

    ! BFS準備
    allocate (dist(H, W, 3))
    dist = -1
    maxq = H*W*3
    allocate (q_i(maxq), q_j(maxq), q_d(maxq))
    head = 1; tail = 0

    ! キューにスタートを登録 (last_dir=1:初期)
    tail = tail + 1
    q_i(tail) = si; q_j(tail) = sj; q_d(tail) = 1
    dist(si, sj, 1) = 0

    ! BFSループ
    do while (head <= tail)
        i = q_i(head)
        j = q_j(head)
        ld = q_d(head)
        head = head + 1

        ! ゴール到達チェック
        if (i == gi .and. j == gj) then
            ans = dist(i, j, ld)
            exit
        end if

        ! 縦移動 (直前移動が縦でなければ)
        if (ld /= 2) then
            do nei_i = i - 1, i + 1, 2
                nei_j = j; nei_d = 2
                if (nei_i >= 1 .and. nei_i <= H) then
                    if (S(nei_i) (nei_j:nei_j) /= '#') then
                        if (dist(nei_i, nei_j, nei_d) == -1) then
                            dist(nei_i, nei_j, nei_d) = dist(i, j, ld) + 1
                            tail = tail + 1
                            q_i(tail) = nei_i
                            q_j(tail) = nei_j
                            q_d(tail) = nei_d
                        end if
                    end if
                end if
            end do
        end if

        ! 横移動 (直前移動が横でなければ)
        if (ld /= 3) then
            do nei_j = j - 1, j + 1, 2
                nei_i = i; nei_d = 3
                if (nei_j >= 1 .and. nei_j <= W) then
                    if (S(nei_i) (nei_j:nei_j) /= '#') then
                        if (dist(nei_i, nei_j, nei_d) == -1) then
                            dist(nei_i, nei_j, nei_d) = dist(i, j, ld) + 1
                            tail = tail + 1
                            q_i(tail) = nei_i
                            q_j(tail) = nei_j
                            q_d(tail) = nei_d
                        end if
                    end if
                end if
            end do
        end if
    end do

    ! 結果出力
    if (i == gi .and. j == gj) then
        print *, ans
    else
        print *, -1
    end if

end program abc387d
