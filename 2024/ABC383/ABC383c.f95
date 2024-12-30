program abc383c
    ! H, W              : オフィスが H 行 W 列のマス目で表される
    ! D                 : 加湿器からの最大移動回数（上下左右のみ、壁通過不可）
    ! S(i,j)            : マス (i,j) の状態を表す文字( '#' → 壁  '.' → 床   'H' → 加湿器が置かれた床 )
    ! visited(i,j)      : BFS探索時に、すでに訪れたマスかどうかを示す論理値
    ! distance(i,j)     : 加湿器マスからマス (i,j) までの移動回数(上下左右経由、壁は通れない)
    ! queue_x, queue_y  : BFS用のキューに格納するマスの座標
    ! head, tail        : BFSキューの先頭・末尾インデックス
    ! x, y              : 現在探索中のマスの座標
    ! nx, ny            : 次に探索する隣接マスの座標
    ! dx, dy            : 隣接方向への移動量(dx,dy)
    ! kasitu_count      : 加湿されている床マスの数
    implicit none
    integer :: i, j, H, W, D, kasitu_count, k
    integer, allocatable :: queue_x(:), queue_y(:), distance(:, :)
    character, allocatable :: S(:, :)
    logical, allocatable :: visited(:, :)
    integer :: head, tail, x, y, nx, ny
    integer, dimension(4) :: dx = [1, -1, 0, 0], dy = [0, 0, 1, -1]

    ! 入力
    read (*, *) H, W, D
    allocate (S(H, W), visited(H, W), distance(H, W))
    allocate (queue_x(H*W), queue_y(H*W))
    do i = 1, H
        do j = 1, W
            read (*, '(A1)', advance='no') S(i, j)
        end do
        read (*, *)
    end do

    ! 初期化
    visited = .false.
    distance = -1
    head = 1; tail = 1
    kasitu_count = 0

    ! 加湿器('H')が置かれたマスをキューに入れる
    do i = 1, H
        do j = 1, W
            if (S(i, j) == 'H') then
                queue_x(tail) = i
                queue_y(tail) = j
                tail = tail + 1
                visited(i, j) = .true.
                distance(i, j) = 0
                kasitu_count = kasitu_count + 1
            end if
        end do
    end do

    ! BFS
    do while (head < tail)
        x = queue_x(head)
        y = queue_y(head)
        head = head + 1
        ! 4方向に探索（上下左右）
        do k = 1, 4
            nx = x + dx(k)
            ny = y + dy(k)
            ! 範囲チェック
            if (nx >= 1 .and. nx <= H .and. ny >= 1 .and. ny <= W) then
                ! 未訪問、壁でなく、かつ距離がD未満なら移動可能
                if (.not. visited(nx, ny) .and. S(nx, ny) /= '#' .and. distance(x, y) < D) then
                    visited(nx, ny) = .true.
                    distance(nx, ny) = distance(x, y) + 1
                    queue_x(tail) = nx
                    queue_y(tail) = ny
                    tail = tail + 1
                    kasitu_count = kasitu_count + 1
                end if
            end if
        end do
    end do

    ! 結果の出力
    write (*, *) kasitu_count
end program abc383c
