program abc373d
    ! N              : グラフの頂点数
    ! M              : グラフの辺数
    ! u, v           : 各辺の始点 (u) と終点 (v)
    ! w              : 各辺の重み
    ! j              : 隣接リスト内のインデックス
    ! x              : 各頂点に割り当てられる整数値の配列
    ! queue          : BFS（幅優先探索）で使用するキュー
    ! adj_list_start : 各頂点の隣接リストの開始インデックスを示す配列
    ! adj_list_edges : 隣接リストにおける接続先頂点の配列
    ! adj_list_weights : 隣接リストにおける辺の重みの配列
    ! degree         : 各頂点の次数（隣接する辺の数）
    ! edge_count     : 隣接リスト内の総辺数（双方向の場合は2M）
    ! u_array, v_array : 各辺の始点と終点を保持する配列
    ! w_array         : 各辺の重みを保持する配列
    ! front, rear     : BFS用キューの前端と後端を管理する変数
    implicit none
    integer(16), parameter :: UNVISITED = -1000000000000000001_8 !
    integer(16) N, M, i, u, v, w, j, edge_count, front, rear
    integer(16), allocatable ::x(:), queue(:), degree(:)
    integer(16), allocatable :: adj_list_start(:), adj_list_edges(:), adj_list_weights(:)
    integer(16), allocatable :: u_array(:), v_array(:), w_array(:)

    ! 入力
    read (*, *) N, M
    allocate (x(N), queue(N), degree(N))
    allocate (adj_list_start(N + 1))
    allocate (u_array(M), v_array(M), w_array(M))
    degree = 0
    do i = 1, M
        read (*, *) u_array(i), v_array(i), w_array(i)
        degree(u_array(i)) = degree(u_array(i)) + 1
        degree(v_array(i)) = degree(v_array(i)) + 1
    end do

    ! 隣接リストの開始位置を計算
    adj_list_start(1) = 1
    do i = 1, N
        adj_list_start(i + 1) = adj_list_start(i) + degree(i)
    end do
    edge_count = adj_list_start(N + 1) - 1

    ! 隣接リストを構築
    allocate (adj_list_edges(edge_count)) ! 隣接リストの配列を動的確保
    allocate (adj_list_weights(edge_count))
    degree = 0
    do i = 1, M
        u = u_array(i)
        v = v_array(i)
        w = w_array(i)
        ! 頂点uの隣接リストに頂点vと重みwを追加
        degree(u) = degree(u) + 1
        j = adj_list_start(u) + degree(u) - 1
        adj_list_edges(j) = v
        adj_list_weights(j) = w
        ! 頂点vの隣接リストに頂点uと重み-wを追加（逆向きの辺）
        degree(v) = degree(v) + 1
        j = adj_list_start(v) + degree(v) - 1
        adj_list_edges(j) = u
        adj_list_weights(j) = -w
    end do

    ! BFSを用いて各頂点に値を割り当て
    front = 1; rear = 0; x = UNVISITED
    do i = 1, N
        if (x(i) == UNVISITED) then
            call bfs(i, x, adj_list_start, adj_list_edges, adj_list_weights, queue, front, rear)
        end if
    end do

    ! 結果の出力
    do i = 1, N
        if (i > 1) then
            write (*, '(A)', advance='NO') ' '
        end if
        write (*, '(I0)', advance='NO') x(i)
    end do

contains
    !---------------------------------------
    ! BFS（幅優先探索）を実行するサブルーチン
    !---------------------------------------
    subroutine bfs(start_vertex, x, adj_start, adj_edges, adj_weights, q, front, rear)
        implicit none
        integer(16), intent(in)         :: start_vertex ! BFSの開始頂点（1ベース）
        integer(16), intent(inout) :: x(:) ! 各頂点の値
        integer(16), intent(in)         :: adj_start(:) ! 隣接リストの開始インデックス
        integer(16), intent(in)         :: adj_edges(:) ! 隣接リストの接続先頂点
        integer(16), intent(in)    :: adj_weights(:) ! 隣接リストの辺の重み
        integer(16), intent(inout)      :: q(:) ! BFS用のキュー
        integer(16), intent(inout)      :: front, rear ! キューの前端と後端
        integer(16)            :: current_vertex, neighbor, index
        integer(16)        :: current_value, neighbor_value

        ! 開始頂点をキューに追加
        rear = rear + 1
        q(rear) = start_vertex
        x(start_vertex) = 0_8 ! 開始頂点の値を0に設定

        ! キューが空になるまでループ
        do while (front <= rear)
            ! キューから現在の頂点を取得
            current_vertex = q(front)
            front = front + 1
            current_value = x(current_vertex)
            ! 現在の頂点の隣接リストを走査
            do index = adj_start(current_vertex), adj_start(current_vertex + 1) - 1
                neighbor = adj_edges(index)
                neighbor_value = adj_weights(index)
                ! 隣接頂点が未訪問の場合
                if (x(neighbor) == UNVISITED) then
                    x(neighbor) = current_value + neighbor_value
                    rear = rear + 1
                    q(rear) = neighbor
                end if
            end do
        end do
    end subroutine bfs

end program abc373d
