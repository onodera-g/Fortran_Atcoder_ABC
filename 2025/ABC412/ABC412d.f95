program ABC412d
    ! N                : 頂点の数。最大で8。
    ! M                : 現在のグラフに含まれる辺の数。
    ! initial_mask     : 入力された辺の情報をビット列で表現したもの。
    ! total_edges      : N個の頂点間に存在し得る全ての辺の数（N*(N-1)/2）。
    ! ans              : 編集距離の最小値（初期値は最大値）。
    ! edge_id(8, 8)    : 辺 (i, j) に対応するビット位置を保存する配列。
    ! i, j             : 汎用のループ変数。
    ! a, b             : 頂点の小さい方と大きい方を整理したときの変数。
    ! bit              : 現在の辺に対応するビットの位置。
    ! u, v             : 入力で与えられる辺の両端の頂点。
    ! perm(8)          : 頂点の置換を表現する配列。
    ! used(8)          : 使用済みの頂点かどうかを示す論理値配列。
    ! seen_masks(:)    : 既に処理済みのマスクを保存する可変長配列。
    ! nseen            : 現在までに記録されたマスクの数。
    ! diff             : 初期状態と比較した変更ビット数（XOR後のpopcount）。

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    integer :: N, M
    integer(int32) :: initial_mask
    integer :: total_edges
    integer :: ans
    integer :: edge_id(8, 8)
    integer :: i, j, a, b, bit
    integer :: u, v
    integer :: perm(8)
    logical :: used(8)
    integer(int32), allocatable :: seen_masks(:)
    integer :: nseen
    integer :: diff

    ! 入力読み込み
    read (*, *) N, M

    ! 辺に対応するビット番号を付与
    total_edges = 0
    do i = 1, N - 1
        do j = i + 1, N
            total_edges = total_edges + 1
            edge_id(i, j) = total_edges
        end do
    end do

    ! 入力された辺をマスクで表現
    initial_mask = 0_int32
    do i = 1, M
        read (*, *) u, v
        if (u < v) then
            a = u; b = v
        else
            a = v; b = u
        end if
        bit = edge_id(a, b) - 1
        initial_mask = ior(initial_mask, ishft(1_int32, bit))
    end do

    ! 探索初期化
    allocate (seen_masks(1))
    nseen = 0
    ans = total_edges*2 ! 最悪ケースの差分数で初期化
    call backtrack(1) ! 頂点の置換を全探索開始

    ! 結果出力
    print *, ans

contains

    ! 全ての頂点の置換を生成するバックトラック関数
    recursive subroutine backtrack(pos)
        integer, intent(in) :: pos
        integer :: i

        if (pos > N) then
            call process_perm()
        else
            do i = 1, N
                if (.not. used(i)) then
                    perm(pos) = i
                    used(i) = .true.
                    call backtrack(pos + 1)
                    used(i) = .false.
                end if
            end do
        end if
    end subroutine backtrack

    ! 生成した置換に対して2-regularグラフを構築し、差分を計算
    subroutine process_perm()
        logical :: visited(8)
        integer(int32) :: mask, mm
        integer :: k, v0, v, w, len, pidx
        integer(int32) :: xored

        mask = 0_int32
        visited = .false.

        ! サイクルをたどって辺の集合（マスク）を作成
        do k = 1, N
            if (.not. visited(k)) then
                len = 0
                v = k
                do while (.not. visited(v))
                    visited(v) = .true.
                    len = len + 1
                    v = perm(v)
                end do
                if (len < 3) return ! サイクルの長さが2以下なら不正
                v0 = k
                v = v0
                do
                    w = perm(v)
                    if (v < w) then
                        a = v; b = w
                    else
                        a = w; b = v
                    end if
                    pidx = edge_id(a, b) - 1
                    mask = ior(mask, ishft(1_int32, pidx))
                    v = w
                    if (v == v0) exit
                end do
            end if
        end do

        ! 既に同じ構成を見たことがあるかチェック
        do mm = 1, nseen
            if (seen_masks(mm) == mask) return
        end do

        ! 新しい構成として記録
        nseen = nseen + 1
        if (nseen > size(seen_masks)) then
            call extend_seen()
        end if
        seen_masks(nseen) = mask

        ! XORで差分ビットを取得し、popcountで編集距離を算出
        xored = ieor(initial_mask, mask)
        diff = popcount(xored)
        if (diff < ans) ans = diff
    end subroutine process_perm

    ! マスク記録用配列の拡張処理
    subroutine extend_seen()
        integer(int32), allocatable :: tmp(:)
        allocate (tmp(size(seen_masks)*2))
        tmp(1:nseen - 1) = seen_masks(1:nseen - 1)
        deallocate (seen_masks)
        allocate (seen_masks(size(tmp)))
        seen_masks = tmp
        deallocate (tmp)
    end subroutine extend_seen

    ! ビットが立っている個数（popcount）を求める関数
    pure integer function popcount(x) result(c)
        integer(int32), intent(in) :: x
        integer(int32) :: y
        c = 0
        y = x
        do while (y /= 0_int32)
            y = iand(y, y - 1_int32)
            c = c + 1
        end do
    end function popcount

end program ABC412d
