program ABC399c
    ! N        : 頂点数
    ! M        : 辺数
    ! u, v     : 各辺の両端の頂点番号
    ! parent   : Union-Find の親配列
    ! rank     : Union-Find のランク配列（木の高さの概算）
    ! i        : ループ用インデックス
    ! root_u   : 頂点 u の根
    ! root_v   : 頂点 v の根
    ! K        : 連結成分の個数
    ! ans      : 削除すべき辺の本数

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    integer(int32) :: N, M
    integer(int32), allocatable :: parent(:), rank(:)
    integer(int32) :: i, u, v
    integer(int32) :: root_u, root_v
    integer(int32) :: K, ans

    ! 入力
    read (*, *) N, M
    allocate (parent(N), rank(N)) ! Union-Find 用配列を確保し、初期化
    do i = 1, N
        parent(i) = i ! 自分自身を親に
        rank (i) = 0 ! ランクはすべて0
    end do

    ! 各辺を読み込んで Union-Find に統合
    do i = 1, M
        read (*, *) u, v
        call find_root(u, root_u)
        call find_root(v, root_v)

        if (root_u /= root_v) then
            ! 異なる連結成分ならマージ（ランクで小さいほうを下に）
            if (rank(root_u) < rank(root_v)) then
                parent(root_u) = root_v
            else if (rank(root_u) > rank(root_v)) then
                parent(root_v) = root_u
            else
                parent(root_v) = root_u
                rank (root_u) = rank(root_u) + 1
            end if
        end if
    end do

    ! 連結成分の個数 K を数える
    K = 0
    do i = 1, N
        call find_root(i, root_u)
        if (root_u == i) then
            K = K + 1
        end if
    end do

    ! 削除すべき辺数 = M - (N - K)
    ans = M - (N - K)

    ! 結果の出力
    write (*, '(I0)') ans

contains

    recursive subroutine find_root(x, r)
        ! find_root: 頂点 x の根を r に返す（パス圧縮付き）
        integer(int32), intent(in)  :: x
        integer(int32), intent(out) :: r
        integer(int32) :: p

        p = parent(x)
        if (p == x) then
            r = x
        else
            call find_root(p, r)
            parent(x) = r ! パス圧縮
        end if
    end subroutine find_root

end program ABC399c
