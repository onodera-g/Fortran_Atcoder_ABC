program ABC404c
    ! N       : 頂点数
    ! M       : 辺数
    ! i       : ループカウンタ
    ! deg     : 各頂点の次数
    ! parent  : Union-Find の親ポインタ配列
    ! sz      : Union-Find の各根の部分木サイズ
    ! ai, bi  : 辺の両端の頂点番号
    ! root    : 代表元 (連結性チェック用)
    ! ok      : 判定フラグ

    use iso_fortran_env, only: int32
    implicit none
    integer(int32)            :: N, M, i, root
    integer(int32), allocatable :: deg(:), parent(:), sz(:)
    integer(int32)            :: ai, bi
    logical                   :: ok

    ! 入力
    read (*, *) N, M
    allocate (deg(N), parent(N), sz(N))
    deg = 0
    do i = 1, N
        parent(i) = i
        sz(i) = 1
    end do

    ! 辺の読み込み：次数をカウントし、Union-Find で併合
    do i = 1, M
        read (*, *) ai, bi
        deg(ai) = deg(ai) + 1
        deg(bi) = deg(bi) + 1
        call unite(ai, bi, parent, sz)
    end do

    ! 全頂点の次数が 2 であるか検査
    ok = .true.
    do i = 1, N
        if (deg(i) /= 2) then
            ok = .false.
            exit
        end if
    end do
    if (.not. ok) then
        print *, "No"
        stop
    end if

    ! 連結性の検査：すべての頂点が 1 の集合に属するか
    root = find(1, parent)
    do i = 2, N
        if (find(i, parent) /= root) then
            ok = .false.
            exit
        end if
    end do

    ! 結果の出力
    if (ok) then
        print *, "Yes"
    else
        print *, "No"
    end if

contains

    ! Find with path compression
    recursive integer(int32) function find(x, parent) result(r)
        integer(int32), intent(in)    :: x
        integer(int32), intent(inout) :: parent(:)
        integer(int32)                :: p
        if (parent(x) == x) then
            r = x
        else
            p = find(parent(x), parent)
            parent(x) = p
            r = p
        end if
    end function find

    ! Union by size
    subroutine unite(a, b, parent, sz)
        integer(int32), intent(in)    :: a, b
        integer(int32), intent(inout) :: parent(:), sz(:)
        integer(int32)                :: ra, rb
        ra = find(a, parent)
        rb = find(b, parent)
        if (ra == rb) return
        if (sz(ra) < sz(rb)) then
            parent(ra) = rb
            sz(rb) = sz(rb) + sz(ra)
        else
            parent(rb) = ra
            sz(ra) = sz(ra) + sz(rb)
        end if
    end subroutine unite
end program ABC404c
