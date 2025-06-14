program ABC410c
    ! N: 配列の長さ
    ! Q: クエリの数
    ! off: 回転オフセット（累積）
    ! line: 入力クエリ行の一時格納
    ! c: クエリ種別, p: 位置引数, x: 値引数
    ! k: 回転量（タイプ3クエリ）
    ! pos: 実際にアクセスする配列インデックス
    ! A: 1-indexedの配列

    use, intrinsic :: iso_fortran_env, only: int32, int64
    implicit none
    integer(int32) :: N, Q, i
    integer(int64) :: off
    character(len=256) :: line
    integer(int32) :: c, p, x
    integer(int64) :: k
    integer(int32) :: pos
    integer(int32), allocatable :: A(:)

    ! 入力
    read (*, *) N, Q
    allocate (A(N))
    do i = 1, N
        A(i) = i
    end do
    off = 0_int64

    ! クエリ処理
    do i = 1, Q
        read (*, '(A)') line
        ! クエリ種別判定のため先頭整数を取得
        read (line, *) c
        select case (c)
        case (1)
            ! タイプ1
            read (line, *) c, p, x
            pos = idx(p, off, N)
            A(pos) = x

        case (2)
            ! タイプ2
            read (line, *) c, p
            pos = idx(p, off, N)
            write (*, *) A(pos)

        case (3)
            ! タイプ3
            read (line, *) c, k
            off = mod(off + k, int(N, kind=int64))

        end select
    end do

contains
    ! p: 1-origin位置, N: 配列サイズ
    ! idx: 仮想配列位置pを実オフセット付き位置に変換
    ! off: 現在のオフセット累積
    ! tmp: 計算用一時, Ni: Nの64bit版
    pure integer(int32) function idx(p, off, N) result(pidx)
        integer(int32), intent(in) :: p, N
        integer(int64), intent(in) :: off
        integer(int64) :: tmp, Ni
        Ni = int(N, kind=int64)
        tmp = mod(off + int(p - 1, kind=int64), Ni)
        pidx = int(tmp, kind=int32) + 1
    end function idx
end program ABC410c
