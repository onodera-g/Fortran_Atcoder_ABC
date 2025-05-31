program ABC393c
    ! N            : 頂点数
    ! M            : 辺の本数
    ! loops        : 自己ループの数 (u == v)
    ! nonloop      : 自己ループを除いた辺の数
    ! u_arr(1:nonloop), v_arr(1:nonloop)
    !               : 各辺 (u,v) を u<v の形で格納する配列
    ! cnt_v(1:N)   : “v をキーとするカウントソート” 用の補助配列
    ! cnt_u(1:N)   : “u をキーとするカウントソート” 用の補助配列
    ! tmp_u(1:nonloop), tmp_v(1:nonloop)
    !               : ソートしたあと一時的に使う配列
    ! duplicates   : 重複辺 (multi‐edge) の個数
    ! key_prev, key_curr : (u,v) を一意に比較する 64 ビットキー
    ! i, u, v, pos : ループ用変数およびソート位置
    use, intrinsic :: iso_fortran_env, only: int32, int64
    implicit none
    integer(int32) :: N, M
    integer(int32) :: loops, nonloop
    integer(int32) :: i, u, v, pos, duplicates
    integer(int64) :: key_prev, key_curr
    integer(int32), allocatable :: u_arr(:), v_arr(:)
    integer(int32), allocatable :: cnt_v(:), cnt_u(:)
    integer(int32), allocatable :: tmp_u(:), tmp_v(:)

    ! 入力
    read (*, *) N, M
    allocate (u_arr(M), v_arr(M))
    loops = 0
    nonloop = 0
    do i = 1, M
        read (*, *) u, v
        if (u == v) then
            ! 自己ループなら除去候補にカウント
            loops = loops + 1
        else
            ! 無向辺なので (u<v) の形にして格納
            if (u < v) then
                nonloop = nonloop + 1
                u_arr(nonloop) = u
                v_arr(nonloop) = v
            else
                nonloop = nonloop + 1
                u_arr(nonloop) = v
                v_arr(nonloop) = u
            end if
        end if
    end do

    ! カウントソートで並び替え
    if (nonloop > 0) then
        ! 第1段階：v をキーにカウントソート ――
        allocate (cnt_v(N))
        cnt_v = 0
        do i = 1, nonloop
            cnt_v(v_arr(i)) = cnt_v(v_arr(i)) + 1
        end do
        do i = 2, N
            cnt_v(i) = cnt_v(i) + cnt_v(i - 1)
        end do
        allocate (tmp_u(nonloop), tmp_v(nonloop))
        do i = nonloop, 1, -1
            pos = cnt_v(v_arr(i))
            tmp_u(pos) = u_arr(i)
            tmp_v(pos) = v_arr(i)
            cnt_v(v_arr(i)) = cnt_v(v_arr(i)) - 1
        end do
        ! コピーして u_arr, v_arr を更新
        do i = 1, nonloop
            u_arr(i) = tmp_u(i)
            v_arr(i) = tmp_v(i)
        end do
        deallocate (cnt_v)
        deallocate (tmp_u, tmp_v)
    end if

    if (nonloop > 0) then
        ! 第2段階：u をキーにカウントソート (v は安定ソートを保持) ――
        allocate (cnt_u(N))
        cnt_u = 0
        do i = 1, nonloop
            cnt_u(u_arr(i)) = cnt_u(u_arr(i)) + 1
        end do
        do i = 2, N
            cnt_u(i) = cnt_u(i) + cnt_u(i - 1)
        end do
        allocate (tmp_u(nonloop), tmp_v(nonloop))
        do i = nonloop, 1, -1
            pos = cnt_u(u_arr(i))
            tmp_u(pos) = u_arr(i)
            tmp_v(pos) = v_arr(i)
            cnt_u(u_arr(i)) = cnt_u(u_arr(i)) - 1
        end do
        ! ソート結果を反映
        do i = 1, nonloop
            u_arr(i) = tmp_u(i)
            v_arr(i) = tmp_v(i)
        end do
        deallocate (cnt_u)
        deallocate (tmp_u, tmp_v)
    end if

    ! 重複辺をカウン
    duplicates = 0
    if (nonloop > 0) then
        ! 1番目の辺をキー化して key_prev に格納
        key_prev = int(u_arr(1), int64)*int(1000000, int64) + int(v_arr(1), int64)
        do i = 2, nonloop
            key_curr = int(u_arr(i), int64)*int(1000000, int64) + int(v_arr(i), int64)
            if (key_curr == key_prev) then
                duplicates = duplicates + 1
            end if
            key_prev = key_curr
        end do
    end if

    !　結果の出力
    print *, loops + duplicates
end program ABC393C
