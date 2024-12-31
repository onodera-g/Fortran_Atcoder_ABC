program abc385c
    ! N     : ビルの総数
    ! H     : 各ビルの高さを格納する配列
    ! i     : ビルの開始位置を示すループ変数
    ! w     : ビルを選ぶ間隔（何個飛ばしで選ぶか）
    ! cnt   : 選ばれたビルの数をカウントする変数
    ! ans   : 選択可能なビルの最大数
    implicit none
    integer :: N, i, w, cnt
    integer, allocatable :: H(:)
    integer :: ans

    ! 入力
    read (*, *) N
    allocate (H(N))
    read (*, *) (H(i), i=1, N)

    ans = 1 ! 最低でも1つのビルは選べる

    ! 全ての開始位置 i を試す
    do i = 1, N
        ! 全ての間隔 w を試す
        do w = 1, N - i
            cnt = 1
            ! 列をできるだけ伸ばす
            do while ((i + cnt*w) <= N .and. H(i) == H(i + cnt*w))
                cnt = cnt + 1
            end do
            if (cnt > ans) then
                ans = cnt
            end if
        end do
    end do

    ! 結果の出力
    print *, ans
end program abc385c
