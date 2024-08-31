program abc348b
    !N：点の数
    !X：各点のX座標
    !Y：各点のY座標
    !max_num：対象の点から距離が最大になる点番号
    !max_dis：対象の点から最大の距離
    !dis：点iから点jまでの距離
    !x1,y1：点iのx,y座標
    !x2,y2；点jのx,y座標
    implicit none
    integer i, j
    integer N, max_num
    real dis, x1, x2, y1, y2, max_dis
    integer, allocatable::X(:), Y(:)

    !入力
    read (*, *) N
    allocate (X(N), Y(N))
    do i = 1, N
        read (*, *) X(i), Y(i)
    end do

    !距離の計算
    do i = 1, N
        max_dis = 0
        do j = 1, N
            x1 = X(i); x2 = X(j); y1 = Y(i); y2 = Y(j)
            dis = sqrt((x1 - x2)**2 + (y1 - y2)**2)
            if (max_dis < dis) then
                max_dis = dis
                max_num = j
            end if
        end do
        write (*, *) max_num!結果の出力
    end do
end program abc348b
