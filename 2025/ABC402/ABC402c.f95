program ABC402c
    ! N, M            : 食材数および料理数
    ! i, j, k         : ループ用汎用変数
    ! sumK            : 全食材登場ノード数の総和
    ! ing, dish       : 食材番号, 料理番号
    ! N+1 まで確保した startIndex : 各食材の隣接リスト開始位置（番兵付き）
    ! sumK+1 まで確保した adjList : 各食材ごとに含む料理番号を連続格納する一次元配列
    ! dishTempCount   : 各料理に残っている苦手食材数のカウンタ
    ! countPerIng     : 構築時の各食材のリスト挿入オフセット（初期化用再利用）
    ! tempA           : 1行分の食材リスト読み込み用一時配列
    ! B(N)            : 日ごとに克服する食材番号
    ! edible_count    : i 日目終了時点で食べられる料理の累計数

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    integer(int32) :: N, M, i, j, k, sumK, ing, dish
    integer(int32) :: edible_count
    integer(int32), allocatable :: B(:)
    integer(int32), allocatable :: dishTempCount(:)
    integer(int32), allocatable :: countPerIng(:), startIndex(:), adjList(:)
    integer(int32), allocatable :: tempA(:)

    ! 入力
    read (*, *) N, M
    allocate (dishTempCount(M))
    allocate (countPerIng(N))
    allocate (tempA(N))
    countPerIng = 0
    sumK = 0

    ! 料理ごとに食材リストを読みつつ集計
    do i = 1, M
        read (*, *) k, (tempA(j), j=1, k)
        dishTempCount(i) = k
        do j = 1, k
            ing = tempA(j)
            countPerIng(ing) = countPerIng(ing) + 1
        end do
        sumK = sumK + k
    end do

    ! 隣接リスト用配列を確保 (sumK 要素) と番兵付き開始位置 (N+1 要素)
    allocate (adjList(sumK))
    allocate (startIndex(N + 1))

    ! 番兵付き位置テーブル構築
    startIndex(1) = 1
    do i = 1, N
        startIndex(i + 1) = startIndex(i) + countPerIng(i)
    end do

    ! カウンタ再利用: 挿入オフセットを 0 に
    countPerIng = 0

    ! 隣接リストに料理番号を格納
    rewind (unit=5)
    read (*, *) N, M ! 入力を先頭へ戻し再度読み飛ばす
    do i = 1, M
        read (*, *) k, (tempA(j), j=1, k)
        do j = 1, k
            ing = tempA(j)
            dish = i
            adjList(startIndex(ing) + countPerIng(ing)) = dish
            countPerIng(ing) = countPerIng(ing) + 1
        end do
    end do

    ! B配列を一括読み込み
    allocate (B(N))
    read (*, *) (B(i), i=1, N)

    ! i 日目終了時の食べられる料理累計を出力
    edible_count = 0
    do i = 1, N
        ing = B(i)
        do j = startIndex(ing), startIndex(ing + 1) - 1
            dishTempCount(adjList(j)) = dishTempCount(adjList(j)) - 1
            if (dishTempCount(adjList(j)) == 0) then
                edible_count = edible_count + 1
            end if
        end do
        write (*, '(I0)') edible_count
    end do

end program ABC402c
