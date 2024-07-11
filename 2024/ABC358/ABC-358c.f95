program abc358c
    !N：ポップコーン売り場の数
    !M：ポップコーンの味の種類
    !S：売り場に売っているポップコーンの種類
    !bit_pop：売り場にあるポップコーンの種類を２ビットで管理
    !bit_ans：すべての味のポップコーンを購入するために必要な最低何個の売り場
    !cnt：全探索の制御用
    !ans：購入できているポップコーンを２ビットで表現
    !tmp：全部のポップコーンが購入できた状態を２ビットで表現
    implicit none
    integer(16) N, M, i, j
    character(1), allocatable :: S(:, :)
    integer(16), allocatable::bit_pop(:)
    integer(16) bit_ans, cnt, ans, tmp

    !入力
    read (*, *) N, M
    allocate (S(N, M))
    do i = 1, N
        read (*, '(*(a1))') (S(i, j), j=M, 1, -1)
    end do
    allocate (bit_pop(N))
    bit_pop = 0
    bit_ans = 0
    cnt = 0
    ans = 9999
    tmp = 0

    !売っているポップコの種類をビットに変換
    do i = 1, N
        do j = 1, M
            if (S(i, j) == "o") then
                bit_pop(i) = ibset(bit_pop(i), j - 1)
            end if
        end do
    end do

    do j = 0, M - 1
        tmp = ibset(tmp, j)
    end do

    !全探索
    do i = 0, 2**N - 1
        cnt = 0; bit_ans = 0
        do j = 1, N
            if (btest(i, j - 1) .eqv. .true.) then
                bit_ans = ior(bit_ans, bit_pop(j))
                cnt = cnt + 1
            end if
        end do
        if (popcnt(bit_ans) == popcnt(tmp)) then
            ans = min(ans, cnt)
        end if
    end do

    !結果の出力
    write (*, *) ans
end
