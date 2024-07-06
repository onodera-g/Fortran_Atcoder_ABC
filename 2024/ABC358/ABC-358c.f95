program test
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

    write (*, *) ans
end
