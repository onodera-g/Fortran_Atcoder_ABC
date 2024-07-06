program abc353b
    !
    !
    !
    implicit none
    integer N, K, tmp
    integer i, j, cnt
    integer, allocatable ::A(:)

    !入力
    read (*, *) N, K
    allocate (A(N))
    read (*, *) A(:)

    !カートに積み込み作業
    tmp = 0
    cnt = 0
    i = 1
    do
        cnt = cnt + 1
        tmp = A(i)
        !write (*, *) "cnt", cnt
        !write (*, *) i, tmp
        do j = i + 1, N
            if (tmp + A(j) <= K) then
                tmp = tmp + A(j)
                !write (*, *) i, tmp
            else
                exit
            end if
        end do
        i = j
        if (i > N) exit
    end do

    !結果の出力
    write (*, *) cnt
end program abc353b
