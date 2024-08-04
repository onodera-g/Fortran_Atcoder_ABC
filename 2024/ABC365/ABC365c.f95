program abc365c
    !N    ：参加人数
    !M    ：予算の上限
    !A    ：交通費の希望金額
    !cnt  ：満額払える人数
    !start：１人あたりの予算の開始値
    !tmp  ：満額もらえる人たちの合計金額
    !tmp2 ：必要な合計金額
    implicit none
    integer(16) i, j
    integer(16) N, M, cnt, start, tmp, tmp2
    integer(16), allocatable:: A(:)

    !入力
    read (*, *) N, M
    allocate (A(N))
    read (*, *) A(:)

    !ソート
    call margesort(A, N)
    start = M/N
    cnt = 1
    tmp = 0

    !全員の交通費の合計がM以下ならどれだけ増やしてもOK
    if (sum(A) <= M) then
        write (*, '(a)') 'infinite'
        stop
    end if

    !交通費の上限の検索
    do i = start, A(N) !i=交通費の上限x
        !満額払える位置の更新
        do j = cnt, N
            if (A(j) > i) then
                cnt = j
                exit
            else
                tmp = tmp + A(j)
            end if
        end do
        !合計金額
        if (j == N + 1) then !全員が満額もらえる
            tmp2 = tmp
        else !一部の人が上限分のみ支給
            tmp2 = tmp + i*(N - cnt + 1)
        end if
        !予算超えるなら終了
        if (tmp2 > M) then
            exit
        end if
    end do

    !結果の出力
    write (*, '(i0)') i - 1

end program abc365c
subroutine margesort(x, n)
    integer(16) N
    integer(16) x(N), tmp(N)
    integer(16) start, end
    start = 1; end = N
    call loop_margesort(x, tmp, N, start, end)
end subroutine
recursive subroutine loop_margesort(x, tmp, N, left, right)
    integer(16) left, right, mid
    integer(16) N
    integer(16) x(N), tmp(N)
    integer(16) i, j, k

    if (left >= right) return

    mid = (left + right)/2
    call loop_margesort(x, tmp, N, left, mid)
    call loop_margesort(x, tmp, N, mid + 1, right)

    j = 0
    tmp(left:mid) = x(left:mid)
    do i = mid + 1, right
        tmp(i) = x(right - j)
        j = j + 1
    end do

    i = left
    j = right

    do k = left, right
        if (tmp(i) < tmp(j)) then
            x(k) = tmp(i)
            i = i + 1
        else if (tmp(i) == tmp(j)) then
            x(k) = tmp(i)
            i = i + 1
        else
            x(k) = tmp(j)
            j = j - 1
        end if
    end do
end subroutine loop_margesort

