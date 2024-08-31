program abc368b
    !N  ：正整数列Aの長さ
    !A  ：正整数列A
    !cnt：正の要素の中での１以下の個数
    !ans：正整数列Aを操作した回数
    implicit none
    integer(16) i
    integer(16) N, cnt, ans
    integer(16), allocatable::A(:)

    !入力
    read (*, *) N
    allocate (A(N))
    read (*, *) A

    !出力
    ans = 0
    do
        ans = ans + 1
        call margesort(A, N)
        A(1) = A(1) - 1
        A(2) = A(2) - 1
        cnt = 0
        do i = 1, N
            if (A(i) > 0) cnt = cnt + 1
        end do
        if (cnt <= 1) exit
    end do

    !結果の出力
    print *, ans

contains
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

        !これ以上2分かつできないならretrun
        if (left >= right) return

        !分割できるだけ分割する
        mid = (left + right)/2
        call loop_margesort(x, tmp, N, left, mid)
        call loop_margesort(x, tmp, N, mid + 1, right)

        !並び替えの下準備としてtmpに配列をコピー
        j = 0
        tmp(left:mid) = x(left:mid)
        do i = mid + 1, right
            tmp(i) = x(right - j)
            j = j + 1
        end do

        !大小比較して小さい順に入れていく
        i = left
        j = right
        do k = left, right
            if (tmp(i) > tmp(j)) then !
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
end program abc368b

