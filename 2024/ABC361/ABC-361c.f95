program abc361c
    !N数列Aの長さN
    !A：長さNの数列
    !K：消したい要素の数
    !diff：(B の最大値)−(B の最小値)
    !ans：diffの最小値
    implicit none
    integer(16) N, K, i
    integer(16), allocatable::A(:)
    integer(16) diff, ans

    !入力
    read (*, *) N, K
    allocate (A(N))
    read (*, *) A(:)

    !最小値の計算
    ans = 1000000000
    call margesort(A, N)
    do i = 1, K + 1
        diff = A(i + N - K - 1) - A(i)
        ans = min(ans, diff)
    end do

    !結果の出力
    write (*, *) ans

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
        !write (*, '(3x,*(f13.101x),a)', advance='no') x(left:right)
        !write (*, '(a)', advance='no') '>>'
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
        !write (*, '(3x,*(f13.10,1x))') x(left:right)
    end subroutine loop_margesort
end program abc361c
