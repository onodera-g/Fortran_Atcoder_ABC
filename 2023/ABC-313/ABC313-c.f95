program ABC313c
    !N：整数列の長さ
    !A：整数列A
    !X：最終的な整数列の値
    !r：最終的な整数列の値の端数
    !B：操作後の最終的な整数列B
    implicit none
    integer(16) N, X, r
    integer(16), allocatable::A(:), B(:)

    !入力
    read (*, *) N
    allocate (A(N), B(N))
    read (*, *) A

    !操作回数の計算
    X = sum(A)/N
    r = mod(sum(A), N)
    B(1:N - r) = X
    B(N - r + 1:N) = X + 1
    call margesort(A, N)

    !結果の出力
    if (N == 1) then
        print *, 0
    else
        print *, sum(abs(B - A))/2
    end if
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
            if (tmp(i) < tmp(j)) then !
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
end program ABC313c
