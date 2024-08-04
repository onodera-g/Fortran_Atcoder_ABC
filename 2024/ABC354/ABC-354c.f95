program abc354c
    !N  ：カードの枚数
    !A  ：カードの強さ
    !C  ：カードのコスト
    !num：カードの並び順
    !Ans：捨てた後のカード構成
    !cnt：捨てた後のカード枚数
    implicit none
    integer(16) N, i, ii, cnt
    integer(16), allocatable::A(:), C(:), num(:), Ans(:)

    !入力
    read (*, *) N
    allocate (A(N), C(N), Ans(N), num(N))
    do i = 1, N
        read (*, *) A(i), C(i)
        num(i) = i
    end do
    call margesort(A, num, N)

    !弱いカードを捨てる
    Ans(1) = num(1); cnt = 1; ii = 1
    do i = 2, N
        if (C(num(ii)) > C(num(i))) then
            cnt = cnt + 1
            Ans(cnt) = num(i)
            ii = i
        end if
    end do
    call margesort(Ans, Ans, N)

    !結果の出力
    write (*, '(i0)') cnt
    do i = cnt, 1, -1
        write (*, '(i0,1x)', advance='no') Ans(i)
    end do

contains
    !
    subroutine margesort(x, y, n)
        integer(16) N
        integer(16) x(N), tmp(N)
        integer(16) y(N), tmp2(N)
        integer(16) start, end
        start = 1; end = N
        call loop_margesort(x, y, tmp, tmp2, N, start, end)
    end subroutine
    recursive subroutine loop_margesort(x, y, tmp, tmp2, N, left, right)
        integer(16) left, right, mid
        integer(16) N
        integer(16) x(N), tmp(N)
        integer(16) y(N), tmp2(N)
        integer(16) i, j, k

        !これ以上2分かつできないならretrun
        if (left >= right) return

        !分割できるだけ分割する
        mid = (left + right)/2
        call loop_margesort(x, y, tmp, tmp2, N, left, mid)
        call loop_margesort(x, y, tmp, tmp2, N, mid + 1, right)

        !並び替えの下準備としてtmpに配列をコピー
        j = 0
        tmp(left:mid) = x(left:mid)
        tmp2(left:mid) = y(left:mid)
        do i = mid + 1, right
            tmp(i) = x(right - j)
            tmp2(i) = y(right - j)
            j = j + 1
        end do

        !大小比較して小さい順に入れていく
        i = left
        j = right
        !write (*, '(3x,*(f13.101x),a)', advance='no') x(left:right)
        !write (*, '(a)', advance='no') '>>'
        do k = left, right
            if (tmp(i) > tmp(j)) then
                x(k) = tmp(i)
                y(k) = tmp2(i)
                i = i + 1
            else if (tmp(i) == tmp(j)) then
                x(k) = tmp(i)
                y(k) = tmp2(i)
                i = i + 1
            else
                x(k) = tmp(j)
                y(k) = tmp2(j)
                j = j - 1
            end if
        end do
        !write (*, '(3x,*(f13.10,1x))') x(left:right)
    end subroutine loop_margesort

end program
