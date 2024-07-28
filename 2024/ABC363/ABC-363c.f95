program abc363c
    !N        ：文字列の長さ
    !S        ：文字列S
    !S_num    ：文字列Sを数字に変換
    !K        ：回文の長さ
    !cnt      ：並び替えた文字列に含まれる回文の長さ
    !ans      ：長さ K の回文を部分文字列として含まないものの個数
    !next_flag：次の文字列の組み合わせの管理用(falseなら終了)
    implicit none
    integer N, K, i, cnt, ans
    logical next_flag
    character(:), allocatable :: S
    integer, allocatable::S_num(:)

    !入力
    read (*, *) N, K
    allocate (character(N) :: S)
    allocate (S_num(N))
    read (*, *) S(:)

    !数字に変換
    do i = 1, N
        S_num(i) = ichar(S(i:i))
    end do

    !ソート
    call margesort(S_num, n)

    !回文判定
    next_flag = .true.
    ans = 0; 
    call cal_kaibun(S_num, N, k, mod(k, 2), cnt)
    if (cnt /= k) ans = ans + 1
    do !next_permutationを使って次の組み合わせを作る
        call next_permutation(S_num, N, next_flag)
        if (next_flag .eqv. .false.) exit
        call cal_kaibun(S_num, N, k, mod(k, 2), cnt)
        if (cnt /= k) ans = ans + 1
    end do

    !結果の出力
    write (*, *) ans
end program abc363c

subroutine margesort(x, n)
    integer N
    integer x(N), tmp(N)
    integer start, end
    start = 1; end = N
    call loop_margesort(x, tmp, N, start, end)
end subroutine
recursive subroutine loop_margesort(x, tmp, N, left, right)
    integer left, right, mid
    integer N
    integer x(N), tmp(N)
    integer i, j, k

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

subroutine next_permutation(arr, n, next_flag)
    integer arr(n), n
    logical next_flag
    integer kari, tmp(n)
    integer i, j, k

    !これ以上組み合わせが作れるか
    do i = n - 1, 1, -1
        if (arr(i) < arr(i + 1)) exit
        if (i == 1) then
            next_flag = .false.
            return
        end if
    end do

    !組み合わせ生成
    do j = n, i + 1, -1
        if (arr(i) < arr(j)) exit
    end do
    kari = arr(i)
    arr(i) = arr(j)
    arr(j) = kari
    if (i + 1 /= n) then
        do k = 1, i
            tmp(k) = arr(k)
        end do
        do k = 0, n - (i + 1)
            tmp(n - k) = arr(i + 1 + k)
        end do
        arr = tmp
    end if

end subroutine

subroutine cal_kaibun(arr, n, k, search_case, cnt)
    implicit none
    integer n, arr(n), k, search_case
    integer cnt, left, right
    integer i

    !回文判定
    select case (search_case)
    case (0) !偶数範囲
        do i = 1, n - 1
            cnt = 0; left = i; right = i + 1
            if (left <= 0 .or. right > N) return
            call search_kaibun(arr, n, k, left, right, cnt)
            if (cnt >= k) then
                return
            end if
        end do
    case (1) !奇数範囲
        do i = 1, n - 1
            cnt = 1; left = i; right = i + 2
            if (left <= 0 .or. right > N) return
            call search_kaibun(arr, n, k, left, right, cnt)
            if (cnt >= k) then
                return
            end if
        end do
    end select
end subroutine cal_kaibun

subroutine search_kaibun(arr, n, k, left, right, cnt)
    integer arr(n), n, k, left, right, cnt
    !回文長さの測定
    do
        !文字列の範囲内か判定
        if (left <= 0 .or. right > N .or. cnt >= k) return
        !回文判定
        if (arr(left) == arr(right)) then
            cnt = cnt + 2
        else
            return
        end if
        left = left - 1
        right = right + 1
    end do
end subroutine
