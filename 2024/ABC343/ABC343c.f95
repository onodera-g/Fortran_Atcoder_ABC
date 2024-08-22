program abc343c
    !N   ：正整数
    !x   ：立方根
    !mozi：立法数を文字型に変換
    !num ：立法数を1文字区切りで格納
    !nn  ：立法数の文字数
    !cnt ：立法数の回文になっている文字数
    implicit none
    integer(16) i, j, tmp
    integer(16) N, x, num(1000), nn, cnt
    character(1000) mozi

    !入力
    read (*, *) N

    !立法数の最大値を検索
    x = 1
    do
        if ((x + 1)**3 > N) exit
        x = x + 1
    end do

    !立法数に対して回文かどうかを判定
    do i = x, 1, -1
        write (mozi, '(i0)') i**3
        nn = len_trim(mozi)
        do j = 1, nn
            num(j) = ichar(mozi(j:j))
        end do
        tmp = 2; tmp = mod(nn, tmp)
        call cal_kaibun(num(1:nn), nn, tmp, cnt)
        if (cnt == nn) exit
    end do

    !結果の出力
    write (*, '(i0)') i**3
contains
    subroutine cal_kaibun(arr, n, search_case, cnt)
        implicit none
        integer(16) n, arr(n), search_case
        integer(16) cnt, left, right, tmp
        integer(16) i

        !１文字なら回文判定不要
        if (n == 1) then
            cnt = 1
            return
        end if

        !回文判定
        cnt = 0
        select case (search_case)
        case (0) !偶数範囲
            do i = 1, n - 1
                tmp = 0; left = i; right = i + 1
                if (left <= 0 .or. right > N) return
                call search_kaibun(arr, n, left, right, tmp)
                cnt = max(tmp, cnt)
            end do
        case (1) !奇数範囲
            do i = 1, n - 1
                tmp = 1; left = i; right = i + 2
                if (left <= 0 .or. right > N) return
                call search_kaibun(arr, n, left, right, tmp)
                cnt = max(tmp, cnt)
            end do
        end select
    end subroutine cal_kaibun

    subroutine search_kaibun(arr, n, left, right, cnt)
        integer(16) n, arr(n), left, right, cnt
        !回文長さの測定
        do
            !文字列の範囲内か判定
            if (left <= 0 .or. right > N) return
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
end program abc343c
