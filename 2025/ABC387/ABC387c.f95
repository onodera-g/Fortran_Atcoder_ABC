program abc387c
    ! L    : 区間の下限 (10 ≤ L ≤ 10^18)
    ! R    : 区間の上限 (L ≤ R ≤ 10^18)
    ! cntL : L−1 以下のヘビ数の個数
    ! cntR : R 以下のヘビ数の個数
    ! ans  : 区間 [L,R] に含まれるヘビ数の個数
    implicit none
    integer(kind=8) :: L, R
    integer(kind=8) :: cntL, cntR, ans
    character(len=20) :: sR, sLm1
    integer(kind=8)   :: Lm1

    ! 入力
    read (*, *) L, R

    ! R 以下のヘビ数をカウント
    write (sR, '(I0)') R
    cntR = count_snake(sR)

    ! (L−1) 以下のヘビ数をカウント
    if (L <= 10) then
        cntL = 0
    else
        Lm1 = L - 1
        write (sLm1, '(I0)') Lm1
        cntL = count_snake(sLm1)
    end if

    ! 結果出力
    ans = cntR - cntL
    print *, ans

contains

    function count_snake(s) result(cnt)
        ! s    : チェック対象の整数を文字列で表したもの
        ! cnt  : s 以下のヘビ数の総数
        implicit none
        character(len=*), intent(in) :: s
        integer(kind=8) :: cnt
        integer :: len_s, pos
        integer(kind=8) :: digit_val, lead, maxc, ways
        logical :: tight
        integer(kind=8), dimension(20) :: digit

        ! 文字列 s を各桁の数字配列に分解
        len_s = len_trim(s)
        do pos = 1, len_s
            digit(pos) = ichar(s(pos:pos)) - ichar('0')
        end do

        cnt = 0

        ! 桁数が 2～len_s-1 のヘビ数を事前計算
        do pos = 2, len_s - 1
            do lead = 1, 9
                cnt = cnt + lead**(pos - 1)
            end do
        end do

        ! 桁数 len_s のヘビ数：先頭桁を 1～digit(1)-1 に固定
        do lead = 1, digit(1) - 1
            cnt = cnt + lead**(len_s - 1)
        end do

        ! 桁数 len_s のヘビ数：先頭桁 = digit(1) の場合を枝刈りで数え上げ
        tight = .true.
        do pos = 2, len_s
            if (.not. tight) exit
            digit_val = digit(pos)
            maxc = min(digit_val - 1, digit(1) - 1)
            if (maxc >= 0) then
                ways = digit(1)**(len_s - pos)
                cnt = cnt + (maxc + 1)*ways
            end if
            if (digit_val >= digit(1)) then
                tight = .false.
                exit
            end if
        end do

        ! 完全一致で s 自身がヘビ数ならカウント
        if (tight .and. len_s >= 2) then
            cnt = cnt + 1
        end if
    end function count_snake

end program abc387c
