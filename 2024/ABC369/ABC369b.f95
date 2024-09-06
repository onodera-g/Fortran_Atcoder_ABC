program abc369b
    !N  ：ピアノの操作回数
    !A  ：各操作で押す鍵盤
    !S  ：各操作を右手と左手のどちらで行うか
    !L  ：左手で初めに押す鍵盤の位置
    !Ls ：左手で初めに押す鍵盤の保存場所
    !R  ：右手で初めに押す鍵盤の位置
    !Rs ：右手で初めに押す鍵盤の保存場所
    !Lh ：左手の疲労度
    !Rh ：右手の疲労度
    !cnt：疲労度の合計
    implicit none
    integer i
    integer N, L, R, Ls, Rs, Lh, Rh, cnt
    integer, allocatable::A(:)
    character(1), allocatable ::S(:)

    !入力
    read (*, *) N
    allocate (A(N), S(N))
    do i = 1, N
        read (*, *) A(i), S(i)
    end do

    !初期位置の決定
    L = 0; R = 0
    do i = 1, N
        if (L == 0 .and. S(i) == 'L') then
            L = A(i)
            Ls = i
        elseif (R == 0 .and. S(i) == 'R') then
            R = A(i)
            Rs = i
        end if
    end do

    !左手のシミュレーション
    Lh = 0
    do i = Ls + 1, N
        if (S(i) == 'L') then
            Lh = Lh + abs(A(i) - L)
            L = A(i)
        end if
    end do
    !右手のシミュレーション
    Rh = 0
    do i = Rs + 1, N
        if (S(i) == 'R') then
            Rh = Rh + abs(A(i) - R)
            R = A(i)
        end if
    end do

    !出力
    cnt = Rh + Lh
    print *, cnt

end program abc369b

