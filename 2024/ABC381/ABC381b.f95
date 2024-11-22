program abc381b
    ! N          : 入力文字列 S の長さ
    ! S          : 入力文字列 (最大長 100 文字)
    ! cnt        : 各文字 ('a' から 'z') の出現回数を記録する配列
    implicit none
    integer i, j, N, cnt(26)
    character(100) :: S

    ! 入力
    read (*, *) S
    N = len_trim(S)

    ! 条件1: 文字列の長さが偶数であるかを確認
    if (mod(N, 2) == 1) then
        print *, 'No'
        stop
    end if

    ! 条件2: 奇数番目と偶数番目の文字が等しいことを確認
    ! 奇数番目の文字: S(1), S(3), ...
    ! 偶数番目の文字: S(2), S(4), ...
    do i = 1, N/2
        if (S(2*i - 1:2*i - 1) /= S(2*i:2*i)) then
            print *, 'No'
            stop
        end if
    end do

    ! 条件3: 各文字がちょうど2回ずつ登場していることを確認
    ! cnt 配列を初期化 (0 で初期化)
    cnt = 0
    do i = 1, N
        j = ichar(S(i:i)) - ichar('a') + 1 ! 現在の文字の 'a' からの相対位置を計算
        cnt(j) = cnt(j) + 1 ! 対応する文字のカウントを増加
    end do

    ! 各文字の出現回数を確認
    do i = 1, 26
        if (cnt(i) == 2 .or. cnt(i) == 0) then
            cycle ! 条件を満たす場合は次の文字を確認
        else
            print *, 'No'
            stop
        end if
    end do

    ! 全ての条件を満たす場合
    print *, 'Yes'

end program abc381b
