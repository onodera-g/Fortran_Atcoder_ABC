program abc386c
    ! K    : 最大操作回数（今回の問題では K=1）
    ! S    : 初期文字列
    ! T    : 目標文字列
    ! sl   : 文字列 S の有効な長さ
    ! tl   : 文字列 T の有効な長さ
    ! pc   : 先頭から一致する部分のカウント
    ! sc   : 末尾から一致する部分のカウント
    ! i    : ループ用インデックス
    ! diff : 変更（置換）が必要な文字の数
    implicit none
    integer K
    character(500001) :: S, T
    integer sl, tl
    integer pc, sc
    integer i, diff

    ! 入力
    read (*, *) K
    read (*, *) S
    read (*, *) T
    sl = len_trim(S)
    tl = len_trim(T)

    ! SとTが一致できるかを鑑賞
    if (sl == tl .and. S(1:sl) == T(1:tl)) then ! SとTが完全に同じ場合
        print *, 'Yes'
        stop
    end if

    if (sl == tl) then ! SとTで同じ文字数の場合
        ! 変更（置換）の場合
        diff = 0
        do i = 1, sl
            if (S(i:i) /= T(i:i)) then
                diff = diff + 1
                if (diff > 1) exit
            end if
        end do
        if (diff <= 1) then
            print *, 'Yes'
        else
            print *, 'No'
        end if

    else if (sl + 1 == tl) then ! SがTより１文字少ない場合
        ! 挿入の場合
        pc = 0
        sc = 0

        ! 先頭から一致する部分をカウント
        do while (pc < sl .and. S(pc + 1:pc + 1) == T(pc + 1:pc + 1))
            pc = pc + 1
        end do
        ! 末尾から一致する部分をカウント
        do while (sc < sl .and. S(sl - sc:sl - sc) == T(tl - sc:tl - sc))
            sc = sc + 1
        end do
        if (pc + sc >= sl) then
            print *, 'Yes'
        else
            print *, 'No'
        end if

    else if (sl - 1 == tl) then ! TがSより１文字少ない場合
        ! 削除の場合
        pc = 0
        sc = 0
        ! 先頭から一致する部分をカウント
        do while (pc < tl .and. S(pc + 1:pc + 1) == T(pc + 1:pc + 1))
            pc = pc + 1
        end do

        ! 末尾から一致する部分をカウント
        do while (sc < tl .and. S(sl - sc:sl - sc) == T(tl - sc:tl - sc))
            sc = sc + 1
        end do

        if (pc + sc >= tl) then
            print *, 'Yes'
        else
            print *, 'No'
        end if

    else
        ! 長さの差が1以外の場合
        print *, 'No'
    end if

end program abc386c
