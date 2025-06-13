program ABC397b
    ! S        : 入力される文字列 (i/o のみ)
    ! n        : S の長さ
    ! idx      : 走査用インデックス
    ! t        : 次に期待される文字 ('i' または 'o')
    ! ans      : 挿入操作の回数
    ! c        : 現在の S(idx) 文字

    implicit none
    character(len=100) :: Sraw
    character(len=:), allocatable :: S
    integer :: n, idx, ans
    character :: t, c

    ! 入力
    read (*, '(A)') Sraw
    n = len_trim(Sraw)
    S = Sraw(1:n)

    ! 初期: 次に期待される文字は 'i'
    t = 'i'
    ans = 0

    ! S を前から走査
    do idx = 1, n
        c = S(idx:idx)
        if (c == t) then
            ! 期待どおりの文字なら、次は反対
            if (t == 'i') then
                t = 'o'
            else
                t = 'i'
            end if
        else
            ! 期待外の文字なら、挿入操作
            ans = ans + 1
            ! 挿入後に期待は反転
            if (t == 'i') then
                t = 'o'
            else
                t = 'i'
            end if
            ! 今の文字 c を再チェック
            if (c == t) then
                if (t == 'i') then
                    t = 'o'
                else
                    t = 'i'
                end if
            end if
        end if
    end do

    ! 最後に長さが奇数なら 'o' を追加
    if (t == 'o') then
        ans = ans + 1
    end if

    ! 結果の出力
    print *, ans

end program ABC397b
