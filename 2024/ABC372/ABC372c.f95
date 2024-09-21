program abc372c
    ! N           : 文字列の長さ
    ! Q           : クエリの数
    ! S           : 文字列 S を1文字ずつ格納する配列
    ! i           : ループカウンタ
    ! X           : クエリの位置
    ! C           : 置き換える文字
    ! ans         : 「ABC」部分文字列の総数
    ! input_string: 入力される文字列を一時的に格納
    ! idx         : 部分文字列の開始位置
    implicit none
    integer :: N, Q
    character(len=1), allocatable :: S(:)
    integer :: i, X
    character(len=1) :: C
    integer :: ans
    character(len=1000000) :: input_string
    integer :: idx

    ! 入力
    read (*, *) N, Q ! 文字列の長さ N とクエリの数 Q を読み込む
    allocate (S(N))
    read (*, '(A)') input_string ! 文字列 S を一時的な文字列に読み込む
    do i = 1, N
        S(i) = input_string(i:i) ! 1文字ずつ配列 S に格納する
    end do

    ! 初期の「ABC」部分文字列のカウント
    ans = 0
    do i = 1, N - 2
        if (S(i) == 'A' .and. S(i + 1) == 'B' .and. S(i + 2) == 'C') then
            ans = ans + 1
        end if
    end do

    ! 各クエリの処理
    do i = 1, Q
        ! クエリの読み込み: 位置 X と置き換える文字 C
        read (*, *) X, C
        ! 変更前に影響を受ける可能性のある部分文字列の開始位置を特定
        ! 変更位置が部分文字列のどの位置にあるかに応じて最大3つの部分文字列が影響を受ける
        do idx = max(1, X - 2), min(X, N - 2)
            ! 変更前に「ABC」だった場合はカウントを減少
            if (S(idx) == 'A' .and. S(idx + 1) == 'B' .and. S(idx + 2) == 'C') then
                ans = ans - 1
            end if
        end do
        ! 文字列 S の X 番目の文字を C に変更
        S(X) = C
        ! 変更後に影響を受ける部分文字列のカウント
        do idx = max(1, X - 2), min(X, N - 2)
            ! 変更後に「ABC」になった場合はカウントを増加
            if (S(idx) == 'A' .and. S(idx + 1) == 'B' .and. S(idx + 2) == 'C') then
                ans = ans + 1
            end if
        end do

        ! 結果の出力
        print *, ans
    end do
end program abc372c
