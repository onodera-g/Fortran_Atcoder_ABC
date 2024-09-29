program abc373b
    ! S        : キーボードの配列を表す入力文字列（'A'から'Z'の順列）
    ! alphabet : 標準のアルファベット文字列 "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    ! Sn       : 現在入力すべき文字
    ! cnt      : 指の移動距離の合計
    ! r0       : 前回のキーの位置
    ! r1       : 現在のキーの位置

    implicit none
    integer :: i, j
    integer :: cnt, r0, r1
    character(26) :: S, alphabet
    character(1) :: Sn

    ! 入力
    read (*, *) S
    alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    ! 'A'の位置を検索して、開始位置 r1 を設定
    do i = 1, 26
        if (S(i:i) == "A") then
            r1 = i
            exit
        end if
    end do

    ! 標準のアルファベットから 'B' から 'Z' までを順に処理
    cnt = 0
    do i = 2, 26
        r0 = r1
        Sn = alphabet(i:i)
        ! キーボード配列 S の中で Sn の位置を検索
        do j = 1, 26
            if (S(j:j) == Sn) then
                r1 = j
                cnt = cnt + abs(r1 - r0)
                exit
            end if
        end do
    end do

    ! 出力: 移動距離の合計を表示
    print *, cnt

end program abc373b
