program abc386a
    ! A, B, C, D       : 入力として与えられる4枚のカードの値。1から13の整数。
    ! E                : 追加するカードの値。1から13の整数を順に試す。
    ! freq(14)         : 各数字（1～13）の出現頻度をカウントする配列。インデックス0は未使用。
    ! possible         : フルハウスが形成可能かどうかを示すフラグ。初期値は .false.、条件を満たす場合 .true. になる。
    ! num_distinct     : 現在の5枚のカードにおける異なる数字の種類数。フルハウスはこの値が2である必要がある。
    ! count3           : 同じ数字が3枚ある種類のカウント。フルハウスではこの値が1である必要がある。
    ! count2           : 同じ数字が2枚ある種類のカウント。フルハウスではこの値が1である必要がある。
    implicit none
    integer :: A, B, C, D, E
    integer, dimension(14) :: freq
    integer :: i
    logical :: possible
    integer :: num_distinct, count3, count2

    ! 入力
    read (*, *) A, B, C, D
    possible = .false.

    ! 5枚目のカードEが1から13までの場合をためす
    do E = 1, 13
        ! A~Dのカードの値の頻度をカウント
        freq = 0
        freq(A) = freq(A) + 1
        freq(B) = freq(B) + 1
        freq(C) = freq(C) + 1
        freq(D) = freq(D) + 1
        freq(E) = freq(E) + 1

        ! フルハウスの条件をチェック
        num_distinct = 0
        count3 = 0
        count2 = 0

        ! 1~13までのカードがどれだけあるか確認
        do i = 1, 13
            if (freq(i) > 0) then
                num_distinct = num_distinct + 1
                if (freq(i) == 3) then !3カードの有無の確認
                    count3 = count3 + 1
                else if (freq(i) == 2) then !2カードの有無の確認
                    count2 = count2 + 1
                else
                    ! 3カードがないとフルハウスにはならないのでループを抜ける
                    count3 = -1
                    exit
                end if
            end if
        end do

        !　フルハウスの成立判定
        if (num_distinct == 2 .and. count3 == 1 .and. count2 == 1) then
            possible = .true.
            exit
        end if

    end do

    ! 結果の出力
    if (possible) then
        print *, 'Yes'
    else
        print *, 'No'
    end if

end program abc386a
