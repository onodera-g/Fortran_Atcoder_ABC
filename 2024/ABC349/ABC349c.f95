program abc349c
    !S             ：文字列S
    !T             ：空港コード
    !cnt           ：Sと空港コードのヒット数
    !alphabet_large：アルファベット大文字(Tを小文字に変換する対応表)
    !alphabet_small：アルファベット小文字(Tを小文字に変換する対応表)
    implicit none
    integer i, j
    integer cnt
    character(100000) S
    character(3) T
    character(26) alphabet_large, alphabet_small
    alphabet_large = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    alphabet_small = 'abcdefghijklmnopqrstuvwxyz'

    !入力
    read (*, *) S
    read (*, *) T

    !Tを小文字に変換
    do i = 1, 3
        do j = 1, 26
            if (T(i:i) == alphabet_large(j:j)) then
                T(i:i) = alphabet_small(j:j)
                exit
            end if
        end do
    end do

    !Sに含まれるアルファベットのカウント
    cnt = 1
    do i = 1, len_trim(S)
        if (S(i:i) == T(cnt:cnt)) cnt = cnt + 1
    end do

    !空港コードが再現できるか
    if (cnt == 4) then
        print *, 'Yes'
    else if (cnt == 3 .and. T(3:3) == 'x') then
        print *, 'Yes'
    else
        print *, 'No'
    end if
end program abc349c
