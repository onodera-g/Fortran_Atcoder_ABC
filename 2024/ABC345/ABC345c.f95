program abc345c
    !S   ：文字列
    !mozi：文字列Sに含まれるアルファベットの出現回数
    !cnt ：組み合わせのパターン数
    !flag：文字列Sに含まれるアルファベットに同一のものが含まれれるか
    implicit none
    character(2000000) S
    integer(16) i, j, tmp
    integer(16) mozi(122), cnt
    logical flag

    !入力
    read (*, *) S

    !出現するあるdファベットのカウント
    mozi = 0
    do i = 1, len_trim(S)
        tmp = ichar(S(i:i))
        mozi(tmp) = mozi(tmp) + 1
    end do

    !組み合わせパターンの計算
    cnt = 0; flag = .false.
    do i = 97, 122
        if (mozi(i) > 1) flag = .true.
        do j = i + 1, 122
            if (i /= j) cnt = cnt + mozi(i)*mozi(j)
        end do
    end do
    if (flag .eqv. .true.) cnt = cnt + 1

    !結果の出力
    write (*, *) cnt

end program abc345c
