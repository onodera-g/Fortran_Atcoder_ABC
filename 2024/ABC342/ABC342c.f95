program abc342c
    !N   ：文字列Sの長さ
    !S   ：操作したい文字列
    !Q   ：クエリの数
    !c   ：クエリ：置き換え対象の文字
    !d   ：クエリ：置き換え後の文字
    !mozi：置き換えた文字の管理(a~z)
    implicit none
    integer i, j
    integer N, Q
    character(:), allocatable::S
    character(1), allocatable::c(:), d(:)
    character(1) mozi(127)

    !入力
    read (*, *) N
    allocate (character(N)::S)
    read (*, *) S
    read (*, *) Q
    allocate (c(Q), d(Q))
    do i = 1, Q
        read (*, *) c(i), d(i)
    end do

    !小文字の格納
    mozi = '#'
    do i = 97, 122
        mozi(i) = char(i)
    end do

    !クエリの処理
    do i = 1, Q
        do j = 97, 122
            if (mozi(j) == c(i)) mozi(j) = d(i)
        end do
    end do

    !結果の出力
    do i = 1, N
        S(i:i) = mozi(ichar(S(i:i)))
    end do
    write (*, *) S

end program abc342c
