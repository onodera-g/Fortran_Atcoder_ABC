program abc356c
    !N：鍵の本数
    !M：試したテスト回数
    !K：ドアに最低刺さす必要のある鍵の本数
    !C(M)：各テストで差し込んだ鍵の総数
    !R(M)：ドアの開閉　o：開、x：閉
    !A(M,N)：各テストで差し込んだ鍵の種類
    !bit_C(M)：使用した鍵の組み合わせを２進数で管理
    !ans：どのテスト結果にも矛盾しない組み合わせの個数
    implicit none
    integer(16) i, j
    integer(16) N, M, K, ans
    integer(16), allocatable::C(:), A(:, :), bit_C(:)
    character(1), allocatable:: R(:)

    !入力
    read (*, *) N, M, K
    allocate (C(M), A(M, N), R(M), bit_C(M))
    A = 0; bit_C = 0; ans = 0
    do i = 1, M
        read (*, *) C(i), (A(i, j), j=1, C(i)), R(i)
    end do

    !使った鍵を2ビットで管理、1：使用、0：未使用
    do i = 1, M
        do j = 1, C(i)
            bit_C(i) = ibset(bit_C(i), A(i, j) - 1)
        end do
    end do

    !ビット全探索で一致ケースを数える
    check_key: do i = 0, 2**N - 1
        do j = 1, M
            if (R(j) == "o") then
                if (popcnt(iand(bit_C(j), i)) < k) cycle check_key
            else
                if (popcnt(iand(bit_C(j), i)) >= k) cycle check_key
            end if
        end do
        ans = ans + 1
    end do check_key

    !結果の出力
    write (*, *) ans
end program abc356c
