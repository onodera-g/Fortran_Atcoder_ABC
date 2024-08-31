program abc346c
    !N  ：正整数列の長さ
    !A  ：正整数列A
    !K  ：総和の対象外にする上限値
    !ans：1 以上 K 以下の整数のうち、A の中に一度も現れないものの総和
    use stdlib_kinds
    use stdlib_hashmaps
    use stdlib_hashmap_wrappers
    implicit none
    !変数
    integer i, tmp
    integer(16) N, K, ans
    integer(16), allocatable::A(:)
    logical :: present

    !hash map
    type(key_type)              :: key
    type(other_type)            :: other
    type(chaining_hashmap_type) :: map
    call map%init(fnv_1_hasher)

    !入力
    read (*, *) N, K
    allocate (A(N))
    read (*, *) A

    !合計の計算
    ans = K*(K + 1)/2
    tmp = 1; call set(other, tmp)
    do i = 1, N
        if (A(i) <= K) then
            key%value = transfer(A(i), key%value)
            call map%key_test(key, present)
            if (present .eqv. .false.) then
                call map%map_entry(key, other)
                ans = ans - A(i)
            end if
        end if
    end do

    !結果の出力
    print *, ans

end program abc346c
