program abc348c
    use stdlib_kinds
    use stdlib_hashmaps
    use stdlib_hashmap_wrappers
    implicit none
    !N  ：ビーンズの総数
    !A  ：各ビーンズのおいしさ
    !C  ：各ビーンズの色
    !ans：ビーンズの色ごとのおいしさの最小値の最大値

    !hash map
    type(key_type)              :: key
    type(key_type), allocatable :: keys(:)
    type(other_type)            :: other
    type(chaining_hashmap_type) :: map
    type dummy_type
        integer                 :: value(1)
    end type dummy_type
    type(dummy_type)            :: dummy
    class(*), allocatable       :: data

    !変数
    integer i, N, ans, tmp(1)
    integer, allocatable ::A(:), C(:)
    logical present

    !hashmapの初期化
    call map%init(fnv_1_hasher)

    !入力
    read (*, *) N
    allocate (A(N), C(N))

    !hash mapに最小値を登録
    do i = 1, N
        read (*, *) A(i), C(i)
        !データの登録有無の判定
        key%value = transfer(C(i), key%value)
        call map%key_test(key, present)
        if (present .eqv. .false.) then !無
            dummy%value = A(i)
            call set(other, dummy)
            call map%map_entry(key, other)
        else !有
            !hashmapから値を取得
            call map%get_other_data(key, other)
            call get(other, data)
            select type (data)
            type is (dummy_type)
                tmp(1) = data%value(1)
            end select
            !最小値の更新
            if (A(i) < tmp(1)) then
                call map%remove(key)
                dummy%value = A(i)
                call set(other, dummy)
                call map%map_entry(key, other)
            end if
        end if
    end do

    !ビーンズの美味しさの最大値の取得
    ans = 0
    do i = 1, N
        key%value = transfer(C(i), key%value)
        call map%get_other_data(key, other)
        call get(other, data)
        select type (data)
        type is (dummy_type)
            tmp(1) = data%value(1)
        end select
        ans = max(ans, tmp(1))
    end do

    !結果の出力
    write (*, *) ans

    !********************************************************************************
    !  本来であれば、call map%get_all_keysして登録しているkeyを取得する方が計算量が減るので良い。
    !  ただし、Fortran stdlib v0.2 には　get_all_keys　は実装されていない。(v0.4から)
    !********************************************************************************
    !ans = 0
    !call map%get_all_keys(keys)
    !do i = 1, size(keys)
    !    call map%get_other_data(keys(i), other)
    !    call get(other, data)
    !    select type (data)
    !    type is (dummy_type)
    !        tmp(1) = data%value(1)
    !    end select
    !    ans = max(ans, tmp(1))
    !end do

end program abc348c
