program ABC387c
    ! N   :  数列の長さ
    ! A   :  正性数列
    ! map :  Aiの出現位置を記録する連想配列
    use stdlib_kinds
    use stdlib_hashmaps
    use stdlib_hashmap_wrappers
    ! hash map
    type(key_type)              :: key
    type(other_type)            :: other
    type(chaining_hashmap_type) :: map
    type dummy_type
        integer(16)                   :: value(1)
    end type dummy_type
    type(dummy_type) :: dummy
    class(*), allocatable       :: data
    logical :: present

    ! 変数
    integer(16) i, j, k
    integer(16) N, val
    integer(16), allocatable ::A(:)

    ! hashmapの初期化
    call map%init(fnv_1_hasher)

    ! 入力
    read (*, *) N
    allocate (A(N))
    read (*, *) A

    ! 数列 B の判定
    do i = 1, N
        val = A(i)
        key%value = transfer(val, key%value)
        call map%key_test(key, present)
        if (present .eqv. .false.) then ! 未登録の場合
            write (*, '(i0,1x)', advance='no') - 1
            ! 新しい値を登録
            val = i
            dummy%value = val
            call set(other, dummy)
            call map%map_entry(key, other)
        else ! データありの場合
            ! データ取り出し
            call map%get_other_data(key, other)
            call get(other, data)
            select type (data)
            type is (dummy_type)
                write (*, '(i0,1x)', advance='no') data%value
            end select
            ! 次の値で上書き
            call map%remove(key)
            dummy%value = [i]
            call set(other, dummy)
            call map%map_entry(key, other)
        end if
    end do
end program ABC387c
