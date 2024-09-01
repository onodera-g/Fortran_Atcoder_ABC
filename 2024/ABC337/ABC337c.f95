program ABC337c
    use stdlib_kinds
    use stdlib_hashmaps
    use stdlib_hashmap_wrappers
    !N  ：数列Aの長さ
    !A  ：数列A
    !Ans：列に並んでいる順番

    implicit none
    !hash map
    type(key_type)              :: key
    type(other_type)            :: other
    type(chaining_hashmap_type) :: map
    type dummy_type
        integer                 :: value(1)
    end type dummy_type
    type(dummy_type)            :: dummy
    class(*), allocatable       :: data

    !変数
    integer i, j
    integer N, tmp(1), tmp2(1)
    integer, allocatable::A(:), Ans(:)

    !hashmapの初期化
    call map%init(fnv_1_hasher)

    !入力
    read (*, *) N
    allocate (A(N), Ans(N))
    read (*, *) A(:)

    !hash map に登録
    do i = 1, N
        key%value = transfer(A(i), key%value)
        dummy%value = i
        call set(other, dummy)
        call map%map_entry(key, other)
    end do

    !先頭を確定
    tmp(1) = -1
    key%value = transfer(tmp(1), key%value)
    call map%get_other_data(key, other)
    call get(other, data)
    select type (data)
    type is (dummy_type)
        Ans(1) = data%value(1)
    end select

    !先頭より後ろを確定
    tmp(1) = Ans(1)
    do j = 2, N
        key%value = transfer(tmp(1), key%value)
        call map%get_other_data(key, other)
        call get(other, data)
        select type (data)
        type is (dummy_type)
            Ans(j) = data%value(1)
        end select
        tmp(1) = Ans(j)
    end do

    !結果の出力
    write (*, '(*(i0,1x))') Ans

end program ABC337c
