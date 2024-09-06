program ABC296C
    use stdlib_kinds
    use stdlib_hashmaps
    use stdlib_hashmap_wrappers
    implicit none

    !hashmap
    type(key_type)              :: key
    type(other_type)            :: other
    type(chaining_hashmap_type) :: map
    type dummy_type
        integer                 :: value(1)
    end type dummy_type
    type(dummy_type)            :: dummy
    class(*), allocatable       :: data

    !変数
    integer i, j, k
    integer N, X, Aj
    integer, allocatable::A(:)
    logical present

    !入力
    read (*, *) N, X
    allocate (A(N))
    read (*, *) A

    !hashmapの初期化
    call map%init(fnv_1_hasher)
    dummy%value = 1
    call set(other, dummy)

    !hashmapに登録
    do i = 1, N
        key%value = transfer(A(i), key%value)
        call map%key_test(key, present)
        if (present .eqv. .false.) call map%map_entry(key, other)
    end do

    !Ai − Aj =Xが成立するか
    do i = 1, N
        Aj = A(i) - X
        key%value = transfer(Aj, key%value)
        call map%key_test(key, present)
        if (present .eqv. .true.) then
            print *, 'Yes'
            stop
        end if
    end do
    print *, 'No'
end program ABC296C
