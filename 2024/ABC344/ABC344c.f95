program abc344c
    !A      ：数列A
    !N      ：数列Aの個数
    !B      ：数列B
    !M      ：数列Bの個数
    !C      ：数列C
    !L      ：数列Cの個数
    !X      ：数列X
    !Q      ：数列Xの個数
    !val    ；A,B,Cの和
    !present：hashmapに登録されているかの判定用
    use stdlib_kinds
    use stdlib_hashmaps
    use stdlib_hashmap_wrappers
    implicit none
    !変数
    integer(int32) i, j, k
    integer(int32) N, M, L, Q, val(1), tmp
    integer(int32), allocatable::A(:), B(:), C(:), X(:)
    logical :: present

    !hash map
    type(key_type)              :: key
    type(other_type)            :: other
    type(chaining_hashmap_type) :: map
    call map%init(fnv_1_hasher)

    !入力
    read (*, *) N; allocate (A(N)); read (*, *) A
    read (*, *) M; allocate (B(M)); read (*, *) B
    read (*, *) L; allocate (C(L)); read (*, *) C
    read (*, *) Q; allocate (X(Q)); read (*, *) X

    !考えられる回答の作成
    tmp = 1
    call set(other, tmp)
    do i = 1, N
        do j = 1, M
            do k = 1, L
                val(1) = A(i) + B(j) + C(k)
                !call set_int32_key(key, val)
                key%value = transfer(val, key%value)
                call map%key_test(key, present)
                if (present .eqv. .false.) then
                    call map%map_entry(key, other)
                end if
            end do
        end do
    end do

    !Xを満たすか検証
    do i = 1, Q
        val(1) = X(i)
        !call set_int32_key(key, val)
        key%value = transfer(val, key%value)
        call map%key_test(key, present)
        if (present .eqv. .false.) then
            print *, 'No'
        else
            print *, 'Yes'
        end if
    end do
end program abc344c
