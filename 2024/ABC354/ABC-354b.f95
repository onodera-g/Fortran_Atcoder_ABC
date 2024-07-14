program abc354b
    use stdlib_kinds
    use iso_fortran_env
    use stdlib_hashmaps
    use stdlib_hashmap_wrappers
    use stdlib_sorting
    implicit none
    !hashmap
    type(key_type):: key
    type(other_type):: other
    type(chaining_hashmap_type):m
    !N       ：ユーザーの数
    !rate    ：各ユーザーのレート
    !sum_rate：レートの合計
    !Sn      ：ユーザーと紐つける番号
    !S       ：ユーザー名
    integer(8) N, i, rate, sum_rate
    integer(int8) rate_mod
    integer(int8), allocatable ::Sn(:)
    character(16), allocatable::S(:)
    call m%init(fnv_1_hasher)

    !入力
    read (*, *) N
    sum_rate = 0
    allocate (S(N), Sn(N))
    do i = 1, N
        read (*, *) S(i), rate
        sum_rate = sum_rate + rate
        !write (*, *) sum_rate
    end do

    !ユーザー名と管理番号の紐つけ
    call sort(S)
    do i = 1, N
        Sn(i) = i - 1
        call set(key, [Sn(i)])
        call set(other, S(i))
        call m%map_entry(key, other)
    end do

    !勝者の計算
    rate_mod = mod(sum_rate, N)
    !write (*, *) rate_mod

    !勝者のユーザーの呼び出し、結果の出力
    call set(key, [rate_mod])
    call m%get_other_data(key, other)
    select type (v => other%value)
    type is (CHARACTER(*))
        write (*, *) v
    class default
        print *, 'Invalid data type in other'
    end select
end program
