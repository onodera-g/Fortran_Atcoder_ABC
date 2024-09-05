program ABC310c
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
    integer N, cnt, lenA, MAXlen_A, A1(122)
    character(1) mozi
    character(100000) A, A_reverse
    logical present, flag_cnt

    !hashmapの初期化
    call map%init(fnv_1_hasher)
    dummy%value = 1
    call set(other, dummy)

    !異なる棒の種類数
    read (*, *) N
    cnt = 0; A1 = 0
    do i = 1, N
        read (*, *) A
        if (len_trim(A) == 1) then !１文字の場合
            mozi = trim(A)
            A1(ichar(mozi)) = A1(ichar(mozi)) + 1
        else !２文字以上の場合
            flag_cnt = .false.
            key%value = transfer(trim(A), key%value)
            call map%key_test(key, present)
            !Aが登録済みかチェック
            if (present .eqv. .false.) then !無
                call map%map_entry(key, other)
                flag_cnt = .true.
            end if
            !Aの逆順が登録済みかチェック
            lenA = len_trim(A)
            if (lenA /= 1) then
                A_reverse = A
                call reverse_string(A_reverse(1:lenA))
                key%value = transfer(trim(A_reverse), key%value)
                call map%key_test(key, present)
                if (present .eqv. .false.) then !無
                    call map%map_entry(key, other)
                    flag_cnt = .true.
                end if
            end if
            !未登録ならカウント
            if (flag_cnt .eqv. .true.) then
                cnt = cnt + 1
                !print *, trim(A)
            end if
        end if
    end do
    !１文字の分のカウント
    do i = 97, 122
        if (A1(i) > 0) then
            cnt = cnt + 1
            !print *, char(i)
        end if
    end do

    !結果の出力
    print *, cnt
contains

    subroutine reverse_string(original)
        implicit none
        character(len=*), intent(inout) :: original
        character(len=:), allocatable :: temp_string
        integer :: i, length_of_string

        ! 変数の説明:
        ! original: 逆さまにする元の文字列（入力兼出力）
        ! temp_string: 逆さまにした一時的な文字列
        ! length_of_string: 元の文字列の長さ
        ! i: ループカウンタ

        ! 元の文字列の長さを取得
        length_of_string = len_trim(original)

        ! 一時的な文字列temp_stringにメモリを確保
        allocate (character(len=length_of_string) :: temp_string)

        ! 文字列を逆さまにする処理
        do i = 1, length_of_string
            temp_string(i:i) = original(length_of_string - i + 1:length_of_string - i + 1)
        end do

        ! 結果をoriginalに上書き
        original = temp_string

        ! メモリを解放
        deallocate (temp_string)

    end subroutine reverse_string
end program ABC310c
