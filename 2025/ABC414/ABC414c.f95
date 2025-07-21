program abc414c
    ! A           : 与えられた進数
    ! N           : 上限値
    ! total       : 条件を満たす数の総和
    ! num         : 現在調べている回文数
    ! s           : 文字列にしたときの一時変数
    ! i           : ループ変数
    ! is_palindrome_base: A進数の文字列が回文かどうか判定する関数
    implicit none
    integer :: A
    integer(kind=8) :: N
    integer(kind=8) :: total, num
    integer :: i
    character(len=40) :: s

    ! 入力
    read (*, *) A, N
    total = 0

    ! 回文数の生成: 奇数桁
    do i = 1, 1000000
        write (s, '(I0)') i
        call make_palindrome(s, num, .true.)
        if (num > N) exit
        if (is_palindrome_base(num, A)) total = total + num
    end do

    ! 回文数の生成: 偶数桁
    do i = 1, 1000000
        write (s, '(I0)') i
        call make_palindrome(s, num, .false.)
        if (num > N) exit
        if (is_palindrome_base(num, A)) total = total + num
    end do

    ! 結果の出力
    print *, total

contains

    ! 回文を生成するサブルーチン
    ! s: 前半文字列
    ! num: 生成した回文数
    ! odd: 奇数桁かどうか
    subroutine make_palindrome(s, num, odd)
        character(len=*), intent(in) :: s
        integer(kind=8), intent(out) :: num
        logical, intent(in) :: odd
        character(len=40) :: tmp
        integer :: len_s, j

        len_s = len_trim(s)
        tmp = s
        if (odd) then
            do j = len_s - 1, 1, -1
                tmp(len_trim(tmp) + 1:len_trim(tmp) + 1) = s(j:j)
            end do
        else
            do j = len_s, 1, -1
                tmp(len_trim(tmp) + 1:len_trim(tmp) + 1) = s(j:j)
            end do
        end if
        read (tmp(1:len_trim(tmp)), *) num
    end subroutine make_palindrome

    ! A進数の文字列が回文か判定する関数
    logical function is_palindrome_base(num, base)
        integer(kind=8), intent(in) :: num
        integer, intent(in) :: base
        character(len=40) :: digits
        integer(kind=8) :: n, b
        integer(kind=8) :: digit
        integer :: len_d, i

        n = num
        b = int(base, kind=8)
        len_d = 0
        do while (n > 0)
            len_d = len_d + 1
            digit = mod(n, b)
            digits(len_d:len_d) = achar(iachar('0') + int(digit))
            n = n/b
        end do

        is_palindrome_base = .true.
        do i = 1, len_d/2
            if (digits(i:i) /= digits(len_d - i + 1:len_d - i + 1)) then
                is_palindrome_base = .false.
                return
            end if
        end do
    end function is_palindrome_base

end program abc414c
