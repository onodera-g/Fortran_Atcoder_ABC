program ABC403b
    ! T        : 英小文字と'?'からなる文字列（長さ 4～10）
    ! U        : 英小文字のみからなる文字列（長さ 1～|T|）
    ! n, m     : T, U の長さ
    ! i, j     : ループカウンタ
    ! match    : 部分文字列一致可能フラグ

    use, intrinsic :: iso_fortran_env
    implicit none
    character(len=10) :: T, U
    integer :: n, m, i, j
    logical :: match

    ! 入力
    read (*, '(A)') T
    read (*, '(A)') U
    n = len_trim(T)
    m = len_trim(U)

    ! 連続部分文字列として含むかどうかの判定
    do i = 1, n - m + 1
        match = .true.
        do j = 1, m
            if (T(i + j - 1:i + j - 1) /= '?' .and. T(i + j - 1:i + j - 1) /= U(j:j)) then
                match = .false.
                exit
            end if
        end do
        if (match) then
            write (*, '(A)') 'Yes'
            stop
        end if
    end do
    write (*, '(A)') 'No'
end program ABC403b
