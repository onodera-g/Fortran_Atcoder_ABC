program ABC404a
    ! S: 入力文字列
    ! seen(i): アルファベット 'a'+i-1 が S に出現したかどうかのフラグ

    use, intrinsic :: iso_fortran_env
    implicit none
    character(len=25) :: S
    logical           :: seen(26)
    integer(int32)    :: i, idx
    character(len=1)  :: c

    ! 入力
    read (*, '(A)') S

    ! S の各文字についてマーク
    seen = .false.
    do i = 1, len_trim(S)
        seen(iachar(S(i:i)) - iachar('a') + 1) = .true.
    end do

    ! 出現しなかった最小の文字を出力
    do idx = 1, 26
        if (.not. seen(idx)) then
            c = achar(iachar('a') + idx - 1)
            write (*, '(A)') c
            exit
        end if
    end do

end program ABC404a
