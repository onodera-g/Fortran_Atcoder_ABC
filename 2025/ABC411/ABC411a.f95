program ABC411a
    ! P       : 入力されるパスワード候補（英小文字からなる文字列）
    ! L       : 要求される最小の長さ
    ! lenP    : P の長さ

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    character(len=100) :: P
    integer(int32)   :: L, lenP

    ! 入力
    read (*, '(A)') P
    read (*, *) L
    lenP = len_trim(P)

    ! 結果の出力
    if (lenP >= L) then
        print *, 'Yes'
    else
        print *, 'No'
    end if

end program ABC411a
