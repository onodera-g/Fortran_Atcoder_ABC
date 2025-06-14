program ABC400a
    ! A       : 行数として与えられる正整数
    ! B       : 列数（A × B = 400 を満たす正整数、存在しない場合は -1）

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    integer(int32) :: A, B

    ! 入力
    read (*, *) A

    ! 400 人を A 行に並べるときに余りなく列数が定まるかを判定
    if (mod(400, A) == 0) then
        ! 余りなく並べられるなら B = 400 / A
        B = 400/A
    else
        ! 並べられないなら -1
        B = -1
    end if

    ! 結果の出力
    write (*, '(I0)') B

end program ABC400a
