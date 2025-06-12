program ABC396a
    ! X       : 入力される体温 (小数第一位まで)
    ! category: 分類結果を格納する整数 (1=高熱, 2=発熱, 3=平熱)

    use, intrinsic :: iso_fortran_env, only: real64
    implicit none

    real(real64) :: X
    integer :: category

    ! 入力
    read (*, *) X

    ! 分類判定
    if (X >= 38.0_real64) then
        category = 1 ! 高熱
    else if (X >= 37.5_real64) then
        category = 2 ! 発熱
    else
        category = 3 ! 平熱
    end if

    ! 結果の出力
    write (*, '(I0)') category

end program ABC396a
