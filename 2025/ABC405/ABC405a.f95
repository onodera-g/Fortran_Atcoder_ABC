program ABC405a
    ! R  : レーティング
    ! X  : ARC の Division (1 または 2)

    use iso_fortran_env, only: int32
    implicit none
    integer(int32) :: R, X

    ! 入力
    read (*, *) R, X

    ! 判定および結果の出力
    if (X == 1) then
        ! Div.1: 1600 ≤ R ≤ 2999
        if (1600 <= R .and. R <= 2999) then
            print *, "Yes"
        else
            print *, "No"
        end if

    else if (X == 2) then
        ! Div.2: 1200 ≤ R ≤ 2399
        if (1200 <= R .and. R <= 2399) then
            print *, "Yes"
        else
            print *, "No"
        end if

    else
        ! X は 1 または 2 のはず
        print *, "No"
    end if

end program ABC405a
