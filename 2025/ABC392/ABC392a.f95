program ABC392a
    ! A1, A2, A3  : 入力される3つの正整数
    ! flag          : 条件を満たすかどうかのフラグ

    implicit none
    integer :: A1, A2, A3
    logical :: flag

    ! 入力
    read (*, *) A1, A2, A3

    ! いずれかの 2 つの積が残りの 1 つに等しいかをチェック
    flag = .false.
    if (A1*A2 == A3) flag = .true.
    if (A1*A3 == A2) flag = .true.
    if (A2*A3 == A1) flag = .true.

    ! 出力
    if (flag) then
        print *, "Yes"
    else
        print *, "No"
    end if

end program ABC392a
