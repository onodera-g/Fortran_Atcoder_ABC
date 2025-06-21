program ABC406a
    ! A : 締切時の時 (0–23)
    ! B : 締切時の分 (0–59)
    ! C : 提出時の時 (0–23)
    ! D : 提出時の分 (0–59)

    implicit none
    integer :: A, B, C, D

    ! 入力
    read (*, *) A, B, C, D

    ! 提出時刻が締切時刻よりも前かどうかを判定
    if (C < A .or. (C == A .and. D < B)) then
        print *, "Yes"
    else
        print *, "No"
    end if

end program ABC406a
