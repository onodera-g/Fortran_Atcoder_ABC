program ABC401b
    ! N              : 操作の回数
    ! op             : 各操作を表す文字列 ("login","logout","public","private")
    ! is_logged_in   : 現在ログイン状態かどうかを示すフラグ (.true. = ログイン中)
    ! error_count    : 認証エラーを受け取った回数

    implicit none
    integer :: N, i
    character(len=7) :: op
    logical :: is_logged_in
    integer :: error_count

    ! 初期状態：ログインしていない状態
    is_logged_in = .false.
    error_count = 0

    ! 入力
    read (*, *) N

    !　ログイン状態の判定
    do i = 1, N
        read (*, '(A)') op
        select case (op)
        case ("login")
            ! ログイン操作：ログイン状態に
            is_logged_in = .true.
        case ("logout")
            ! ログアウト操作：ログアウト状態に
            is_logged_in = .false.
        case ("public")
            ! 公開ページアクセス：エラーなし
            ! 何もしない
        case ("private")
            ! 非公開ページアクセス：ログインしていなければ認証エラー
            if (.not. is_logged_in) then
                error_count = error_count + 1
            end if
        end select
    end do

    ! 結果の出力
    print *, error_count

end program ABC401b
