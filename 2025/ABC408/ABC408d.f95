program ABC408d
    ! T           ：総テスト数を格納する変数
    ! ci          ：現在処理中のテスト番号
    ! N           ：文字列 S の長さ
    ! S           ：長さ N の 0 と 1 のみからなる文字列
    ! B(i)        ：S(i) を +1 または -1 に変換した値を格納する配列
    ! i           ：ループ用カウンタ
    ! totalOnes   ：S に含まれる '1' の総数
    ! currentSum  ：Kadane のアルゴリズムで計算する現在の部分和
    ! maxSum      ：Kadane のアルゴリズムで検出した最大部分和

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none

    integer(int32) :: T
    integer(int32) :: ci
    integer(int32) :: N
    character(len=:), allocatable :: S
    integer(int32), allocatable :: B(:)
    integer(int32) :: i
    integer(int32) :: totalOnes
    integer(int32) :: currentSum
    integer(int32) :: maxSum

    ! 入力
    read (*, *) T
    do ci = 1, T
        read (*, *) N
        allocate (character(len=N) :: S)
        read (*, '(A)') S

        ! 整数配列 B を用意しつつ '1' の個数を数える
        allocate (B(N))
        totalOnes = 0
        do i = 1, N
            if (S(i:i) == '1') then
                totalOnes = totalOnes + 1
                B(i) = 1
            else
                B(i) = -1
            end if
        end do

        ! Kadane のアルゴリズムで最大部分和を求める
        currentSum = 0
        maxSum = 0
        do i = 1, N
            currentSum = currentSum + B(i)
            if (currentSum < 0) then
                currentSum = 0
            end if
            if (currentSum > maxSum) then
                maxSum = currentSum
            end if
        end do

        ! 最小操作回数を計算して出力する
        print *, totalOnes - maxSum

        deallocate (S)
        deallocate (B)
    end do

end program ABC408d
