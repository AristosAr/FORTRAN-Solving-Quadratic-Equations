module trinomial
    implicit none

    contains

    subroutine gt(a,b,r1,r2,delta)
        real:: a,b,r1,r2
        real:: delta

        r1=(-b + sqrt(delta)) / (2.*a)
        r2=(-b - sqrt(delta)) / (2.*a)
    end subroutine

    subroutine eq(a,b,r)
        real:: a,b,r

        r=-b / (2.*a)
    end subroutine

    subroutine lt(a,b,r1,r2,r,delta)
        real:: a,b,r1,r2,r
        real:: delta

        r= -b / (1.*a)
        r1= sqrt(delta) / (2.*a)
        r2= sqrt(delta) / (2.*a)
    end subroutine

end module

program trinomial_solution
    use trinomial
    implicit none

    real:: a,b,c
    real:: r1,r2,r
    real:: delta

    print*, "Give the values of the quadratic equation"
    read*, a,b,c

    delta = (b**2)-(4*a*c)

    if (delta > 0) then
        call gt(a,b,r1,r2,delta)
        print*, "Delta > 0 therefore there are two real solutions"
        print*, "r1= ", r1
        print*, "r2= ", r2

        elseif (delta == 0) then
            call eq(a,b,r)
            print*, "Delta = 0 therefore there is a unique real solution "
            print*, "r= ",r

            else
                call lt(a,b,r1,r2,r,delta)
                print*, "Delta < 0 therefore there are two complex solutions"
                print*, "r1= ",r,"+",r1,"i"
                print*, "r2= ",r,"-",r2,"i"
    end if

end program

