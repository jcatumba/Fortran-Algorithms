!! Bisection method to find roots of functions
!! Implemented by Jorge Catumba jcatumbar at unal.edu.co

subroutine BISECTION(F, a, b, steps)
    !! Parameters:
    !! F: a function
    !! a: the lower value of the interval
    !! b: the upper value of the interval
    !! steps: the total steps to perform
    !! Result:
    !! c: the aproximate root of the function F

    function, intent (in) :: F
    float, intent (in) :: a
    float, intent (in) :: b
    integer, intent (in) :: steps
    float, intent (out) :: c

    float :: fa
    float :: fb
    float :: fc
    integer :: I

    fa = F(a)
    fb = F(b)

    if (fa*fb>0) then
        return
    end if

    do I=1, steps
        c = a + 0.5*(b-a)
        fc = F(c)

        if (fa*fc<0) then
            b = c
            fb = fc
        else if (fa*fc>0) then
            a = c
            fa = fc
        else
            return c
        end if
    end do

    return a + 0.5*(b-a)
end subroutine