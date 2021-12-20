program Quiz1
    implicit none
    real :: y, x, a

    x = 1.3
    a = (1) / (3 + (27.1 * (x ** 2)))
    y = (((x ** 3.2) - 1) ** (4 * cos(x))) / (log(x - 1) - a)
    write(*, *) "For x=", x, "y is equal to:", y

    x = 2.6
    a = (1) / (3 + (27.1 * (x ** 2)))
    y = (((x ** 3.2) - 1) ** (4 * cos(x))) / (log(x - 1) - a)
    write(*, *) "For x=", x, "y is equal to:", y

    x = 3.9
    a = (1) / (3 + (27.1 * (x ** 2)))
    y = (((x ** 3.2) - 1) ** (4 * cos(x))) / (log(x - 1) - a)
    write(*, *) "For x=", x, "y is equal to:", y

    x = 5.2
    a = (1) / (3 + (27.1 * (x ** 2)))
    y = (((x ** 3.2) - 1) ** (4 * cos(x))) / (log(x - 1) - a)
    write(*, *) "For x=", x, "y is equal to:", y

end program
