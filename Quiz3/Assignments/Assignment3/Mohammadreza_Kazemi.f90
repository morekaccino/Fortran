program Assignment3
    ! Declaring variables
    implicit none
    real :: starting_price, ending_price, exchange_rate, frac, temp, i
    real :: after_discount, after_discount_tax, after_discount_tax_US
    integer :: discount
    character(len = 85) :: col_name
    logical :: starting_49, starting_99, ending_49, ending_99, positive

    ! Welcome Banner
    write(*, *) "This program creates a discount chart for the store. HST+GST is accounted 13%."
    write(*, *) "You can copy-&-paste the chart from the screen to print and post."
    write(*, *) "You should provide the store price range, and most current CAD to USD exchange rate"
    write(*, *) "If you are not ready yet press control+C to exit."

    ! Setting the col name
    write(*, *) "Enter the title (maximum 85 characters between quotations) of the discount chart."
    read(*, *) col_name

    ! Setting min and max price
    write(*, *) "Please enter the minimum and maximum price tags in store using comma."
    write(*, *) "(Note: price-tags must end in .49 or .99, For example: 1.99,14.99)"
    read(*, *) starting_price, ending_price
    starting_49 = abs(starting_price - int(starting_price) - 0.49) <= 0.000001
    starting_99 = abs(starting_price - int(starting_price) - 0.99) <= 0.000001
    ending_49 = abs(ending_price - int(ending_price) - 0.49) <= 0.000001
    ending_99 = abs(ending_price - int(ending_price) - 0.99) <= 0.000001
    positive = (starting_price > 0) .and. (ending_price > 0)
    do while (.not.((starting_49 .or. starting_99) .and. (ending_49 .or. ending_99) .and. positive))
        write(*, *) "One or both price-tags are not valid values."
        write(*, *) "Note that price-tags must be positive and end in .49 or .99."
        write(*, *) "Please re-enter the minimum and maximum price tags in store."
        read(*, *) starting_price, ending_price
        starting_49 = abs(starting_price - int(starting_price) - 0.49) <= 0.000001
        starting_99 = abs(starting_price - int(starting_price) - 0.99) <= 0.000001
        ending_49 = abs(ending_price - int(ending_price) - 0.49) <= 0.000001
        ending_99 = abs(ending_price - int(ending_price) - 0.99) <= 0.000001
        positive = (starting_price > 0) .and. (ending_price > 0)
    end do

    ! Make sure starting is the lowest value
    if (starting_price > ending_price) then
        temp = starting_price
        starting_price = ending_price
        ending_price = temp
    end if

    ! Setting discount
    write(*, *) "Please enter the discount percentage (a value between 0 to 100)"
    write(*, *) "Note do not use % symbol otherwise execution exits."
    read(*, *) discount
    do while (discount < 0 .or. discount > 100)
        write(*, *) "The discount percentage is not a valid value."
        write(*, *) "Please re-enter a value between 0 to 100"
        read(*, *) discount
    end do

    ! Setting exchange_rate
    write(*, *) "Please enter the most current CAD to USD exchange rate."
    read(*, *) exchange_rate
    do while(exchange_rate < 0)
        write(*, *) "The exchange rate is not a valid value."
        write(*, *) "Note that exchange rate must be a positive value."
        write(*, *) "Please re-enter the most current CAD to USD exchange rate."
        read(*, *) exchange_rate
    end do

    ! The chart
    write(*, *) "copy-&-paste the chart (provided between the dash-lines)."
    write(*, *) "--------------------------------------------------------------"
    write(*, *) col_name
    write(*, *) "Original Price Tag Before Tax     Discounted Price After Tax"
    write(*, *) "CAD                               CAD            USD"
    i = starting_price
    do while (i < ending_price + 0.000001)
        after_discount = ((100 - discount) * i) / 100
        after_discount_tax = ((after_discount * 13) / 100) + after_discount
        after_discount_tax_US = after_discount_tax * exchange_rate
        write(*, *) i, after_discount_tax, after_discount_tax_US

        i = i + 0.5
    end do
    write(*, *) "--------------------------------------------------------------"

end program Assignment3
