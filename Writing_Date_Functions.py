# -*- coding: utf-8 -*-
"""
Created on Fri Mar 14 04:50:25 2025

@author: bawoyemi
"""
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
""" 
A LEAP YEAR
"""

def is_year_leap(year):
    #
    if year % 4 != 0:
        return False
    elif year % 100 != 0:
        return True
    elif year % 400 != 0:
        return False
    else:
        return True


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
""" 
HOW MANY DAYS?
"""

def days_in_month(year, month):
    #
    if year < 1582 or month < 1 or month > 12:
        return None

    days_in_m = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    if month == 2 and is_year_leap(year):
        days_in_m[month - 1] = 29
        
    num_of_days_in_m = days_in_m[month - 1]
    return num_of_days_in_m

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
""" 
DAY OF THE YEAR
"""

def day_of_year(year, month, day):
    
    # if is_year_leap(year) == False and month == 2 and day == 29:
    #     return None
    # Not necessary since it is addressed below
    
    if day > days_in_month(year, month):
        return None
    
    week_days = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday']
    
    leap_years = 0
    
    no_of_years = len(range(1582, year)) # Number of years before given year
    
    for i in range(1582, year):
        if is_year_leap(i):
            leap_years += 1 
    common_years = no_of_years - leap_years
    tot_days = common_years * 365 + leap_years * 366
    
    num = tot_days % 7
    
    first_day = (num + 5) % 7
    firstday_year = week_days[first_day] 
    # Added 5 because first day of 1582 was Friday which is week_days[5]
    days_so_far_in_year = 0
    
    for i in range(1, month):
        month_days = days_in_month(year, i)
        days_so_far_in_year += month_days
    
    num2 = days_so_far_in_year % 7
    first_day2 = (num2 + first_day - 1) % 7
    firstday_month = week_days[first_day2] 
    
    day_num = (first_day2 + day) % 7
    year_weekday = week_days[day_num]
    
    return year_weekday
    
print(day_of_year(2025, 12, 11))    

