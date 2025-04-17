import datetime as dt
import holidays

def same_day_last_year(d: dt.datetime, holiday_adjustment: bool = True, shoulder: int = 0, country_alpha2_code: str = 'US') -> dt.datetime: 
    # target year, week, and day
    target_year = d.year - 1
    target_week = d.isocalendar()[1]
    target_day_num = d.isocalendar()[2]
    target_day_name = d.strftime('%A')
    
    # addressing first day/week of the year issues
    first_day = dt.date(target_year, 1, 1)
    if first_day.isocalendar()[2] > target_day_num:
      target_week += 1
      
    if holiday_adjustment != True:
      # computing target_date
      delta_days = (target_week -1)*7 + (target_day_num - first_day.isocalendar()[2])
      target_date = first_day + dt.timedelta(days = delta_days)
      return(target_date)
    
    else:
      # returning the same holiday from previous year
      # note that for simplification, I am ignoring the fact that the holiday can
      # be observed on a different day (with the assumption that the demand will be high either way;
      # typically, if it is observed on a different day -- due to holiday falling on a weekend)
      holiday_lst = holidays.country_holidays(country = country_alpha2_code, years = [d.year-1, d.year])
      
      if d in holiday_lst:
        holiday_name = holiday_lst.get(d).replace(' (Observed)', '')
        target_date = get_holiday_date(holiday_name, d.year-1, country = country_alpha2_code)
        return(target_date)
      
      if shoulder > 0:
        for i in range(-shoulder, (shoulder + 1) ):
          check_date = d + dt.timedelta(days = i)
          if check_date in holiday_lst:
            holiday_name = holiday_lst.get(check_date).replace(' (Observed)', '')
            target_date = get_holiday_date(holiday_name, d.year-1, country = country_alpha2_code)
            target_date = target_date - dt.timedelta(days = i)
            return(target_date)
          
      delta_days = (target_week -1)*7 + (target_day_num - first_day.isocalendar()[2])
      target_date = first_day + dt.timedelta(days = delta_days)
      return(target_date)
