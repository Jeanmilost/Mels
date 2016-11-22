/******************************************************************************
 * ==> QR_DateAndTimeTools ---------------------------------------------------*
 ******************************************************************************
 * Description : Date and time tools                                          *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_DateAndTimeTools.h"

// std
#include <cstdlib>
#include <sstream>
#include <ctime>

//------------------------------------------------------------------------------
// QR_Date
//------------------------------------------------------------------------------
QR_Date::QR_Date() :
    m_Year(0),
    m_Month(0),
    m_Day(0)
{}
//------------------------------------------------------------------------------
QR_Date::QR_Date(QR_Int32 year, QR_Int32 month, QR_Int32 day) :
    m_Year(year),
    m_Month(month),
    m_Day(day)
{}
//------------------------------------------------------------------------------
QR_Int32 QR_Date::GetYear()
{
    return m_Year;
}
//------------------------------------------------------------------------------
void QR_Date::SetYear(QR_Int32 year)
{
    m_Year = year;
}
//------------------------------------------------------------------------------
QR_Int32 QR_Date::GetMonth()
{
    return m_Month;
}
//------------------------------------------------------------------------------
void QR_Date::SetMonth(QR_Int32 month)
{
    m_Month = month;
}
//------------------------------------------------------------------------------
QR_Int32 QR_Date::GetDay()
{
    return m_Day;
}
//------------------------------------------------------------------------------
void QR_Date::SetDay(QR_Int32 day)
{
    m_Day = day;
}
//------------------------------------------------------------------------------
void QR_Date::FromISO8601(const std::string& date)
{
    QR_SizeT position = 0;

    // read date
    m_Year  = ExtractValueFromISO8601(date, 1970, position);
    m_Month = ExtractValueFromISO8601(date, 1,    position);
    m_Day   = ExtractValueFromISO8601(date, 1,    position);
}
//------------------------------------------------------------------------------
std::string QR_Date::ToISO8601(QR_Char separator)
{
    std::ostringstream sstr;

    // add year
    sstr << m_Year;

    // is month out of bounds?
    if (m_Month < 1 || m_Month > 12)
        return sstr.str();

    // add month
    if (separator)
        sstr << std::string(1, separator) << m_Month;
    else
        sstr << m_Month;

    // is day out of bounds?
    if (m_Month < 1 || m_Month > 12)
        return sstr.str();

    // add day
    if (separator)
        sstr << std::string(1, separator) << m_Day;
    else
        sstr << m_Day;

    return sstr.str();
}
//------------------------------------------------------------------------------
QR_Int32 QR_Date::ExtractValueFromISO8601(const std::string& date,
                                                QR_Int32     defaultValue,
                                                QR_SizeT&    position)
{
    std::string value;

    // iterate through string to convert
    for (QR_SizeT i = position; i < date.size(); ++i)
    {
        // update string position
        ++position;

        // is numeric char?
        if (date[i] >= '0' && date[i] <= '9')
            // get numeric value
            value += date[i];
        else
            break;
    }

    // no value?
    if (value.empty())
        return defaultValue;

    // return converted value
    return std::atoi(value.c_str());
}
//------------------------------------------------------------------------------
// QR_DateAndTimeTools
//------------------------------------------------------------------------------
std::wstring QR_DateAndTimeTools::GetDateNow_Custom(QR_WChar separator)
{
    // get date now
    std::time_t t    = std::time(0);
    std::tm*    pNow = std::localtime(&t);

    std::wostringstream sstr;

    // convert date to string
    sstr << pNow->tm_mday << separator << (pNow->tm_mon + 1) << separator << (pNow->tm_year + 1900);

    return sstr.str();
}
//------------------------------------------------------------------------------
std::wstring QR_DateAndTimeTools::GetTimeNow_Custom(QR_WChar separator)
{
    // get time now
    std::time_t t    = std::time(0);
    std::tm*    pNow = std::localtime(&t);

    std::wostringstream sstr;

    // convert time to string
    sstr << (pNow->tm_hour) << separator << (pNow->tm_min) << separator << pNow->tm_sec;

    return sstr.str();
}
//------------------------------------------------------------------------------
std::wstring QR_DateAndTimeTools::GetDateTimeNow_Custom(QR_WChar dateSeparator, QR_WChar timeSeparator)
{
    std::wostringstream sstr;

    // convert date and time to string
    sstr << GetDateNow_Custom(dateSeparator) << " " << GetTimeNow_Custom(timeSeparator);

    return sstr.str();
}
//------------------------------------------------------------------------------

