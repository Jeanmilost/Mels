/******************************************************************************
 * ==> QR_DateAndTimeTools ---------------------------------------------------*
 ******************************************************************************
 * Description : Date and time tools                                          *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_DateAndTimeToolsH
#define QR_DateAndTimeToolsH

// std
#include <string>

// qr engine
#include "QR_Types.h"

/**
* Universal date, can convert from and to many formats
*@author Jean-Milost Reymond
*/
class QR_Date
{
    public:
        QR_Date();
        QR_Date(QR_Int32 year, QR_Int32 month, QR_Int32 day);

        /**
        * Gets year
        *@returns year
        */
        QR_Int32 GetYear();

        /**
        * Sets year
        *@param year - year
        */
        void SetYear(QR_Int32 year);

        /**
        * Gets month
        *@returns month
        */
        QR_Int32 GetMonth();

        /**
        * Sets month
        *@param month - month
        */
        void SetMonth(QR_Int32 month);

        /**
        * Gets day
        *@returns day
        */
        QR_Int32 GetDay();

        /**
        * Sets day
        *@param day - day
        */
        void SetDay(QR_Int32 day);

        /**
        * Converts date from ISO 8601
        *@param date - ISO 8601 date to convert
        */
        void FromISO8601(const std::string& date);

        /**
        * Converts date to ISO 8601
        *@param separator - separator to use
        *@returns converted ISO 8601 date
        */
        std::string ToISO8601(QR_Char separator = '-');

    private:
        QR_Int32 m_Year;
        QR_Int32 m_Month;
        QR_Int32 m_Day;

        /**
        * Extracts value from ISO 8601 string
        *@param date - ISO 8601 date to extract from
        *@param defaultValue - default value in case result is empty
        *@param[in, out] position - current position in string, new position when function ends
        *@returns extracted value
        */
        QR_Int32 ExtractValueFromISO8601(const std::string& date,
                                               QR_Int32     defaultValue,
                                               QR_SizeT&    position);
};

/**
* Universal time, can convert from and to many formats
*@author Jean-Milost Reymond
*/
class QR_Time
{
};

/**
* Universal date and time, can convert from and to many formats
*@author Jean-Milost Reymond
*/
class QR_DateTime
{
    QR_Date m_Date;
    QR_Time m_Time;
};

/**
* Date and time tools
*@author Jean-Milost Reymond
*/
class QR_DateAndTimeTools
{
    public:
        /**
        * Get date now as string, using a custom conversion format (dd-mm-yyyy)
        *@param separator - separator
        *@returns date as string
        */
        static std::wstring GetDateNow_Custom(QR_WChar separator = L'-');

        /**
        * Get time now as string, using a custom conversion format (hh:mm:ss)
         *@param separator - separator
        *@returns time as string
        */
        static std::wstring GetTimeNow_Custom(QR_WChar separator = L':');

        /**
        * Get date and time now as string, using a custom conversion format (dd-mm-yyyy hh:mm:ss)
        *@param dateSeparator - separator to use for date
        *@param timeSeparator - separator to use for time
        *@returns date and time as string
        */
        static std::wstring GetDateTimeNow_Custom(QR_WChar dateSeparator = L'-', QR_WChar timeSeparator = L':');
};
#endif
