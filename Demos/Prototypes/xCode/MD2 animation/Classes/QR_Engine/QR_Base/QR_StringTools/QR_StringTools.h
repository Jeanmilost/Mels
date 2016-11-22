/******************************************************************************
 * ==> QR_StringTools --------------------------------------------------------*
 ******************************************************************************
 * Description : Tools for string manipulation                                *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_StringToolsH
#define QR_StringToolsH

// std
#include <string>

// qr engine
#include "QR_Types.h"

//------------------------------------------------------------------------------
// Specific VCL macros
//------------------------------------------------------------------------------
#define M_UniToChar(str)  (str.IsEmpty() ? ""  : AnsiString(str).c_str())
#define M_UniToWChar(str) (str.IsEmpty() ? L"" : str.c_str())
#define M_StrToUni(str)   (str.empty()   ? ""  : UnicodeString(AnsiString(str.c_str()))
#define M_WStrToUni(str)  (str.empty()   ? L"" : UnicodeString(str.c_str())
//------------------------------------------------------------------------------

/**
* Tools for string manipulation
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_StringTools
{
    public:
        typedef std::vector<std::string>  IStrings;
        typedef std::vector<std::wstring> IwStrings;

        /**
        * Converts string to wstring
        *@param wstr - string to convert
        *@return converted string
        */
        static std::wstring StrToWStr(const std::string& str);

        /**
        * Converts wstring to string
        *@param wstr - string to convert
        *@return converted string
        */
        static std::string WStrToStr(const std::wstring& wstr);

        /**
        * Converts utf8 to utf16
        *@param str - string to convert
        *@return converted string
        */
        static std::wstring UTF8ToUTF16(const std::string& str);

        /**
        * Converts utf16 to utf8
        *@param wstr - string to convert
        *@return converted string
        */
        static std::string UTF16ToUTF8(const std::wstring& wstr);

        /**
        * Fills line
        *@param text - text
        *@param fillChar - fill char
        *@param lineLength - line length
        *@return filled line
        */
        template<class T>
        static std::basic_string<T> Fill(const std::basic_string<T> & text,
                                               T                      fillChar,
                                               QR_SizeT               lineLength);

        /**
        * Trims text (i.e. remove spaces before and after text)
        *@param value - value to trim
        *@return trimmed value
        */
        template<class T>
        static std::basic_string<T> Trim(const std::basic_string<T>& value);

        /**
        * Converts text to lower case
        *@return lowered case text
        */
        template<class T>
        static std::basic_string<T> ToLowerCase(const std::basic_string<T>& text);

        /**
        * Converts text to upper case
        *@return upper case text
        */
        template<class T>
        static std::basic_string<T> ToUpperCase(const std::basic_string<T>& text);

        /**
        * Searches for a string occurrence in another string, and replaces by a replacement string
        *@param str - string to scan
        *@param search - string to search
        *@param replace - string to replace by
        *@param all - if true, all string occurrence will be replaced, only first occurrence if false
        *@return processed string
        */
        template<class T>
        static std::basic_string<T> SearchAndReplace(const std::basic_string<T>& str,
                                                     const std::basic_string<T>& search,
                                                     const std::basic_string<T>& replace,
                                                     bool                        all);

        /**
        * Checks if a digit is an ASCII letter (i.e a letter between a to z or A to Z)
        *@param digit - digit to check
        *@return true if digit is an ASCII letter, otherwise false
        */
        static inline bool IsASCIILetter(char    digit);
        static inline bool IsASCIILetter(wchar_t digit);

        /**
        * Checks if a digit contains only chars that can be converted to number
        *@param digit - digit to check
        *@param strict - if true, only chars from '0' to '9' will be accepted, see note below
        *@return true if digit contains only chars that can be converted to number, otherwise false
        *@note If strict mode is et to false, math symbols as e.g. '.' or '-' will also be accepted
        *      as valid numeric chars. This may be useful when IsNumeric() is used e.g. to determine
        *      if a string can be converted to number
        */
        static inline bool IsNumeric(char    digit, bool strict);
        static inline bool IsNumeric(wchar_t digit, bool strict);

        /**
        * Converts boolean value to string
        *@param value - value to convert
        *@param numeric - if true, result will contains numeric value, otherwise literal value
        *@return converted string
        */
        static std::string  BoolToStr (bool value, bool numeric);
        static std::wstring BoolToWStr(bool value, bool numeric);

        /**
        * Converts string to boolean value
        *@param str - string to convert
        *@return converted value
        */
        static bool StrToBool(const std::string&  str);
        static bool StrToBool(const std::wstring& str);
};
//------------------------------------------------------------------------------
// QR_StringTools - c++ cross-platform
//------------------------------------------------------------------------------
template<class T>
std::basic_string<T> QR_StringTools::Fill(const std::basic_string<T> & text,
                                                T                      fillChar,
                                                QR_SizeT               lineLength)
{
    if (text.length() >= lineLength)
        return text;

    std::basic_string<T> rawLine(lineLength, fillChar);

    QR_SizeT startPos = ((lineLength - text.length()) / 2);

    rawLine.replace(startPos, text.length(), text);
    return rawLine;
}
//------------------------------------------------------------------------------
template<class T>
std::basic_string<T> QR_StringTools::Trim(const std::basic_string<T>& value)
{
    // value is empty?
    if (!value.length())
        return value;

    QR_SizeT startPos = 0;

    // search for first non space char
    for (QR_Int32 i = 0; i < value.length(); ++i)
        if (value[i] != ' ')
        {
            startPos = i;
            break;
        }

    QR_SizeT endPos = value.length();

    // search for last non space char
    for (QR_Int32 i = (QR_Int32)value.length() - 1; i >= 0; --i)
        if (value[i] != ' ')
        {
            endPos = i + 1;
            break;
        }

    // is start and end position identical or invalid?
    if (startPos >= endPos - 1)
        return std::basic_string<T>();

    // return trimmed string
    return value.substr(startPos, endPos - startPos);
}
//------------------------------------------------------------------------------
template<class T>
std::basic_string<T> QR_StringTools::ToLowerCase(const std::basic_string<T>& text)
{
    // is text empty?
    if (text.empty())
        return std::basic_string<T>();

    std::basic_string<T> result;

    // iterate through text chars
    for (QR_SizeT i = 0; i < text.length(); ++i)
        // convert to lower case if needed
        result += std::tolower(text[i]);

    return result;
}
//------------------------------------------------------------------------------
template<class T>
std::basic_string<T> QR_StringTools::ToUpperCase(const std::basic_string<T>& text)
{
    // is text empty?
    if (text.empty())
        return std::basic_string<T>();

    std::basic_string<T> result;

    // iterate through text chars
    for (QR_SizeT i = 0; i < text.length(); ++i)
        // convert to lower case if needed
        result += std::toupper(text[i]);

    return result;
}
//------------------------------------------------------------------------------
template<class T>
std::basic_string<T> QR_StringTools::SearchAndReplace(const std::basic_string<T>& str,
                                                      const std::basic_string<T>& search,
                                                      const std::basic_string<T>& replace,
                                                      bool                        all)
{
    // is source string empty?
    if (str.empty())
        return std::basic_string<T>();

    // is text to search empty?
    if (search.empty())
        return str;

    std::basic_string<T> result = str;

    // begin search on first character
    QR_SizeT position = 0;

    // found search occurrence
    while ((position = result.find(search, position)) != result.npos)
    {
        // replace occurrence
        result.replace(position, search.length(), replace);

        // do replace all occurrences?
        if (!all)
            return result;

        // calculate next search position
        position += replace.length();
    }

    return result;
}
//------------------------------------------------------------------------------
bool QR_StringTools::IsASCIILetter(char digit)
{
    return ((digit >= 'a' && digit <= 'z') || (digit >= 'A' && digit <= 'Z'));
}
//------------------------------------------------------------------------------
bool QR_StringTools::IsASCIILetter(wchar_t digit)
{
    return ((digit >= L'a' && digit <= L'z') || (digit >= L'A' && digit <= L'Z'));
}
//------------------------------------------------------------------------------
bool QR_StringTools::IsNumeric(char digit, bool strict)
{
    return ((digit >= '0' && digit <= '9') || (!strict && (digit == '-' || digit == '.')));
}
//------------------------------------------------------------------------------
bool QR_StringTools::IsNumeric(wchar_t digit, bool strict)
{
    return ((digit >= L'0' && digit <= L'9') || (!strict && (digit == L'-' || digit == L'.')));
}
//------------------------------------------------------------------------------

#endif
