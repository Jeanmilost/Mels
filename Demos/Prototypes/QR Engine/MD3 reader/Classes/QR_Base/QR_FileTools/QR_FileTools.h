/*****************************************************************************
 * ==> QR_FileTools ---------------------------------------------------------*
 * ***************************************************************************
 * Description : Most common file and folder tools                           *
 * Developer   : Jean-Milost Reymond                                         *
 *****************************************************************************/

#ifndef QR_FileToolsH
#define QR_FileToolsH

// std
#include <string>

// qr engine
#include "QR_Types.h"
#include "QR_StringTools.h"
#include "QR_Version.h"

#ifdef _WIN32
    #define M_DirDelim   '\\'
    #define M_DirDelimW L'\\'
#elif defined (__APPLE__)
#else
    #define M_DirDelim   '/'
    #define M_DirDelimW L'/'
#endif

/**
* File and folder tools
*@author Jean-Milost Reymond
*/
class QR_FileTools
{
    public:
        /**
        * Gets Windows desktop directory
        *@return desktop directory, empty string on error
        */
        static std::wstring GetDesktopDir();

        /**
        * Gets running application name and dir
        *@return running application name and dir
        */
        static std::string  GetAppName();
        static std::wstring GetAppNameW();

        /**
        * Gets running application version
        *@return running application version, empty version on error
        */
        static QR_Version GetAppVersion();

        /**
        * Gets file directory from a full path
        *@param fullPath - full path containing file dir and name
        *@param keepLastChar - if true, last delimiter will be kept
        *@param delimiter - dir delimiter
        *@return directory, empty string on error
        */
        static std::string  GetFileDir(const std::string&  fullPath, bool keepLastChar, QR_Char  delimiter = M_DirDelim);
        static std::wstring GetFileDir(const std::wstring& fullPath, bool keepLastChar, QR_WChar delimiter = M_DirDelimW);

        /**
        * Gets file name from a full path
        *@param fullPath - full path containing file dir and name
        *@param delimiter - dir delimiter
        *@return file name
        */
        static std::string  GetFileName(const std::string&  fullPath, QR_Char  delimiter = M_DirDelim);
        static std::wstring GetFileName(const std::wstring& fullPath, QR_WChar delimiter = M_DirDelimW);

        /**
        * Gets file extension from a full path
        *@param fullPath - full path containing file dir and name
        *@return file extension
        */
        static std::string  GetFileExtension(const std::string&  fullPath);
        static std::wstring GetFileExtension(const std::wstring& fullPath);

        /**
        * Appends delimiter at dir end
        *@param dirName - directory name to append to
        *@return appended dir name
        *@note If delimiter already exists at end, it will not be appended again
        */
        static std::string  AppendDelimiter(const std::string&  dirName, QR_Char  delimiter = M_DirDelim);
        static std::wstring AppendDelimiter(const std::wstring& dirName, QR_WChar delimiter = M_DirDelimW);

        /**
        * Checks if a file exists
        *@param fileName - file name to check
        *@returns true if file exists, otherwise false
        */
        static bool FileExists(const std::string&  name);
        static bool FileExists(const std::wstring& name);

        /**
        * Gets directory content
        *@param dir - dir name for which content should be get
        *@param[out] content - dir content
        *@param fullNames - if true, items will contain full names
        *@return true on success, otherwise false
        *@note if fullNames is set, special "." and ".." items will be skipped
        */
        static bool GetDirContent(const std::string& dir,  QR_StringTools::IStrings&  content, bool fullNames);
        static bool GetDirContent(const std::wstring& dir, QR_StringTools::IwStrings& content, bool fullNames);

        /**
        * Loads data from file
        *@param fileName - file name
        *@param[out] data - file content
        *@return true on success, otherwise false
        */
        static bool Load(const std::string& fileName, std::string& data);

        /**
        * Saves data to file
        *@param fileName - file name
        *@param data - file content
        *@param clear - if true, file will be rewritten completely
        *@return true on success, otherwise false
        */
        static bool Save(const std::string& fileName, const std::string& data, bool clear);
};
#endif
