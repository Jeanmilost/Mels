/*****************************************************************************
 * ==> QR_FileTools ---------------------------------------------------------*
 * ***************************************************************************
 * Description : Most common file and folder tools                           *
 * Developer   : Jean-Milost Reymond                                         *
 *****************************************************************************/

#include "QR_FileTools.h"

// std
#include <stdexcept>
#include <cstdio>

#ifdef _WIN32
    // needed for some std::memset implementation, e.g. in CodeBlocks where
    // compiler will not found it otherwise
    #include <cstring>
#endif

// libraries
#ifdef _WIN32
    #ifndef UNICODE
    #define UNICODE
    #endif // UNICODE

    #include <windows.h>
    #include <Shlobj.h>
#endif

//---------------------------------------------------------------------------
std::wstring QR_FileTools::GetDesktopDir()
{
    #ifdef _WIN32
        #if (_WIN32_IE >= 0x0500)
            TCHAR desktop[MAX_PATH];
            std::memset(&desktop, 0, MAX_PATH);

            // get Windows desktop path
            if (SUCCEEDED(::SHGetFolderPath(NULL,
                                            CSIDL_DESKTOP,
                                            NULL,
                                            SHGFP_TYPE_CURRENT,
                                            desktop)))
                return desktop;

            return L"";
        #else
            throw std::runtime_error(std::string("QR_FileTools::GetAppName function is not supported"));
        #endif
    #elif defined (__APPLE__)
        throw std::runtime_error("NOT IMPLEMENTED");
    #else
        #error "QR_FileTools::GetDesktopDir function is not implemented for this platform"
    #endif
}
//---------------------------------------------------------------------------
std::string QR_FileTools::GetAppName()
{
    #ifdef _WIN32
        QR_Char result[MAX_PATH];

        return std::string(result, ::GetModuleFileNameA(NULL, result, MAX_PATH));
    #elif defined (__APPLE__)
        throw std::runtime_error("NOT IMPLEMENTED");
    #else
        #error "QR_FileTools::GetAppName function is not implemented for this platform"
    #endif
}
//---------------------------------------------------------------------------
std::wstring QR_FileTools::GetAppNameW()
{
    #ifdef _WIN32
        QR_WChar result[MAX_PATH];

        return std::wstring(result, ::GetModuleFileNameW(NULL, result, MAX_PATH));
    #elif defined (__APPLE__)
        throw std::runtime_error("NOT IMPLEMENTED");
    #else
        #error "QR_FileTools::GetAppNameW function is not implemented for this platform"
    #endif
}
//---------------------------------------------------------------------------
QR_Version QR_FileTools::GetAppVersion()
{
    #ifdef _WIN32
        // get application file name and dir
        std::wstring fileName = GetAppNameW();

        BYTE* pVersionInfo = NULL;

        try
        {
            // get version info size and create version info container
            DWORD hHandle = 0;
            DWORD size    = ::GetFileVersionInfoSize(fileName.c_str(), &hHandle);
            pVersionInfo  = new BYTE[size];

            // get version info
            if (!::GetFileVersionInfo(fileName.c_str(), hHandle, size, pVersionInfo))
                // return empty version if failed
                return QR_Version();

            UINT              len   = 0;
            VS_FIXEDFILEINFO* pVsfi = NULL;

            // get file version
            ::VerQueryValue(pVersionInfo, L"\\", (void**)&pVsfi, &len);

            // convert file version to QR_Version and returns result
            QR_Version version;
            version.SetMajor(HIWORD(pVsfi->dwFileVersionMS));
            version.SetMinor(LOWORD(pVsfi->dwFileVersionMS));
            version.SetRelease(HIWORD(pVsfi->dwFileVersionLS));
            version.SetBuild(LOWORD(pVsfi->dwFileVersionLS));

            // delete version info, if needed
            if (pVersionInfo)
                delete[] pVersionInfo;

            return version;
        }
        catch (...)
        {}

        // delete version info, if needed
        if (pVersionInfo)
            delete[] pVersionInfo;

        return QR_Version();
    #elif defined (__APPLE__)
        throw std::runtime_error("NOT IMPLEMENTED");
    #else
        #error "QR_FileTools::GetAppVersion function is not implemented for this platform"
    #endif
}
//---------------------------------------------------------------------------
std::string QR_FileTools::GetFileDir(const std::string& fullPath, bool keepLastChar, QR_Char delimiter)
{
    // is source empty?
    if (fullPath.empty())
        return "";

    QR_SizeT position = 0;

    // iterate through full name chars
    for (QR_Int32 i = fullPath.length() - 1; i >= 0; --i)
        // found dir delimiter?
        if (fullPath[i] == delimiter)
        {
            // keep position
            position = i;
            break;
        }

    // is position valid?
    if (!position || (position + 1) >= fullPath.length())
        return "";

    // extract file dir
    std::string fileDir = fullPath.substr(0, (keepLastChar ? position + 1 : position));

    // do keep last char?
    if (keepLastChar)
        // check if last char exists, append if not
        return AppendDelimiter(fileDir, delimiter);

    return fileDir;
}
//---------------------------------------------------------------------------
std::wstring QR_FileTools::GetFileDir(const std::wstring& fullPath, bool keepLastChar, QR_WChar delimiter)
{
    // is source empty?
    if (fullPath.empty())
        return L"";

    QR_SizeT position = 0;

    // iterate through full name chars
    for (QR_Int32 i = fullPath.length() - 1; i >= 0; --i)
        // found dir delimiter?
        if (fullPath[i] == delimiter)
        {
            // keep position
            position = i;
            break;
        }

    // is position valid?
    if (!position || (position + 1) >= fullPath.length())
        return L"";

    // extract file dir
    std::wstring fileDir = fullPath.substr(0, (keepLastChar ? position + 1 : position));

    // do keep last char?
    if (keepLastChar)
        // check if last char exists, append if not
        return AppendDelimiter(fileDir, delimiter);

    return fileDir;
}
//---------------------------------------------------------------------------
std::string QR_FileTools::GetFileName(const std::string& fullPath, QR_Char delimiter)
{
    // is source empty?
    if (fullPath.empty())
        return "";

    QR_SizeT position = 0;

    // iterate through full name chars
    for (QR_Int32 i = fullPath.length() - 1; i >= 0; --i)
        // found dir delimiter?
        if (fullPath[i] == delimiter)
        {
            // keep position
            position = i;
            break;
        }

    // is position valid?
    if (!position || (position + 1) >= fullPath.length())
        return fullPath;

    // extract file dir
    return fullPath.substr(position + 1, fullPath.length() - (position + 1));
}
//---------------------------------------------------------------------------
std::wstring QR_FileTools::GetFileName(const std::wstring& fullPath, QR_WChar delimiter)
{
    // is source empty?
    if (fullPath.empty())
        return L"";

    QR_SizeT position = 0;

    // iterate through full name chars
    for (QR_Int32 i = fullPath.length() - 1; i >= 0; --i)
        // found dir delimiter?
        if (fullPath[i] == delimiter)
        {
            // keep position
            position = i;
            break;
        }

    // is position valid?
    if (!position || (position + 1) >= fullPath.length())
        return fullPath;

    // extract file dir
    return fullPath.substr(position + 1, fullPath.length() - (position + 1));
}
//---------------------------------------------------------------------------
std::string  QR_FileTools::GetFileExtension(const std::string& fullPath)
{
    // is source empty?
    if (fullPath.empty())
        return "";

    QR_SizeT position = 0;

    // iterate through full name chars
    for (QR_Int32 i = fullPath.length() - 1; i >= 0; --i)
        // found extension delimiter?
        if (fullPath[i] == '.')
        {
            // keep position
            position = i;
            break;
        }

    // is position valid?
    if (!position || (position + 1) >= fullPath.length())
        return fullPath;

    // extract file dir
    return fullPath.substr(position + 1, fullPath.length() - (position + 1));
}
//---------------------------------------------------------------------------
std::wstring QR_FileTools::GetFileExtension(const std::wstring& fullPath)
{
    // is source empty?
    if (fullPath.empty())
        return L"";

    QR_SizeT position = 0;

    // iterate through full name chars
    for (QR_Int32 i = fullPath.length() - 1; i >= 0; --i)
        // found extension delimiter?
        if (fullPath[i] == L'.')
        {
            // keep position
            position = i;
            break;
        }

    // is position valid?
    if (!position || (position + 1) >= fullPath.length())
        return fullPath;

    // extract file dir
    return fullPath.substr(position + 1, fullPath.length() - (position + 1));
}
//---------------------------------------------------------------------------
std::string QR_FileTools::AppendDelimiter(const std::string& dirName, QR_Char delimiter)
{
    // is dir name empty?
    if (dirName.empty())
        return std::string(1, delimiter);

    // dir name already contains delimiter at end?
    if (dirName[dirName.length() - 1] == delimiter)
        return dirName;

    // append delimiter
    return dirName + delimiter;
}
//---------------------------------------------------------------------------
std::wstring QR_FileTools::AppendDelimiter(const std::wstring& dirName, QR_WChar delimiter)
{
    // is dir name empty?
    if (dirName.empty())
        return std::wstring(1, delimiter);

    // dir name already contains delimiter at end?
    if (dirName[dirName.length() - 1] == delimiter)
        return dirName;

    // append delimiter
    return dirName + delimiter;
}
//---------------------------------------------------------------------------
bool QR_FileTools::FileExists(const std::string& name)
{
    // try to open file
    if (std::FILE* pFile = std::fopen(name.c_str(), "r"))
    {
        // succeeded?
        std::fclose(pFile);
        return true;
    }

    return false;
}
//---------------------------------------------------------------------------
bool QR_FileTools::Load(const std::string& fileName, std::string& data)
{
    // no file name?
    if (fileName.empty())
        return false;

    QR_Char*   pBuffer    = NULL;
    std::FILE* pStream    = NULL;
    QR_SizeT   fileSize   = 0;
    QR_SizeT   bufferSize = 0;
    bool       success    = true;

    try
    {
        // open file for read
        pStream = std::fopen(fileName.c_str(), "rb");

        // is file stream opened?
        if (!pStream)
            return false;

        // get file size
        std::fseek(pStream, 0, SEEK_END);
        fileSize = std::ftell(pStream);
        std::fseek(pStream, 0, SEEK_SET);

        // copy file content to buffer
        pBuffer    = new QR_Char[fileSize + 1];
        bufferSize = std::fread(pBuffer, 1, fileSize, pStream);
    }
    catch (...)
    {
        success = false;
    }

    // close cached xml file
    if (pStream)
        std::fclose(pStream);

    try
    {
        // file read succeeded?
        if (success)
            // add readed data to output
            data += std::string((const QR_Char*)pBuffer, bufferSize);
    }
    catch (...)
    {
        success = false;
    }

    // delete buffer, if needed
    if (pBuffer)
        delete[] pBuffer;

    return (success && (bufferSize == fileSize));
}
//---------------------------------------------------------------------------
bool QR_FileTools::Save(const std::string& fileName, const std::string& data, bool clear)
{
    // no file name?
    if (fileName.empty())
        return false;

    std::FILE* pStream = NULL;
    bool       success = true;

    try
    {
        // open file for write (create new file if needed, overwrite or append otherwise)
        pStream = std::fopen(fileName.c_str(), (clear ? "wb" : "ab"));

        // is file stream opened?
        if (!pStream)
            return false;

        // write file content
        if (!data.empty())
            std::fwrite(data.c_str(), data.length(), 1, pStream);
    }
    catch (...)
    {
        success = false;
    }

    // close cached xml file
    if (pStream)
        std::fclose(pStream);

    return success;
}
//---------------------------------------------------------------------------

