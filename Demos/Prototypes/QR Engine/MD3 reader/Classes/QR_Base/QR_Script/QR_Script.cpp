/******************************************************************************
 * ==> QR_Script -------------------------------------------------------------*
 ******************************************************************************
 * Description : Class that provides tools to read and write generic scripts  *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_Script.h"

// qr engine
#include "QR_MemoryTools.h"
#include "QR_StringTools.h"
#include "QR_FileTools.h"
#include "QR_StdFileBuffer.h"

//---------------------------------------------------------------------------
// QR_Script - c++  cross-platform
//---------------------------------------------------------------------------
QR_Script::QR_Script()
{}
//---------------------------------------------------------------------------
QR_Script::~QR_Script()
{}
//---------------------------------------------------------------------------
bool QR_Script::Load(const std::string& fileName)
{
    // script file exists?
    if (!QR_FileTools::FileExists(fileName))
        return false;

    QR_StdFileBuffer fileBuffer;

    // open script file
    if (!fileBuffer.Open(fileName, QR_FileBuffer::IE_M_Read))
        return false;

    fileBuffer.Seek(0, 0);

    // load script from file buffer
    return Load(fileBuffer, fileBuffer.GetSize());
}
//---------------------------------------------------------------------------
bool QR_Script::Load(const std::wstring& fileName)
{
    // script file exists?
    if (!QR_FileTools::FileExists(fileName))
        return false;

    QR_StdFileBuffer fileBuffer;

    // open script file
    if (!fileBuffer.Open(fileName, QR_FileBuffer::IE_M_Read))
        return false;

    fileBuffer.Seek(0, 0);

    // load script from file buffer
    return Load(fileBuffer, fileBuffer.GetSize());
}
//---------------------------------------------------------------------------
bool QR_Script::Load(QR_Buffer& buffer, QR_Buffer::ISizeType length)
{
    // is buffer empty?
    if (!buffer.GetSize())
        return true;

    // get current script offset
    QR_Buffer::ISizeType offset = buffer.GetOffset();

    // read file bom
    QR_UInt8 bom[3];
    buffer.Read(bom, sizeof(bom));

    // check if file contains a UTF8 or UTF16 bom
    if (bom[0] == 0xEF && bom[1] == 0xBB && bom[2] == 0xBF)
        // found UTF8 bom, parse data from current offset
        return Parse(buffer, length);
    else
    if (bom[0] == 0xFF && bom[1] == 0xFE)
    {
        // found UTF16 little endian bom, skip it and parse data using wide chars parser
        buffer.Seek(0, offset + 2);
        return ParseW(buffer, length, false);
    }
    else
    if (bom[0] == 0xFE && bom[1] == 0xFF)
    {
        // found UTF16 big endian bom, skip it and parse data using wide chars parser
        buffer.Seek(0, offset + 2);
        return ParseW(buffer, length, true);
    }

    // no bom found, reset cursor to beginning and parse data
    buffer.Seek(0, offset);
    return Parse(buffer, length);
}
//---------------------------------------------------------------------------
bool QR_Script::Parse(QR_Buffer& buffer, QR_Buffer::ISizeType length)
{
    // clear all previous data before parsing new
    Clear();

    const QR_Buffer::ISizeType offset = buffer.GetOffset();
          QR_SizeT             lineNb = 0;
          std::string          line;

    // iterate through data to read
    while (!EndReached(buffer, offset, length))
    {
        // read next char
        char data;
        buffer.Read(&data, 1);

        // dispatch char
        switch (data)
        {
            case '\0':
                // found end of line, break all
                break;

            case '\r':
            case '\n':
                // found line feed/carriage return, parse line if not empty
                if (!line.empty())
                {
                    if (!ParseLine(line, lineNb))
                        return false;

                    line.clear();
                }

                // end of file reached?
                if (EndReached(buffer, offset, length))
                    ++lineNb;
                else
                {
                    std::string crlf;

                    // skip all next crlf chars
                    while (!EndReached(buffer, offset, length))
                    {
                        // not a crlf char?
                        if (data != '\r' && data != '\n')
                        {
                            // add char to next line and break the loop
                            line += data;
                            break;
                        }

                        // get char in special crlf string and read next char
                        crlf += data;
                        buffer.Read(&data, 1);
                    }

                    // happen when file end was reached and the last read char was \r or \n
                    if (data == '\r' || data == '\n')
                        crlf += data;

                    // simplify the string thus all crlf or lfcr will become a simple cr char
                    crlf = QR_StringTools::SearchAndReplace<char>(crlf, "\r\n", "\n", true);
                    crlf = QR_StringTools::SearchAndReplace<char>(crlf, "\n\r", "\n", true);

                    // iterate through carriage returns
                    for (QR_SizeT i = 0; i < crlf.length(); ++i)
                    {
                        // increment line count
                        ++lineNb;

                        // found an empty line?
                        if (i < crlf.length() - 1)
                            // give the opportunity to parse the empty line
                            if (!ParseLine("", lineNb))
                                return false;
                    }

                    // if end was reached and last chars were crlf, parse last empty line, because
                    // ParseLine will no more be called after
                    if (EndReached(buffer, offset, length) && crlf.length())
                        if (!ParseLine("", lineNb))
                            return false;
                }

                continue;

            default:
                // copy char to line to parse
                line += data;
                continue;
        }
    }

    // last line to parse?
    if (!line.empty())
    {
        if (!ParseLine(line, lineNb))
            return false;

        line.clear();
    }

    return true;
}
//---------------------------------------------------------------------------
bool QR_Script::ParseW(QR_Buffer& buffer, QR_Buffer::ISizeType length, bool bigEndian)
{
    // clear all previous data before parsing new
    Clear();

    // check if system is big or little endian
    const bool isSystemBE = QR_MemoryTools::IsSystemBE();

    const QR_Buffer::ISizeType offset = buffer.GetOffset();
          QR_SizeT             lineNb = 0;
          std::wstring         line;

    // iterate through data to read
    while (!EndReached(buffer, offset, length))
    {
        wchar_t data = ReadWChar(buffer, bigEndian, isSystemBE);

        // dispatch char
        switch (data)
        {
            case L'\0':
                // found end of line, break all
                break;

            case L'\r':
            case L'\n':
                // found line feed/carriage return, parse line if not empty
                if (!line.empty())
                {
                    if (!ParseLine(line, lineNb))
                        return false;

                    line.clear();
                }

                // end of file reached?
                if (EndReached(buffer, offset, length))
                    ++lineNb;
                else
                {
                    std::wstring crlf;

                    // skip all next crlf chars
                    while (!EndReached(buffer, offset, length))
                    {
                        // not a crlf char?
                        if (data != L'\r' && data != L'\n')
                        {
                            // add char to next line and break the loop
                            line += data;
                            break;
                        }

                        // get char in special crlf string and read next char
                        crlf += data;
                        data = ReadWChar(buffer, bigEndian, isSystemBE);
                    }

                    // happen when file end was reached and the last read char was \r or \n
                    if (data == L'\r' || data == L'\n')
                        crlf += data;

                    // simplify the string thus all crlf or lfcr will become a simple cr char
                    crlf = QR_StringTools::SearchAndReplace<wchar_t>(crlf, L"\r\n", L"\n", true);
                    crlf = QR_StringTools::SearchAndReplace<wchar_t>(crlf, L"\n\r", L"\n", true);

                    // iterate through carriage returns
                    for (QR_SizeT i = 0; i < crlf.length(); ++i)
                    {
                        // increment line count
                        ++lineNb;

                        // found an empty line?
                        if (i < crlf.length() - 1)
                            // give the opportunity to parse the empty line
                            if (!ParseLine(L"", lineNb))
                                return false;
                    }

                    // if end was reached and last chars were crlf, parse last empty line, because
                    // ParseLine will no more be called after
                    if (EndReached(buffer, offset, length) && crlf.length())
                        if (!ParseLine(L"", lineNb))
                            return false;
                }

                continue;

            default:
                // copy char to line to parse
                line += data;
                continue;
        }
    }

    // last line to parse?
    if (!line.empty())
    {
        if (!ParseLine(line, lineNb))
            return false;

        line.clear();
    }

    return true;
}
//---------------------------------------------------------------------------
bool QR_Script::ParseLine(const std::string& line, QR_SizeT lineNb)
{
    return OnParseLine(line, lineNb);
}
//---------------------------------------------------------------------------
bool QR_Script::ParseLine(const std::wstring& line, QR_SizeT lineNb)
{
    return OnParseLine(QR_StringTools::UTF16ToUTF8(line), lineNb);
}
//---------------------------------------------------------------------------
wchar_t QR_Script::ReadWChar(QR_Buffer& buffer, bool isTextBE, bool isSystemBE) const
{
    wchar_t wChar;

    // read next char
    buffer.Read(&wChar, sizeof(wChar));

    // do swap endianess?
    if (isTextBE == isSystemBE)
        return wChar;

    // swap char endianess and return result
    return (((wChar & 0xFF) << 8) + ((wChar & 0xFF00) >> 8));
}
//---------------------------------------------------------------------------
bool QR_Script::EndReached(QR_Buffer&           buffer,
                           QR_Buffer::ISizeType start,
                           QR_Buffer::ISizeType length) const
{
    // check if end of file is reached or length to read is completed
    return (buffer.GetOffset() >= buffer.GetSize() || (buffer.GetOffset() - start) >= length);
}
//---------------------------------------------------------------------------
