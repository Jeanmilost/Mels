/******************************************************************************
 * ==> QR_MD3SkinFile --------------------------------------------------------*
 ******************************************************************************
 * Description : MD3 skin file parser                                         *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_MD3SkinFile.h"

//---------------------------------------------------------------------------
// QR_MD3SkinFile - c++ cross-platform
//---------------------------------------------------------------------------
QR_MD3SkinFile::QR_MD3SkinFile() : QR_Script()
{}
//---------------------------------------------------------------------------
QR_MD3SkinFile::~QR_MD3SkinFile()
{}
//---------------------------------------------------------------------------
void QR_MD3SkinFile::Clear()
{
    m_PathTable.clear();
    m_LinkKeys.clear();
}
//---------------------------------------------------------------------------
std::wstring QR_MD3SkinFile::GetPath(const std::wstring& name) const
{
    // search for name in table
    IPathTable::const_iterator it = m_PathTable.find(name);

    // found it?
    if (it == m_PathTable.end())
        return L"";

    return it->second;
}
//---------------------------------------------------------------------------
bool QR_MD3SkinFile::OnParseLine(const std::string& line, QR_SizeT lineNb)
{
    if (line.empty())
        return true;

    std::wstring name;
    std::wstring path;
    bool         readPath = false;

    // iterate through line chars
    for (QR_SizeT i = 0; i < line.length(); ++i)
        // dispatch char
        switch (line[i])
        {
            case ',':
                // found separator, from now path should be read
                readPath = true;
                continue;

            default:
                // is reading path?
                if (readPath)
                {
                    // add char to path
                    path += line[i];
                    continue;
                }

                // add char to name
                name += line[i];
                continue;
        }

    // empty name?
    if (name.empty())
        return false;

    // no path?
    if (path.empty())
    {
        // in this case, the line contains a link key
        ILinkKeys::iterator it = m_LinkKeys.find(name);

        // already registered?
        if (it != m_LinkKeys.end())
            return false;

        // add link key to table
        m_LinkKeys.insert(name);
        return true;
    }

    // search for another existing name in table
    IPathTable::iterator it = m_PathTable.find(name);

    // name already exists in table?
    if (it != m_PathTable.end())
        return false;

    // add name and path in table
    m_PathTable[name] = path;
    return true;
}
//---------------------------------------------------------------------------
