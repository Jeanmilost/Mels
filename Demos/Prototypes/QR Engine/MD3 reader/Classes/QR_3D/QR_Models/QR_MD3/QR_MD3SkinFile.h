/******************************************************************************
 * ==> QR_MD3SkinFile --------------------------------------------------------*
 ******************************************************************************
 * Description : MD3 skin file parser                                         *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_MD3SkinFileH
#define QR_MD3SkinFileH

// std
#include <map>
#include <set>

// qr engine
#include "QR_Script.h"

/**
* MD3 skin file parser
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_MD3SkinFile : public QR_Script
{
    public:
        QR_MD3SkinFile();
        virtual ~QR_MD3SkinFile();

        /**
        * Clears skin configuration
        */
        virtual void Clear();

        /**
        * Gets path from table matching with name
        *@param name - path name to get
        *@return path, empty string if not found or on error
        */
        virtual std::wstring GetPath(const std::wstring& name) const;

    protected:
        typedef std::map<std::wstring, std::wstring> IPathTable;
        typedef std::set<std::wstring>               ILinkKeys;

        IPathTable m_PathTable;
        ILinkKeys  m_LinkKeys;

        /**
        * Called when script line should be parsed
        *@param line - line to parse
        *@param linbeNb - line number
        *@return ture on success, otherwise false
        */
        virtual bool OnParseLine(const std::string& line, QR_SizeT lineNb);
};

#endif
