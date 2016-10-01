/*****************************************************************************
 * ==> QR_Version -----------------------------------------------------------*
 * ***************************************************************************
 * Description : Version management class, used e.g. for files, ...          *
 * Developer   : Jean-Milost Reymond                                         *
 *****************************************************************************/

#ifndef QR_VersionH
#define QR_VersionH

// std
#include <string>

// qr engine
#include "QR_Types.h"

/**
* Version management class
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_Version
{
    public:
        /**
        * Constructor
        */
        QR_Version();

        /**
        * Constructor
        *@param major - major version
        *@param minor - minor version
        *@param release - release version
        *@param build - build version
        */
        QR_Version(QR_UInt32 major, QR_UInt32 minor, QR_UInt32 release, QR_UInt32 build);

        virtual ~QR_Version();

        /**
        * Gets version major component
        *@return major version
        */
        virtual QR_UInt32 GetMajor() const;

        /**
        * Sets version major component
        *@param value - major version
        */
        virtual void SetMajor(QR_UInt32 value);

        /**
        * Gets version minor component
        *@return version minor component
        */
        virtual QR_UInt32 GetMinor() const;

        /**
        * Sets version minor component
        *@param value - version minor component
        */
        virtual void SetMinor(QR_UInt32 value);

        /**
        * Gets version release component
        *@return version release component
        */
        virtual QR_UInt32 GetRelease() const;

        /**
        * Sets version release component
        *@param value - version release component
        */
        virtual void SetRelease(QR_UInt32 value);

        /**
        * Gets version build component
        *@return version build component
        */
        virtual QR_UInt32 GetBuild() const;

        /**
        * Sets version build component
        *@param value - version build component
        */
        virtual void SetBuild(QR_UInt32 value);

        /**
        * Gets version as string
        *@return version as string
        */
        virtual std::string  ToStr();
        virtual std::wstring ToWStr();

    private:
        QR_UInt32 m_Major;
        QR_UInt32 m_Minor;
        QR_UInt32 m_Release;
        QR_UInt32 m_Build;
};
#endif
