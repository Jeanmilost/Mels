/*****************************************************************************
 * ==> QR_Version -----------------------------------------------------------*
 * ***************************************************************************
 * Description : Version management class, used e.g. for files, ...          *
 * Developer   : Jean-Milost Reymond                                         *
 *****************************************************************************/

#include "QR_Version.h"

// std
#include <sstream>

//---------------------------------------------------------------------------
//QR_Version - c++ cross-platform
//---------------------------------------------------------------------------
QR_Version::QR_Version() :
    m_Major(0),
    m_Minor(0),
    m_Release(0),
    m_Build(0)
{}
//---------------------------------------------------------------------------
QR_Version::QR_Version(QR_UInt32 major, QR_UInt32 minor, QR_UInt32 release, QR_UInt32 build)
{
    // set version
    SetMajor(major);
    SetMinor(minor);
    SetRelease(release);
    SetBuild(build);
}
//---------------------------------------------------------------------------
QR_Version::~QR_Version()
{}
//---------------------------------------------------------------------------
QR_UInt32 QR_Version::GetMajor() const
{
    return m_Major;
}
//---------------------------------------------------------------------------
void QR_Version::SetMajor(QR_UInt32 value)
{
    m_Major = value;
}
//---------------------------------------------------------------------------
QR_UInt32 QR_Version::GetMinor() const
{
    return m_Minor;
}
//---------------------------------------------------------------------------
void QR_Version::SetMinor(QR_UInt32 value)
{
    m_Minor = value;
}
//---------------------------------------------------------------------------
QR_UInt32 QR_Version::GetRelease() const
{
    return m_Release;
}
//---------------------------------------------------------------------------
void QR_Version::SetRelease(QR_UInt32 value)
{
    m_Release = value;
}
//---------------------------------------------------------------------------
QR_UInt32 QR_Version::GetBuild() const
{
    return m_Build;
}
//---------------------------------------------------------------------------
void QR_Version::SetBuild(QR_UInt32 value)
{
    m_Build = value;
}
//---------------------------------------------------------------------------
std::string QR_Version::ToStr()
{
    std::ostringstream sstr;

    // build version string
    sstr << m_Major << "." << m_Minor << "." << m_Release << "." << m_Build;

    return sstr.str();
}
//---------------------------------------------------------------------------
std::wstring QR_Version::ToWStr()
{
    std::wostringstream sstr;

    // build version string
    sstr << m_Major << L"." << m_Minor << L"." << m_Release << L"." << m_Build;

    return sstr.str();
}
//---------------------------------------------------------------------------

