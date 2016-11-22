/*****************************************************************************
 * ==> QR_Shader ------------------------------------------------------------*
 *****************************************************************************
 * Description : Basic shader language class                                 *
 * Developer   : Jean-Milost Reymond                                         *
 *****************************************************************************/

#include "QR_Shader.h"

//------------------------------------------------------------------------------
// class QR_Shader - c++ cross-platform
//------------------------------------------------------------------------------
QR_Shader::QR_Shader()
{
    PopulateAttributeDict();
}
//------------------------------------------------------------------------------
QR_Shader::~QR_Shader()
{}
//------------------------------------------------------------------------------
std::string QR_Shader::GetAttributeName(IEAttribute attribute) const
{
    // search for attribute to get name
    IAttributeDictionary::const_iterator it = m_AttributeDictionary.find(attribute);

    // found it?
    if (it == m_AttributeDictionary.end())
        return "";

    return it->second;
}
//------------------------------------------------------------------------------
void QR_Shader::SetAttributeName(      IEAttribute  attribute,
                                 const std::string& name)
{
    // is name empty?
    if (!name.length())
        return;

    // change attribute name
    m_AttributeDictionary[attribute] = name;
}
//------------------------------------------------------------------------------
void QR_Shader::PopulateAttributeDict()
{
    m_AttributeDictionary[IE_SA_Position]            = "qr_vPosition";
    m_AttributeDictionary[IE_SA_Normal]              = "qr_vNormal";
    m_AttributeDictionary[IE_SA_Texture]             = "qr_vTexCoord";
    m_AttributeDictionary[IE_SA_Color]               = "qr_vColor";
    m_AttributeDictionary[IE_SA_PerspectiveMatrix]   = "qr_uPerspective";
    m_AttributeDictionary[IE_SA_ProjectionMatrix]    = "qr_uProjection";
    m_AttributeDictionary[IE_SA_ViewMatrix]          = "qr_uViewMatrix";
    m_AttributeDictionary[IE_SA_CameraMatrix]        = "qr_uCamera";
    m_AttributeDictionary[IE_SA_ModelMatrix]         = "qr_uModel";
    m_AttributeDictionary[IE_SA_Interpolation]       = "qr_fInterpolation";
    m_AttributeDictionary[IE_SA_InterpolationPos]    = "qr_viPosition";
    m_AttributeDictionary[IE_SA_InterpolationNormal] = "qr_viNormal";
}
//------------------------------------------------------------------------------
