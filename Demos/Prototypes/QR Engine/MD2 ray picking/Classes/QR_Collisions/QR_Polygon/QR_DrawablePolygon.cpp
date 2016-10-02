/******************************************************************************
 * ==> QR_DrawablePolygon ----------------------------------------------------*
 ******************************************************************************
 * Description : Polygon that can be drawn                                    *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_DrawablePolygon.h"

//------------------------------------------------------------------------------
// QR_DrawablePolygon - c++ cross-platform
//------------------------------------------------------------------------------
QR_DrawablePolygon::QR_DrawablePolygon() : m_pPolygon(NULL)
{}
//------------------------------------------------------------------------------
QR_DrawablePolygon::QR_DrawablePolygon(QR_Polygon* pPolygon) : m_pPolygon(pPolygon)
{}
//------------------------------------------------------------------------------
QR_DrawablePolygon::~QR_DrawablePolygon()
{}
//------------------------------------------------------------------------------
QR_Polygon* QR_DrawablePolygon::Get() const
{
    return m_pPolygon;
}
//------------------------------------------------------------------------------
void QR_DrawablePolygon::Set(QR_Polygon* pPolygon)
{
    m_pPolygon = pPolygon;
}
//------------------------------------------------------------------------------
const QR_Color* QR_DrawablePolygon::GetVertexColor(QR_SizeT index) const
{
    // search for index to get
    switch (index)
    {
        case 0:  return &m_Colors[0];
        case 1:  return &m_Colors[1];
        case 2:  return &m_Colors[2];
        default: return NULL;
    }
}
//------------------------------------------------------------------------------
void QR_DrawablePolygon::SetVertexColor(QR_SizeT index, const QR_Color& color)
{
    // search for index to set
    switch (index)
    {
        case 0:  m_Colors[0] = color; return;
        case 1:  m_Colors[1] = color; return;
        case 2:  m_Colors[2] = color; return;
    }
}
//------------------------------------------------------------------------------
