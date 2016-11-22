/******************************************************************************
 * ==> QR_Rectangle ----------------------------------------------------------*
 ******************************************************************************
 * Description : 2D rectangle                                                 *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_Rectangle.h"

// std
#include <memory>

// qr engine
#include "QR_Exception.h"

//------------------------------------------------------------------------------
// QR_Rectangle - c++ cross-platform
//------------------------------------------------------------------------------
QR_Rectangle::QR_Rectangle() : QR_CollideShape2D<M_Precision>()
{}
//------------------------------------------------------------------------------
QR_Rectangle::QR_Rectangle(const QR_Vector2DP& min, const QR_Vector2DP& max) :
    QR_CollideShape2D<M_Precision>()
{
    m_Min = min;
    m_Max = max;
}
//------------------------------------------------------------------------------
QR_Rectangle::~QR_Rectangle()
{}
//------------------------------------------------------------------------------
bool QR_Rectangle::Inside(const M_Precision& x, const M_Precision& y) const
{
    return Inside(QR_Vector2DP(x, y));
}
//------------------------------------------------------------------------------
bool QR_Rectangle::Inside(const QR_Vector2DP& point) const
{
    return (point.m_X >=  m_Min.m_X                            &&
            point.m_Y >=  m_Min.m_Y                            &&
            point.m_X <= (m_Min.m_X + (m_Max.m_X - m_Min.m_X)) &&
            point.m_Y <= (m_Min.m_Y + (m_Max.m_Y - m_Min.m_Y)));
}
//------------------------------------------------------------------------------
bool QR_Rectangle::Intersect(const QR_CollideShape2D<M_Precision>* pOther) const
{
    const QR_Rectangle* pRect = dynamic_cast<const QR_Rectangle*>(pOther);

    if (!pRect)
        M_THROW_EXCEPTION("Could not convert other object to rectangle");

    return !(m_Min.m_X > pRect->m_Max.m_X || m_Max.m_X < pRect->m_Min.m_X ||
             m_Min.m_Y > pRect->m_Max.m_Y || m_Max.m_Y < pRect->m_Min.m_Y);
}
//------------------------------------------------------------------------------
