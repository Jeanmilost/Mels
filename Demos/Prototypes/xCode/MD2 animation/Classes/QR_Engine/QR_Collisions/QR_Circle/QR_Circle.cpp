/******************************************************************************
 * ==> QR_Circle -------------------------------------------------------------*
 ******************************************************************************
 * Description : 2D circle                                                    *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_Circle.h"

// std
#include <memory>
#include <cmath>

// qr engine
#include "QR_Exception.h"

//------------------------------------------------------------------------------
// QR_Circle - c++ cross-platform
//------------------------------------------------------------------------------
QR_Circle::QR_Circle() : QR_CollideShape2D<M_Precision>()
{
    m_Radius = 0.0f;
}
//------------------------------------------------------------------------------
QR_Circle::QR_Circle(const QR_Vector2DP& center, const M_Precision& radius) :
    QR_CollideShape2D<M_Precision>()
{
    m_Center = center;
    m_Radius = radius;
}
//------------------------------------------------------------------------------
QR_Circle::~QR_Circle()
{}
//------------------------------------------------------------------------------
bool QR_Circle::Inside(const M_Precision& x, const M_Precision& y) const
{
    return Inside(QR_Vector2DP(x, y));
}
//------------------------------------------------------------------------------
bool QR_Circle::Inside(const QR_Vector2DP& point) const
{
    // point is inside circle if distance between point and circle center is
    // smaller than radius
    return (QR_Vector2DP(std::fabs(m_Center.m_X - point.m_X),
                         std::fabs(m_Center.m_Y - point.m_Y)).Length() <= m_Radius);
}
//------------------------------------------------------------------------------
bool QR_Circle::Intersect(const QR_CollideShape2D<M_Precision>* pOther) const
{
    const QR_Circle* pCircle = dynamic_cast<const QR_Circle*>(pOther);

    if (!pCircle)
        M_THROW_EXCEPTION("Could not convert other object to circle");

    // circles are in collision if distance between centers is smaller than sum
    // of radius
    return (QR_Vector2DP(std::fabs(m_Center.m_X - pCircle->m_Center.m_X),
                         std::fabs(m_Center.m_Y - pCircle->m_Center.m_Y)).Length()
                                 <= (m_Radius + pCircle->m_Radius));
}
//------------------------------------------------------------------------------

