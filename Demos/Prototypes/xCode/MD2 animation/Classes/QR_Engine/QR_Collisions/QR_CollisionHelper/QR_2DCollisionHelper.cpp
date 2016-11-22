/******************************************************************************
 * ==> QR_2DCollisionHelper --------------------------------------------------*
 ******************************************************************************
 * Description : Helper for collision detection in 2D world                   *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_2DCollisionHelper.h"

//------------------------------------------------------------------------------
// Class QR_2DCollisionHelper - c++ cross-platform
//------------------------------------------------------------------------------
QR_2DCollisionHelper::QR_2DCollisionHelper()
{}
//------------------------------------------------------------------------------
QR_2DCollisionHelper::~QR_2DCollisionHelper()
{}
//------------------------------------------------------------------------------
bool QR_2DCollisionHelper::GetRectCircleCollision(const QR_Rectangle& rect,
                                                  const QR_Circle&    circle)
{
    if (circle.m_Center.m_Y >= rect.m_Min.m_Y && circle.m_Center.m_Y <= rect.m_Max.m_Y)
        return !((circle.m_Center.m_X + circle.m_Radius) < rect.m_Min.m_X ||
                 (circle.m_Center.m_X - circle.m_Radius) > rect.m_Max.m_X);
    else
    if (circle.m_Center.m_X >= rect.m_Min.m_X && circle.m_Center.m_X <= rect.m_Max.m_X)
        return !((circle.m_Center.m_Y + circle.m_Radius) < rect.m_Min.m_Y ||
                 (circle.m_Center.m_Y - circle.m_Radius) > rect.m_Max.m_Y);
    else
    if (circle.Inside(rect.m_Min.m_X, rect.m_Min.m_Y))
        return true;
    else
    if (circle.Inside(rect.m_Min.m_X, rect.m_Max.m_Y))
        return true;
    else
    if (circle.Inside(rect.m_Max.m_X, rect.m_Min.m_Y))
        return true;
    else
    if (circle.Inside(rect.m_Max.m_X, rect.m_Max.m_Y))
        return true;
    else
        return false;
}
//---------------------------------------------------------------------------

