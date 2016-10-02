/******************************************************************************
 * ==> QR_Box ----------------------------------------------------------------*
 ******************************************************************************
 * Description : 3D box                                                       *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_Box.h"

// std
#include <cmath>

// qr engine
#include "QR_Exception.h"
#include "QR_Sphere.h"
#include "QR_3DCollisionHelper.h"

//------------------------------------------------------------------------------
// Class QR_Box - c++ cross-platform
//------------------------------------------------------------------------------
QR_Box::QR_Box() : QR_CollideShape3D<M_Precision>()
{}
//------------------------------------------------------------------------------
QR_Box::QR_Box(const QR_Vector3DP& min, const QR_Vector3DP& max)
{
    m_Min = min;
    m_Max = max;
}
//------------------------------------------------------------------------------
QR_Box::~QR_Box()
{}
//------------------------------------------------------------------------------
QR_Vector3DP QR_Box::GetCenter() const
{
    return QR_Vector3DP((m_Min.m_X + m_Max.m_X) / 2.0f,
                        (m_Min.m_Y + m_Max.m_Y) / 2.0f,
                        (m_Min.m_Z + m_Max.m_Z) / 2.0f);
}
//------------------------------------------------------------------------------
QR_Box::IE_Axis QR_Box::GetLongestAxis() const
{
    // calculate each edge length
    M_Precision x = std::fabs(m_Max.m_X - m_Min.m_X);
    M_Precision y = std::fabs(m_Max.m_Y - m_Min.m_Y);
    M_Precision z = std::fabs(m_Max.m_Z - m_Min.m_Z);

    // search for longest axis
    if (x >= y && x >= z)
        return IE_X;
    else
    if (y >= x && y >= z)
        return IE_Y;
    else
        return IE_Z;
}
//------------------------------------------------------------------------------
QR_PlaneP QR_Box::GetSplittingPlane(IE_Axis axis) const
{
    switch (axis)
    {
        case IE_X:
            return QR_PlaneP(1.0f, 0.0f, 0.0f, -GetCenter().m_X);

        case IE_Y:
            return QR_PlaneP(0.0f, 1.0f, 0.0f, -GetCenter().m_Y);

        case IE_Z:
            return QR_PlaneP(0.0f, 0.0f, 1.0f, -GetCenter().m_Z);

        default:
            return QR_PlaneP();
    }
}
//------------------------------------------------------------------------------
QR_PlaneP QR_Box::GetSplittingPlane(IE_Axis axis, const QR_Vector3DP& center) const
{
    switch (axis)
    {
        case IE_X:
            return QR_PlaneP(1.0f, 0.0f, 0.0f, -center.m_X);

        case IE_Y:
            return QR_PlaneP(0.0f, 1.0f, 0.0f, -center.m_Y);

        case IE_Z:
            return QR_PlaneP(0.0f, 0.0f, 1.0f, -center.m_Z);

        default:
            return QR_PlaneP();
    }
}
//------------------------------------------------------------------------------
bool QR_Box::Inside(const M_Precision& x, const M_Precision& y,
        const M_Precision& z) const
{
    return Inside(QR_Vector3DP(x, y, z));
}
//------------------------------------------------------------------------------
bool QR_Box::Inside(const QR_Vector3DP& point) const
{
    return (point.m_X >= m_Min.m_X &&
            point.m_Y >= m_Min.m_Y &&
            point.m_Z >= m_Min.m_Z &&
            point.m_X <= m_Max.m_X &&
            point.m_Y <= m_Max.m_Y &&
            point.m_Z <= m_Max.m_Z);
}
//------------------------------------------------------------------------------
bool QR_Box::Intersect(const QR_CollideShape3D<M_Precision>* pOther) const
{
    // try to get box from source
    const QR_Box* pBox = dynamic_cast<const QR_Box*>(pOther);

    // found it?
    if (pBox)
        return !(m_Min.m_X > pBox->m_Max.m_X || m_Max.m_X < pBox->m_Min.m_X ||
                 m_Min.m_Y > pBox->m_Max.m_Y || m_Max.m_Y < pBox->m_Min.m_Y ||
                 m_Min.m_Z > pBox->m_Max.m_Z || m_Max.m_Z < pBox->m_Min.m_Z);

    // try to get sphere from source
    const QR_Sphere* pSphere = dynamic_cast<const QR_Sphere*>(pOther);

    // found it?
    if (pSphere)
        return QR_3DCollisionHelper::GetSphereBoxCollision(*pSphere, *this);

    // found nothing
    M_THROW_EXCEPTION("Unknown source object");
}
//------------------------------------------------------------------------------

