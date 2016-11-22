/******************************************************************************
 * ==> QR_Sphere -------------------------------------------------------------*
 ******************************************************************************
 * Description : 3D sphere                                                    *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_Sphere.h"

// std
#include <cmath>

// qr engine
#include "QR_Exception.h"
#include "QR_Box.h"
#include "QR_3DCollisionHelper.h"

//------------------------------------------------------------------------------
// Class QR_Sphere - c++ cross-platform
//------------------------------------------------------------------------------
QR_Sphere::QR_Sphere() : QR_CollideShape3D<M_Precision>()
{
    m_Radius = 0.0f;
}
//------------------------------------------------------------------------------
QR_Sphere::QR_Sphere(const QR_Vector3DP& center, M_Precision radius) :
    QR_CollideShape3D<M_Precision>()
{
    m_Center = center;
    m_Radius = radius;
}
//------------------------------------------------------------------------------
QR_Sphere::~QR_Sphere()
{}
//------------------------------------------------------------------------------
bool QR_Sphere::Inside(const M_Precision& x,
                       const M_Precision& y,
                       const M_Precision& z) const
{
    return Inside(QR_Vector3DP(x, y, z));
}
//------------------------------------------------------------------------------
bool QR_Sphere::Inside(const QR_Vector3DP& point) const
{
    // calculate the distance between test point and the center of the sphere
    QR_Vector3DP length   = point - m_Center;
    M_Precision  distance = length.Length();

    // check if distance is shorter than the sphere radius and return result
    return (distance <= m_Radius);
}
//------------------------------------------------------------------------------
bool QR_Sphere::Intersect(const QR_CollideShape3D<M_Precision>* pOther) const
{
    // try to get sphere from source
    const QR_Sphere* pSphere = dynamic_cast<const QR_Sphere*>(pOther);

    // found it?
    if (pSphere)
    {
        QR_Vector3DP dist(std::fabs(m_Center.m_X - pSphere->m_Center.m_X),
                          std::fabs(m_Center.m_Y - pSphere->m_Center.m_Y),
                          std::fabs(m_Center.m_Z - pSphere->m_Center.m_Z));

        M_Precision length = dist.Length();

        return (length <= (m_Radius + pSphere->m_Radius));
    }

    // try to get box from source
    const QR_Box* pBox = dynamic_cast<const QR_Box*>(pOther);

    // found it?
    if (pBox)
        return QR_3DCollisionHelper::GetSphereBoxCollision(*this, *pBox);

    M_THROW_EXCEPTION("Unknown source object");
}
//------------------------------------------------------------------------------

