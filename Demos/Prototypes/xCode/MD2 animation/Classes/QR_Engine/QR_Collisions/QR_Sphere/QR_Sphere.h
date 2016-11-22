/******************************************************************************
 * ==> QR_Sphere -------------------------------------------------------------*
 ******************************************************************************
 * Description : 3D sphere                                                    *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_SphereH
#define QR_SphereH

// qr engine
#include "QR_CollideShape3D.h"

/**
* Sphere
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_Sphere : public QR_CollideShape3D<M_Precision>
{
    public:
        QR_Vector3DP m_Center;
        M_Precision  m_Radius;

        /**
        * Constructor
        */
        QR_Sphere();

        /**
        * Constructor
        *@param center - center
        *@param radius - radius
        */
        QR_Sphere(const QR_Vector3DP& center, M_Precision radius);

        /**
        * Destructor
        */
        virtual ~QR_Sphere();

        /**
        * Check if a point is inside rectangle
        *@param x - point x coordinate
        *@param y - point y coordinate
        *@return true if point is inside rectangle, otherwise false
        */
        virtual bool Inside(const M_Precision& x,
                            const M_Precision& y,
                            const M_Precision& z) const;

        /**
        * Check if a point is inside sphere
        *@param point - point coordinate
        *@return true if point is inside sphere, otherwise false
        */
        virtual bool Inside(const QR_Vector3DP& point) const;

        /**
        * Check if sphere intersects with another sphere
        *@param pOther - other sphere to check
        *@return true if spheres intersect, otherwise false
        */
        virtual bool Intersect(const QR_CollideShape3D<M_Precision>* pOther) const;
};

#endif // QR_SphereH
