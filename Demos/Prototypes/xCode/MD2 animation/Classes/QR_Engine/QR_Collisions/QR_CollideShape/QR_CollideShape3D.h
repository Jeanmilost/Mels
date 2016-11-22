/******************************************************************************
 * ==> QR_CollideShape3D -----------------------------------------------------*
 ******************************************************************************
 * Description : basic 3D collide shape                                       *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_CollideShape3DH
#define QR_CollideShape3DH

// qr engine
#include "QR_Types.h"
#include "QR_Vector3D.h"

/**
* Basic 3D collide shape
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
template <class T>
class QR_CollideShape3D
{
    public:
        /**
        * Constructor
        */
        QR_CollideShape3D();

        /**
        * Destructor
        */
        virtual ~QR_CollideShape3D();

        /**
        * Check if a point is inside object
        *@param x - point x coordinate
        *@param y - point y coordinate
        *@param z - point z coordinate
        *@return true if point is inside object, otherwise false
        */
        virtual bool Inside(const T& x, const T& y, const T& z) const = 0;

        /**
        * Check if a point is inside object
        *@param point - point coordinate
        *@return true if point is inside object, otherwise false
        */
        virtual bool Inside(const QR_Vector3D<T>& point) const = 0;

        /**
        * Check if object intersects with another object
        *@param other - other object to check
        *@return true if objects intersect, otherwise false
        */
        virtual bool Intersect(const QR_CollideShape3D* pOther) const = 0;
};

//------------------------------------------------------------------------------
// QR_CollideShape3D implementation - c++ cross-platform
//------------------------------------------------------------------------------
template <class T>
QR_CollideShape3D<T>::QR_CollideShape3D()
{}
//------------------------------------------------------------------------------
template <class T>
QR_CollideShape3D<T>::~QR_CollideShape3D()
{}
//------------------------------------------------------------------------------

#endif // QR_CollideShape3DH
