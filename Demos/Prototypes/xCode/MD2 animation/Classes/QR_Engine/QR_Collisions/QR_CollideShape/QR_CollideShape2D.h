/******************************************************************************
 * ==> QR_CollideShape2D -----------------------------------------------------*
 ******************************************************************************
 * Description : basic 2D collide shape                                       *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_CollideObject_2DH
#define QR_CollideObject_2DH

// qr engine
#include "QR_Types.h"
#include "QR_Vector2D.h"

/**
* Basic 2D collide object
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
template <class T>
class QR_CollideShape2D
{
    public:
        /**
        * Constructor
        */
        QR_CollideShape2D();

        /**
        * Destructor
        */
        virtual ~QR_CollideShape2D();

        /**
        * Check if a point is inside object
        *@param x - point x coordinate
        *@param y - point y coordinate
        *@return true if point is inside object, otherwise false
        */
        virtual bool Inside(const T& x, const T& y) const = 0;

        /**
        * Check if a point is inside object
        *@param point - point coordinate
        *@return true if point is inside object, otherwise false
        */
        virtual bool Inside(const QR_Vector2D<T>& point) const = 0;

        /**
        * Check if object intersects with another object
        *@param other - other object to check
        *@return true if objects intersect, otherwise false
        */
        virtual bool Intersect(const QR_CollideShape2D* pOther) const = 0;
};

//------------------------------------------------------------------------------
// QR_CollideShape2D implementation - c++ cross-platform
//------------------------------------------------------------------------------
template <class T>
QR_CollideShape2D<T>::QR_CollideShape2D()
{}
//------------------------------------------------------------------------------
template <class T>
QR_CollideShape2D<T>::~QR_CollideShape2D()
{}
//------------------------------------------------------------------------------

#endif // QR_CollideObject_2DH
