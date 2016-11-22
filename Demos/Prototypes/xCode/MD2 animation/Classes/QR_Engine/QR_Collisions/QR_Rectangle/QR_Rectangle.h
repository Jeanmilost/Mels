/******************************************************************************
 * ==> QR_Rectangle ----------------------------------------------------------*
 ******************************************************************************
 * Description : 2D rectangle                                                 *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_RectangleH
#define QR_RectangleH

// qr engine
#include "QR_CollideShape2D.h"
#include "QR_Vector2D.h"

/**
* Rectangle
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_Rectangle : public QR_CollideShape2D<M_Precision>
{
    public:
        QR_Vector2DP m_Min; // rectangle min coordinate
        QR_Vector2DP m_Max; // rectangle max coordinate

        /**
        * Constructor
        */
        QR_Rectangle();

        /**
        * Constructor
        *@param min - min corner
        *@param max - max corner
        */
        QR_Rectangle(const QR_Vector2DP& min, const QR_Vector2DP& max);

        /**
        * Destructor
        */
        virtual ~QR_Rectangle();

        /**
        * Check if a point is inside rectangle
        *@param x - point x coordinate
        *@param y - point y coordinate
        *@return true if point is inside rectangle, otherwise false
        */
        virtual bool Inside(const M_Precision& x, const M_Precision& y) const;

        /**
        * Check if a point is inside rectangle
        *@param point - point coordinate
        *@return true if point is inside rectangle, otherwise false
        */
        virtual bool Inside(const QR_Vector2DP& point) const;

        /**
        * Check if rectangle intersects with another rectangle
        *@param other - other rectangle to check
        *@return true if rectangles intersect, otherwise false
        */
        virtual bool Intersect(const QR_CollideShape2D<M_Precision>* pOther) const;
};

#endif // QR_RectangleH
