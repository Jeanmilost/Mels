/******************************************************************************
 * ==> QR_Circle -------------------------------------------------------------*
 ******************************************************************************
 * Description : 2D circle                                                    *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_CircleH
#define QR_CircleH

// qr engine
#include "QR_CollideShape2D.h"
#include "QR_Vector2D.h"

/**
* Circle
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_Circle : public QR_CollideShape2D<M_Precision>
{
    public:
        QR_Vector2DP m_Center; // circle center coordinates
        M_Precision  m_Radius; // circle radius

        /**
        * Constructor
        */
        QR_Circle();

        /**
        * Constructor
        *@param center - circle center coordinates
        *@param radius - circle radius
        */
        QR_Circle(const QR_Vector2DP& center, const M_Precision& radius);

        /**
        * Destructor
        */
        virtual ~QR_Circle();

        /**
        * Check if a point is inside circle
        *@param x - point x coordinate
        *@param y - point y coordinate
        *@return true if point is inside circle, otherwise false
        */
        virtual bool Inside(const M_Precision& x, const M_Precision& y) const;

        /**
        * Check if a point is inside circle
        *@param point - point coordinate
        *@return true if point is inside circle, otherwise false
        */
        virtual bool Inside(const QR_Vector2DP& point) const;

        /**
        * Check if circle intersects with another circle
        *@param pOther - other circle to check
        *@return true if circles intersect, otherwise false
        */
        virtual bool Intersect(const QR_CollideShape2D<M_Precision>* pOther) const;
};

#endif // QR_CircleH
