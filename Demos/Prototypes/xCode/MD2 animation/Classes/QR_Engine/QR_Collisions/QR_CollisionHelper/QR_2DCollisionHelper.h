/******************************************************************************
 * ==> QR_2DCollisionHelper --------------------------------------------------*
 ******************************************************************************
 * Description : Helper for collision detection in 2D world                   *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_2DCollisionHelperH
#define QR_2DCollisionHelperH

// qr engine
#include "QR_Types.h"
#include "QR_Circle.h"
#include "QR_Rectangle.h"

/**
* 2D Collisions detections toolbox
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_2DCollisionHelper
{
    public:
        /**
        * Constructor
        */
        QR_2DCollisionHelper();

        /**
        * Destructor
        */
        virtual ~QR_2DCollisionHelper();

        /**
        * Tests and gets collision between a circle and a rectangle
        *@param rect - rectangle to check
        *@param circle - circle to check
        *@return true if objects are in collision, otherwise false
        */
        static bool GetRectCircleCollision(const QR_Rectangle& rect,
                                           const QR_Circle&    circle);
};

#endif
