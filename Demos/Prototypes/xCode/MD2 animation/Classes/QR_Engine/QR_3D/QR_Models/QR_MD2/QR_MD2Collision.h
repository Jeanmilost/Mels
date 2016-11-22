/******************************************************************************
 * ==> QR_MD2Collision -------------------------------------------------------*
 ******************************************************************************
 * Description : Collision data to use with a MD2 model                       *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_MD2CollisionH
#define QR_MD2CollisionH

// qr engine
#include "QR_ModelCollision.h"
#include "QR_SimplePolygon.h"
#include "QR_IndexedPolygon.h"
#include "QR_MD2Animation.h"

/**
* Collision data to use with a MD2 model
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_MD2Collision : public QR_ModelCollision
{
    public:
        /**
        * Constructor
        */
        QR_MD2Collision();

        /**
        * Destructor
        */
        virtual ~QR_MD2Collision();

        /**
        * Resolves collision on an animated MD2 model
        *@param elapsedTime - elapsed time since last draw
        *@param fps - frame per seconds
        *@param pAnimation - MD2 animation object
        *@param ray - ray to test against
        *@param[out] polygonList - polygon list that can be in collision with ray
        *@returns true on success, otherwise false
        *@note As resulting polygon list content is created dynamically, because of interpolation,
        *      it should be deleted explicitely when useless
        */
        virtual bool ResolveAnimation(const QR_Double&       elapsedTime,
                                            QR_SizeT         fps,
                                            QR_MD2Animation* pAnimation,
                                      const QR_RayP&         ray,
                                            QR_PolygonList&  polygonList) const;

    protected:
        typedef std::vector<QR_IndexedPolygon*> ITestedPolygons;

        /**
        * Gets polygons to check and interpolate them with next frame
        *@param animation - currently running animation
        *@param pItem - item to interpolate with
        *@param polygonsToCheck - polygons that were resolved by AABB tree in first frame
        *@param[out] polygons - interpolated polygons to check
        */
        virtual void InterpolateAndGetPolygonsToCheck(const QR_MD2Animation::IAnimation& animation,
                                                      const ICollideItem*                pItem,
                                                      const QR_PolygonList&              polygonsToCheck,
                                                            QR_PolygonList&              polygons) const;

        /**
        * Creates interpolated polygon
        *@param i1 - first polygon index
        *@param i2 - second polygon index
        *@param i3 - third polygon index
        *@param buffer1 - buffer containing first frame polygons
        *@param buffer2 - buffer containing second frame polygons
        *@param factor - interpolation factor
        *@return interpolated polygon
        */
        virtual QR_SimplePolygon* CreateInterpolatedPolygon(      QR_SizeT            i1,
                                                                  QR_SizeT            i2,
                                                                  QR_SizeT            i3,
                                                            const QR_Vertex::IBuffer& buffer1,
                                                            const QR_Vertex::IBuffer& buffer2,
                                                                  QR_Double           factor) const;
};

#endif // QR_MD2CollisionH
