/******************************************************************************
 * ==> QR_DrawablePolygon ----------------------------------------------------*
 ******************************************************************************
 * Description : Polygon that can be drawn                                    *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_DrawablePolygonH
#define QR_DrawablePolygonH

// qr engine
#include "QR_Polygon.h"
#include "QR_Color.h"

/**
* Polygon that can be drawn
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_DrawablePolygon
{
    public:
        /**
        * Constructor
        */
        QR_DrawablePolygon();

        /**
        * Constructor
        *@param pPolygon - polygon to draw
        */
        QR_DrawablePolygon(QR_Polygon* pPolygon);

        virtual ~QR_DrawablePolygon();

        /**
        * Gets polygon to draw
        *@return polygon to draw
        */
        virtual QR_Polygon* Get() const;

        /**
        * Sets polygon to draw
        *@param pPolygon - polygon to draw
        */
        virtual void Set(QR_Polygon* pPolygon);

        /**
        * Gets polygon vertex color
        *@param index - color index, between 0 and 2
        *@return vertex color, NULL if not found or index is out of bounds
        */
        virtual const QR_Color* GetVertexColor(QR_SizeT index) const;

        /**
        * Sets polygon vertex color
        *@param index - color index, between 0 and 2
        *@param color - vertex color
        */
        virtual void SetVertexColor(QR_SizeT index, const QR_Color& color);

        /**
        * Draws polygon
        */
        virtual void Draw() const = 0;

    protected:
        QR_Polygon* m_pPolygon;
        QR_Color    m_Colors[3];
};

#endif // QR_DrawablePolygonH
