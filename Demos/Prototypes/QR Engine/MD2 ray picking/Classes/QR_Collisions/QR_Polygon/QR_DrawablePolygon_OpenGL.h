/******************************************************************************
 * ==> QR_DrawablePolygon_OpenGL ---------------------------------------------*
 ******************************************************************************
 * Description : Polygon that can be drawn by OpenGL                          *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_DrawablePolygon_OpenGLH
#define QR_DrawablePolygon_OpenGLH

// qr engine
#include "QR_DrawablePolygon.h"

/**
* Polygon that can be drawn by OpenGL
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_DrawablePolygon_OpenGL : public QR_DrawablePolygon
{
    public:
        /**
        * Constructor
        *@param directMode - if true, OpenGL will use direct mode to draw polygon
        */
        QR_DrawablePolygon_OpenGL(bool directMode = false);

        /**
        * Constructor
        *@param pPolygon - polygon to draw
        *@param directMode - if true, OpenGL will use direct mode to draw polygon
        */
        QR_DrawablePolygon_OpenGL(QR_Polygon* pPolygon, bool directMode);

        virtual ~QR_DrawablePolygon_OpenGL();

        /**
        * Draws polygon
        */
        virtual void Draw() const;

    private:
        bool m_DirectMode;
};

#endif // QR_DrawablePolygon_OpenGLH
