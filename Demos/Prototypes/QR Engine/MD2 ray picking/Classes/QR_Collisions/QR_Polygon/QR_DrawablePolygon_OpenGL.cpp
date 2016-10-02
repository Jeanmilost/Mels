/******************************************************************************
 * ==> QR_DrawablePolygon_OpenGL ---------------------------------------------*
 ******************************************************************************
 * Description : Polygon that can be drawn by OpenGL                          *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_DrawablePolygon_OpenGL.h"

// libraries
#include <Windows.h>

// OpenGL
#include <gl\gl.h>

//------------------------------------------------------------------------------
// QR_DrawablePolygon_OpenGL - c++ cross-platform
//------------------------------------------------------------------------------
QR_DrawablePolygon_OpenGL::QR_DrawablePolygon_OpenGL(bool directMode) :
    QR_DrawablePolygon(),
    m_DirectMode(directMode)
{}
//------------------------------------------------------------------------------
QR_DrawablePolygon_OpenGL::QR_DrawablePolygon_OpenGL(QR_Polygon* pPolygon, bool directMode) :
    QR_DrawablePolygon(pPolygon),
    m_DirectMode(directMode)
{}
//------------------------------------------------------------------------------
QR_DrawablePolygon_OpenGL::~QR_DrawablePolygon_OpenGL()
{}
//------------------------------------------------------------------------------
void QR_DrawablePolygon_OpenGL::Draw() const
{
    // no polygon defined?
    if (!m_pPolygon)
        return;

    // do draw polygon using direct mode?
    if (m_DirectMode)
    {
        glBegin(GL_TRIANGLES);

        // draw vertex 1
        glColor3f(m_Colors[0].GetRedF(), m_Colors[0].GetGreenF(), m_Colors[0].GetBlueF());
        glVertex3f(m_pPolygon->GetVertex1().m_X,
                   m_pPolygon->GetVertex1().m_Y,
                   m_pPolygon->GetVertex1().m_Z);

        // draw vertex 2
        glColor3f(m_Colors[1].GetRedF(), m_Colors[1].GetGreenF(), m_Colors[1].GetBlueF());
        glVertex3f(m_pPolygon->GetVertex2().m_X,
                   m_pPolygon->GetVertex2().m_Y,
                   m_pPolygon->GetVertex2().m_Z);

        // draw vertex 3
        glColor3f(m_Colors[2].GetRedF(), m_Colors[2].GetGreenF(), m_Colors[2].GetBlueF());
        glVertex3f(m_pPolygon->GetVertex3().m_X,
                   m_pPolygon->GetVertex3().m_Y,
                   m_pPolygon->GetVertex3().m_Z);

        glEnd();

        glFlush();

        return;
    }

    // todo -cFeature -oJean: Implement this
    throw "NOT IMPLEMENTED";
}
//------------------------------------------------------------------------------
