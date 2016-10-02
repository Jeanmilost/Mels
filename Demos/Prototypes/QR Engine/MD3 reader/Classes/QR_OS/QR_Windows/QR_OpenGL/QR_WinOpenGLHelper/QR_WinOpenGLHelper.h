/******************************************************************************
 * ==> QR_WinOpenGLHelper ----------------------------------------------------*
 ******************************************************************************
 * Description : OpenGL helper in the Windows API context                     *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_WinOpenGLHelperH
#define QR_WinOpenGLHelperH

// qr engine
#include "QR_Rect.h"
#include "QR_Vector3D.h"

// libraries
#include <Windows.h>

/**
* OpenGL helper in the Windows API context
*@author Jean-Milost Reymond
*/
class QR_WinOpenGLHelper
{
    public:
        /**
        * Constructor
        */
        QR_WinOpenGLHelper();

        /**
        * Destructor
        */
        virtual ~QR_WinOpenGLHelper();

        /**
        * Enables OpenGL
        *@param hWnd - control or form hanle on which OpenGL scene will be drawn
        *@param hDC[out] - device context to use to draw OpenGL scene
        *@param hRC[out] - newly created OpenGL context
        *@return true on success, otherwise false
        */
        static bool EnableOpenGL(HWND hWnd, HDC& hDC, HGLRC& hRC);

        /**
        * Disables OpenGL and clears memory
        *@param hWnd - control or form hanle on which OpenGL scene was drawn
        *@param hDC - device context used to draw OpenGL scene
        *@param hRC - OpenGL context to shutdown
        */
        static void DisableOpenGL(HWND hWnd, HDC hDC, HGLRC hRC);

        /**
        * Sets pixel format to use with OpenGL
        *@param hDC - device context used to draw OpenGL scene
        *@return true on success, otherwise false
        */
        static bool SetPixelFormat(HDC hDC);

        /**
        * Converts mouse position to OpenGL point (i.e. a point in the OpenGL space)
        *@param hWnd - handle of the window or control on which mouse is hoving
        *@param viewRect - OpenGL view rectangle        *@return converted point in the OpenGL space
        */
        static QR_Vector3DP MousePosToGLPoint(HWND hWnd, const QR_Rect<M_Precision>& viewRect);
};

#endif // QR_WinOpenGLHelperH
