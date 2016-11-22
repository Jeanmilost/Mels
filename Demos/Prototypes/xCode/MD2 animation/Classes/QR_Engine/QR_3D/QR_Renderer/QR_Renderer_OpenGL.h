/******************************************************************************
 * ==> QR_Renderer_OpenGL ----------------------------------------------------*
 ******************************************************************************
 * Description : Specialized OpenGL renderer                                  *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_Renderer_OpenGLH
#define QR_Renderer_OpenGLH

// qr engine
#include "QR_Types.h"
#include "QR_Renderer.h"

// windows
#if defined(_WIN32) && !defined(USE_GLFW_LIBRARY)
    #include <Windows.h>
#endif // defined(_WIN32) && !defined(USE_GLFW_LIBRARY)

/**
* Generic renderer for 3D engine
*@note This class is cross-platform
*@note USE_OPENGL_DIRECT_MODE must be globally defined to use the OpenGL direct
*      mode instead of shader. BE CAREFUL, OpenGL direct mode is deprecated
*@author Jean-Milost Reymond
*/
class QR_Renderer_OpenGL : public QR_Renderer
{
    public:
        /**
        * Constructor
        */
        QR_Renderer_OpenGL();

        #if defined(_WIN32)
            /**
            * Constructor
            *@param hWnd - Windows handle
            *@param fOnConfigure - OnConfigure callback function handler
            */
            QR_Renderer_OpenGL(HWND           hWnd,
                               ITfOnConfigure fOnConfigure = NULL);
        #endif // defined(_WIN32)

        virtual ~QR_Renderer_OpenGL();

        /**
        * Begins a scene
        *@param color - scene background color
        *@param flags - scene flags
        */
        virtual void BeginScene(const QR_Color& color, IESceneFlags flags) const;

        /**
        * Ends a scene
        */
        virtual void EndScene() const;

    private:
        #ifdef _WIN32
            HWND  m_hWnd;
            #ifndef USE_GLFW_LIBRARY
                HDC   m_hDC;
                HGLRC m_hRC;
            #endif // USE_GLFW_LIBRARY
        #endif // defined(_WIN32)

        /**
        * Initializes the renderer
        *@param fOnConfigure - OnConfigure callback function handler
        */
        void Initialize(ITfOnConfigure fOnConfigure);
};

#endif // QR_Renderer_OpenGLH
