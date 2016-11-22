/******************************************************************************
 * ==> QR_Renderer_OpenGL ----------------------------------------------------*
 ******************************************************************************
 * Description : Specialized OpenGL renderer                                  *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_Renderer_OpenGL.h"

// std
#include <sstream>
#include <cmath>

// qr engine
#include "QR_Exception.h"

// openGL
#ifdef _WIN32
    #include <gl/gl.h>
#elif defined (__APPLE__)
    #ifdef USE_OPENGL_DIRECT_MODE
        #include </Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS9.3.sdk/System/Library/Frameworks/OpenGLES.framework/Headers/ES1/gl.h>
    #else
        #include <ES2/gl.h>
        #include <ES2/glext.h>
    #endif // USE_OPENGL_DIRECT_MODE
#endif // _WIN32 / __APPLE__

#ifdef USE_GLFW_LIBRARY
    #include <gl/glfw.h>
#endif // USE_GLFW_LIBRARY

//------------------------------------------------------------------------------
// Class QR_Renderer_OpenGL - c++ cross-platform
//------------------------------------------------------------------------------
QR_Renderer_OpenGL::QR_Renderer_OpenGL() : QR_Renderer()
{}
//------------------------------------------------------------------------------
#ifdef _WIN32
    QR_Renderer_OpenGL::QR_Renderer_OpenGL(HWND           hWnd,
                                           ITfOnConfigure fOnConfigure) :
        QR_Renderer(),
        m_hWnd(hWnd)
        #ifndef USE_GLFW_LIBRARY
            ,
            m_hDC(NULL),
            m_hRC(NULL)
        #endif // USE_GLFW_LIBRARY
    {
        Initialize(fOnConfigure);
    }
#endif // defined(_WIN32)
//------------------------------------------------------------------------------
QR_Renderer_OpenGL::~QR_Renderer_OpenGL()
{
    #if defined(_WIN32) && !defined(USE_GLFW_LIBRARY)
        // release OpenGL context
        ::wglMakeCurrent(NULL, NULL);

        // delete OpenGL context
        if (m_hRC)
            wglDeleteContext(m_hRC);

        // delete device context
        if (m_hWnd && m_hDC)
            ::ReleaseDC(m_hWnd, m_hDC);
    #endif // defined(_WIN32) && !defined(USE_GLFW_LIBRARY)
}
//------------------------------------------------------------------------------
void QR_Renderer_OpenGL::BeginScene(const QR_Color& color, IESceneFlags flags) const
{
    // begin post-processing effect
    if (m_pPostProcessingEffect)
        m_pPostProcessingEffect->Begin();

    GLbitfield openGLSceneFlags = 0;

    // clear background color, if needed
    if (flags & IE_SF_ClearColor)
    {
        glClearColor((GLclampf)color.GetRedF(), (GLclampf)color.GetGreenF(),
                (GLclampf)color.GetBlueF(), (GLclampf)color.GetAlphaF());

        openGLSceneFlags |= GL_COLOR_BUFFER_BIT;
    }

    // clear Z buffer, if needed
    if (flags & IE_SF_ClearDepth)
    {
        #ifdef __APPLE__
            glClearDepthf(1.0f);
        #else
            glClearDepth(1.0f);
        #endif

        openGLSceneFlags |= GL_DEPTH_BUFFER_BIT;
    }

    // clear scene, fill with background color and set render flags
    glClear(openGLSceneFlags);
}
//------------------------------------------------------------------------------
void QR_Renderer_OpenGL::EndScene() const
{
    // end post-processing effect
    if (m_pPostProcessingEffect)
        m_pPostProcessingEffect->End();

    #if defined(_WIN32)
        #ifdef USE_GLFW_LIBRARY
            // swap the display buffers (displays what was just drawn)
            glfwSwapBuffers();
        #else
            // no device context?
            if (!m_hDC)
                return;

            // present back buffer
            ::SwapBuffers(m_hDC);
        #endif // USE_GLFW_LIBRARY
    #elif defined(__APPLE__)
        // nothing to do, Apple OpenGLES implementation does everything for us
    #else
        #error "Do implement EndScene() for this platform"
    #endif
}
//------------------------------------------------------------------------------
void QR_Renderer_OpenGL::Initialize(ITfOnConfigure fOnConfigure)
{
    #if defined(_WIN32) && !defined(USE_GLFW_LIBRARY)
        ::PIXELFORMATDESCRIPTOR pfd;
        QR_Int32                iFormat;

        // get device context
        m_hDC = ::GetDC(m_hWnd);

        // set device context pixel format
        ::ZeroMemory(&pfd, sizeof(pfd));
        pfd.nSize        = sizeof(pfd);
        pfd.nVersion     = 1;
        pfd.dwFlags      = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER;
        pfd.iPixelType   = PFD_TYPE_RGBA;
        pfd.cColorBits   = 32;
        pfd.cDepthBits   = 32;
        pfd.cStencilBits = 32;
        pfd.iLayerType   = PFD_MAIN_PLANE;
        iFormat          = ::ChoosePixelFormat(m_hDC, &pfd);
        ::SetPixelFormat(m_hDC, iFormat, &pfd);

        // create and enable OpenGL render context
        m_hRC = ::wglCreateContext(m_hDC);
        ::wglMakeCurrent(m_hDC, m_hRC);

        // notify that OpenGL can be configured
        if (fOnConfigure)
            fOnConfigure(this);
    #endif // defined(_WIN32) && !defined(USE_GLFW_LIBRARY)
}
//------------------------------------------------------------------------------
