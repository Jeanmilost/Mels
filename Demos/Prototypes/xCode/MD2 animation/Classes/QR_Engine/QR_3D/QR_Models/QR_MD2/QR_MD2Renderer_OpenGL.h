/******************************************************************************
 * ==> QR_MD2Renderer_OpenGL -------------------------------------------------*
 ******************************************************************************
 * Description : MD2 model renderer that uses OpenGL for the drawing          *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_MD2RENDERER_OPENGLH
#define QR_MD2RENDERER_OPENGLH

// std
#include <vector>

// qr engine
#include "QR_MD2Renderer.h"
#include "QR_OpenGLHelper.h"

// openGL
#ifdef USE_OPENGL_DIRECT_MODE
    #ifdef _WIN32
        // needed before including gl.h, otherwise RAD studio compilers will generate an error
        #include <windows.h>

        #include <gl/gl.h>
    #elif defined (__APPLE__)
        #include <OpenGLES/ES1/gl.h>
    #endif // _WIN32 / __APPLE__
#else
    #ifdef _WIN32
        // needed before including gl.h, otherwise RAD studio compilers will generate an error
        #include <windows.h>

        // needed for shader language
        #define GLEW_STATIC
        #include <gl/glew.h>

        #include <gl/gl.h>
        #include <gl/glu.h>
    #elif defined (__APPLE__)
        #include <OpenGLES/ES2/gl.h>
        #include <OpenGLES/ES2/glext.h>
    #endif // _WIN32 / __APPLE__
#endif // USE_OPENGL_DIRECT_MODE

/**
* MD2 model renderer that uses OpenGL for the drawing
*@note This class is cross-platform
*@note USE_OPENGL_DIRECT_MODE must be globally defined to use the OpenGL direct
*      mode instead of shader. BE CAREFUL, OpenGL direct mode is deprecated
*@author Jean-Milost Reymond
*/
class QR_MD2Renderer_OpenGL : public QR_MD2Renderer
{
    public:
        /**
        * Constructor
        */
        QR_MD2Renderer_OpenGL();

        /**
        * Destructor
        */
        virtual ~QR_MD2Renderer_OpenGL();

        /**
        * Initializes model
        */
        virtual void Initialize();

        /**
        * Draws the model
        *@param elapsedTime - elapsed time since last frame was drawn
        *@param fps - number of frames per seconds
        */
        virtual void Draw(const QR_Double& elapsedTime, QR_SizeT fps);

    private:
        #ifndef USE_OPENGL_DIRECT_MODE
            GLuint                              m_VAO;
            QR_OpenGLHelper::IGLFrameDictionary m_GLFrameDict;
        #endif // USE_OPENGL_DIRECT_MODE

        /**
        * Finalizes draw operation
        */
        #ifndef USE_OPENGL_DIRECT_MODE
            void FinalizeDraw();
        #endif // USE_OPENGL_DIRECT_MODE

        /**
        * Creates OpenGL vertex buffers to use to draw model
        */
        #ifndef USE_OPENGL_DIRECT_MODE
            void CreateVertexBuffers();
        #endif // USE_OPENGL_DIRECT_MODE

        /**
        * Deletes OpenGL vertex buffers
        */
        #ifndef USE_OPENGL_DIRECT_MODE
            void DeleteVertexBuffers();
        #endif // USE_OPENGL_DIRECT_MODE
};

#endif // QR_MD2RENDERER_OPENGLH
