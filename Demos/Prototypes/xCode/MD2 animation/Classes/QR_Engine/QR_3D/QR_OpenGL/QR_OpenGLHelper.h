/*****************************************************************************
 * ==> QR_OpenGLHelper ------------------------------------------------------*
 *****************************************************************************
 * Description : OpenGL helper, contains some tools for OpenGL, and some     *
 *               important functions no more implemented in latest versions  *
 *               of OpenGL                                                   *
 * Developer   : Jean-Milost Reymond                                         *
 *****************************************************************************/

#ifndef QR_OpenGLHelperH
#define QR_OpenGLHelperH

// std
#include <vector>
#include <map>

// qr engine
#include "QR_Matrix16.h"
#include "QR_Algorithms.h"
#include "QR_Vertex.h"
#include "QR_Shader.h"

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

#ifdef USING_OPENGL_GLM_LIB
    #include <glm/glm.hpp>
    #include <glm/gtc/type_ptr.hpp>
#endif

/**
* OpenGL helper
*@note This class is cross-platform
*@note USE_OPENGL_DIRECT_MODE must be globally defined to use the OpenGL direct
*      mode instead of shader. BE CAREFUL, OpenGL direct mode is deprecated
*@author Jean-Milost Reymond
*/
class QR_OpenGLHelper
{
    public:
        /**
        * Interpolation info
        */
        struct IInterpolation
        {
            M_Precision m_Factor;
            bool        m_Enabled;

            IInterpolation();
            virtual ~IInterpolation();
        };

        #ifndef USE_OPENGL_DIRECT_MODE
            /**
            * OpenGL vertex structure
            */
            struct IGLVertex
            {
                GLuint            m_VBO;
                QR_Vertex::IEType m_Type;
                QR_SizeT          m_Size;

                IGLVertex();
                virtual ~IGLVertex();
            };

            typedef std::vector<IGLVertex*>           IGLMesh;
            typedef std::vector<IGLMesh*>             IGLFrames;
            typedef std::map<std::string, IGLFrames*> IGLFrameDictionary;
        #endif // USE_OPENGL_DIRECT_MODE

        /**
        * Constructor
        */
        QR_OpenGLHelper();

        /**
        * Destructor
        */
        virtual ~QR_OpenGLHelper();

        /**
        * Gets orthogonal projection matrix (glOrtho() OpenGL equivalent)
        *@param left - viewport left edge
        *@param right - viewport right edge
        *@param bottom - viewport bottom edge
        *@param top - viewport top edge
        *@param zNear - near clipping plane
        *@param zFar - far clipping plane
        *@return matrix
        */
        static inline QR_Matrix16P GetOrtho(const M_Precision& left,
                                            const M_Precision& right,
                                            const M_Precision& bottom,
                                            const M_Precision& top,
                                            const M_Precision& zNear,
                                            const M_Precision& zFar);

        /**
        * Gets frustum projection matrix (glFrustum() OpenGL equivalent)
        *@param left - viewport left edge
        *@param right - viewport right edge
        *@param bottom - viewport bottom edge
        *@param top - viewport top edge
        *@param zNear - near clipping plane
        *@param zFar - far clipping plane
        *@return matrix
        */
        static inline QR_Matrix16P GetFrustum(const M_Precision& left,
                                              const M_Precision& right,
                                              const M_Precision& bottom,
                                              const M_Precision& top,
                                              const M_Precision& zNear,
                                              const M_Precision& zFar);

        /**
        * Gets perspective projection matrix (glPerspective() OpenGL equivalent)
        *@param fovyDeg - field of view in degrees
        *@param aspect - viewport aspect
        *@param zNear - near clipping plane
        *@param zFar - far clipping plane
        *@return matrix
        */
        static inline QR_Matrix16P GetPerspective(const M_Precision& fovyDeg,
                                                  const M_Precision& aspect,
                                                  const M_Precision& zNear,
                                                  const M_Precision& zFar);

        /**
        * Creates OpenGL vertex array object
        *@param[out] vao - created vertex buffer array identifier
        */
        #ifndef USE_OPENGL_DIRECT_MODE
            static inline void CreateVertexArrayObject(GLuint& vao);
        #endif // USE_OPENGL_DIRECT_MODE

        /**
        * Deletes OpenGL vertex array object
        *@param vao - vertex buffer array identifier to delete
        */
        #ifndef USE_OPENGL_DIRECT_MODE
            static inline void DeleteVertexArrayObject(GLuint& vao);
        #endif // USE_OPENGL_DIRECT_MODE

        /**
        * Gets uniform attribute identifier from shader
        *@param pShader - shader to get from
        *@param attribute - shader attribute to get
        *@return Uniform attribute identifier, -1 if not found or on error
        *@note In GLSL, an uniform attribute is a global variable that may
        *      change per primitive (may not be set inside glBegin/glEnd), that
        *      is passed from the OpenGL application to the shader. This
        *      qualifier can be used in both vertex and fragment shader. For the
        *      shader this is a read-only variable
        */
        #ifndef USE_OPENGL_DIRECT_MODE
            static inline GLint GetUniform(const QR_Shader*             pShader,
                                                 QR_Shader::IEAttribute attribute);
        #endif // USE_OPENGL_DIRECT_MODE

        /**
        * Gets attribute identifier from shader
        *@param pShader - shader to get from
        *@param attribute - shader attribute to get
        *@return Attribute identifier, -1 if not found or on error
        *@note In GLSL, an attribute is a global variable that may change per
        *      vertex, that is passed from the OpenGL application to vertex
        *      shader. This qualifier can only be used in vertex shader. For the
        *      shader this is a read-only variable
        */
        #ifndef USE_OPENGL_DIRECT_MODE
            static inline GLint GetAttribute(const QR_Shader*             pShader,
                                                   QR_Shader::IEAttribute attribute);
        #endif // USE_OPENGL_DIRECT_MODE

        /**
        * Draws a mesh using OpenGL
        *@param mesh - mesh to draw
        *@param vertex - mesh vertex, should be the same for all meshes in list
        *@param translation - translation to apply to mesh
        *@param rotationX - rotation on x axis to apply to mesh
        *@param rotationY - rotation on y axis to apply to mesh
        *@param rotationZ - rotation on z axis to apply to mesh
        *@param scale - scaling to apply to mesh
        *@param deleteAfterUse - if true, mesh content will be deleted after drawn
        */
        #ifdef USE_OPENGL_DIRECT_MODE
            static void Draw(      QR_Mesh                mesh,
                                   QR_Vertex::IEFormat    format,
                                   QR_Vertex::IECoordType coordType,
                             const QR_Vector3DP           translation,
                             const M_Precision            rotationX,
                             const M_Precision            rotationY,
                             const M_Precision            rotationZ,
                             const QR_Vector3DP           scale,
                                   bool                   deleteAfterUse);
        #endif // USE_OPENGL_DIRECT_MODE

        /**
        * Draws a mesh using OpenGL
        *@param pGLMesh - OpenGL mesh to draw
        *@param vertex - mesh vertex, should be the same for all meshes in list
        *@param modelMatrix - model matrix, to position the mesh in the 3D world
        *@param interpolation - interpolation info
        *@param pShader - shader to use to draw mesh
        *@return true on success, otherwise false
        */
        #ifndef USE_OPENGL_DIRECT_MODE
            static bool Draw(const IGLMesh*               pGLMesh,
                                   QR_Vertex::IEFormat    format,
                                   QR_Vertex::IECoordType coordType,
                             const QR_Matrix16P&          modelMatrix,
                             const IInterpolation&        interpolation,
                             const QR_Shader*             pShader);
        #endif // USE_OPENGL_DIRECT_MODE

        #ifdef USING_OPENGL_GLM_LIB
            /**
            * Converts QR_Matrix to GLM matrix
            *@param matrix - matrix to convert
            *@return converted matrix
            */
            static inline glm::mat4 ToGLMMatrix(const QR_Matrix16P& matrix);
        #endif // USING_OPENGL_GLM_LIB
};

//------------------------------------------------------------------------------
// QR_OpenGLHelper implementation - c++ cross-platform
//------------------------------------------------------------------------------
QR_Matrix16P QR_OpenGLHelper::GetOrtho(const M_Precision& left,
                                       const M_Precision& right,
                                       const M_Precision& bottom,
                                       const M_Precision& top,
                                       const M_Precision& zNear,
                                       const M_Precision& zFar)
{
    // OpenGL specifications                                 can be rewritten as
    // | 2/(r-l)     0        0       -(r+l)/(r-l)  |        | 2/(r-l)     0        0      (r+l)/(l-r)  |
    // |   0      2/(t-b)     0       -(t+b)/(t-b)  |   =>   |   0      2/(t-b)     0      (t+b)/(b-t)  |
    // |   0         0     -2/(f-n)   -(f+n)/(f-n)  |        |   0         0     2/(n-f)   (f+n)/(n-f)  |
    // |   0         0        0             1       |        |   0         0        0           1       |
    // invalid for l = r, b = t, or n = f

    // are input values out of bounds?
    if (zNear <= 0.0f || zFar <= 0.0f || left == right || bottom == top || zNear == zFar)
        M_THROW_EXCEPTION("Incorrect input values - cannot create orthogonal matrix");

    // calculate matrix component values
    const M_Precision pfn = zFar  + zNear;
    const M_Precision mnf = zNear - zFar;
    const M_Precision prl = right + left;
    const M_Precision mrl = right - left;
    const M_Precision ptb = top   + bottom;
    const M_Precision mtb = top   - bottom;
    const M_Precision mlr = -mrl;
    const M_Precision mbt = -mtb;

    // build matrix
    return QR_Matrix16P(2.0f / mrl, 0.0f,       0.0f,       prl / mlr,
                        0.0f,       2.0f / mtb, 0.0f,       ptb / mbt,
                        0.0f,       0.0f,       2.0f / mnf, pfn / mnf,
                        0.0f,       0.0f,       0.0f,       1.0f);
}
//------------------------------------------------------------------------------
QR_Matrix16P QR_OpenGLHelper::GetFrustum(const M_Precision& left,
                                         const M_Precision& right,
                                         const M_Precision& bottom,
                                         const M_Precision& top,
                                         const M_Precision& zNear,
                                         const M_Precision& zFar)
{
    // OpenGL specifications                                     can be rewritten as
    // |  2n/(r-l)    0        (r+l)/(r-l)       0      |        |  2n/(r-l)    0        (r+l)/(r-l)       0      |
    // |     0     2n/(t-b)    (t+b)/(t-b)       0      |   =>   |     0     2n/(t-b)    (t+b)/(t-b)       0      |
    // |     0        0       -(f+n)/(f-n)  -2fn/(f-n)  |        |     0        0        (f+n)/(n-f)   2fn/(n-f)  |
    // |     0        0            -1            0      |        |     0        0            -1            0      |
    // invalid for n <= 0, f <= 0, l = r, b = t, or n = f

    // are input values out of bounds?
    if (zNear <= 0.0f || zFar <= 0.0f || left == right || bottom == top || zNear == zFar)
        M_THROW_EXCEPTION("Incorrect input values - cannot create frustum matrix");

    // calculate matrix component values
    const M_Precision x2n  = 2.0f  * zNear;
    const M_Precision x2nf = 2.0f  * zNear * zFar;
    const M_Precision pfn  = zFar  + zNear;
    const M_Precision mnf  = zNear - zFar;
    const M_Precision prl  = right + left;
    const M_Precision mrl  = right - left;
    const M_Precision ptb  = top   + bottom;
    const M_Precision mtb  = top   - bottom;

    // build matrix
    return QR_Matrix16P(x2n / mrl, 0.0f,       prl / mrl, 0.0f,
                        0.0f,      x2n / mtb,  ptb / mtb, 0.0f,
                        0.0f,      0.0f,       pfn / mnf, x2nf / mnf,
                        0.0f,      0.0f,      -1.0f,      0.0f);
}
//------------------------------------------------------------------------------
QR_Matrix16P QR_OpenGLHelper::GetPerspective(const M_Precision& fovyDeg,
                                             const M_Precision& aspect,
                                             const M_Precision& zNear,
                                             const M_Precision& zFar)
{
    // OpenGL specifications                             can be rewritten as
    // |    c/a    0        0            0       |       |    c/a    0         0           0      |
    // |     0     c        0            0       |  ==>  |     0     c         0           0      |
    // |     0     0   -(f+n)/(f-n)  -2nf/(f+n)  |       |     0     0    (f+n)/(n-f)  2nf/(n-f)  |
    // |     0     0       -1            0       |       |     0     0        -1           0      |
    // invalid for n <= 0, f <= 0, a = 0, or n = f

    // are input values out of bounds?
    if (zNear <= 0.0f || zFar <= 0.0f || aspect == 0.0f || zNear == zFar)
        M_THROW_EXCEPTION("Incorrect input values - cannot create orthogonal matrix");

    // calculate field of view, and sinus and cosinus values
    const M_Precision halfFovyRad = QR_Algorithms::GetPI() * fovyDeg / 360.0f;
    const M_Precision si          = std::sin(halfFovyRad);
    const M_Precision co          = std::cos(halfFovyRad);

    // sinus should not be equal to 0
    if (si == 0.0f)
        M_THROW_EXCEPTION("Sinus value is 0");

    // calculate matrix component values
    const M_Precision c    = co / si; // cotangent
    const M_Precision a    = aspect;
    const M_Precision x2nf = 2.0f  * zNear * zFar;
    const M_Precision pfn  = zFar  + zNear;
    const M_Precision mnf  = zNear - zFar;

    // build matrix
    return QR_Matrix16P(c / a, 0.0f,  0.0f,      0.0f,
                        0.0f,  c,     0.0f,      0.0f,
                        0.0f,  0.0f,  pfn / mnf, x2nf / mnf,
                        0.0f,  0.0f, -1.0f,      0.0f);
}
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    void QR_OpenGLHelper::CreateVertexArrayObject(GLuint& vao)
    {
        #ifdef __APPLE__
            glGenVertexArraysOES(1, &vao);
            glBindVertexArrayOES(vao);
        #else
            glGenVertexArrays(1, &vao);
            glBindVertexArray(vao);
        #endif
    }
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    void QR_OpenGLHelper::DeleteVertexArrayObject(GLuint& vao)
    {
        // already deleted?
        if (!vao)
            return;

        // unbind and delete vertex array object
        #ifdef __APPLE__
            glBindVertexArrayOES(0);
            glDeleteVertexArraysOES(1, &vao);
        #else
            glBindVertexArray(0);
            glDeleteVertexArrays(1, &vao);
        #endif

        vao = 0;
    }
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    GLint QR_OpenGLHelper::GetUniform(const QR_Shader*             pShader,
                                            QR_Shader::IEAttribute attribute)
    {
        // no shader?
        if (!pShader)
            return -1;

        // get model matrix slot from shader
        return glGetUniformLocation(pShader->GetProgramID(),
                                    pShader->GetAttributeName(attribute).c_str());
    }
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    GLint QR_OpenGLHelper::GetAttribute(const QR_Shader*             pShader,
                                              QR_Shader::IEAttribute attribute)
    {
        // no shader?
        if (!pShader)
            return -1;

        // get shader interpolation position attribute
        return glGetAttribLocation(pShader->GetProgramID(),
                                   pShader->GetAttributeName(attribute).c_str());
    }
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
#ifdef USING_OPENGL_GLM_LIB
    inline glm::mat4 QR_Matrix16<T>::ToGLMMatrix(const QR_Matrix16P& matrix)
    {
        glm::mat4 glmMatrix;
        std::memcpy(glm::value_ptr(glmMatrix), matrix.GetPtr(), 16 * sizeof(M_Precision));
        return glmMatrix;
    }
#endif
//------------------------------------------------------------------------------

#endif // QR_OPENGLHELPER_H
