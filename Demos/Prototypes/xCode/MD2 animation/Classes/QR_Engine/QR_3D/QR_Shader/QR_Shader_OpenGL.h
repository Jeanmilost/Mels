/*****************************************************************************
 * ==> QR_Shader_OpenGL -----------------------------------------------------*
 *****************************************************************************
 * Description : OpenGL shader language                                      *
 * Developer   : Jean-Milost Reymond                                         *
 *****************************************************************************/

#ifndef QR_SHADER_OPENGLH
#define QR_SHADER_OPENGLH

#ifdef USE_OPENGL_DIRECT_MODE
    #warning "Shaders are not compatible with OpenGL 1.x - cannot use them if direct mode is enabled"
#else
    // std
    #include <string>

    // qr engine
    #include "QR_Types.h"
    #include "QR_Shader.h"

    // openGL
    #ifdef _WIN32
        #define GLEW_STATIC
        #include <GL/glew.h>
    #elif defined (__APPLE__)
        #include <OpenGLES/ES2/gl.h>
    #endif // _WIN32 / __APPLE__

    /**
    * OpenGL shader language
    *@note This class is cross-platform
    *@note This class cannot be compiled if USE_OPENGL_DIRECT_MODE is defined
    *@author Jean-Milost Reymond
    */
    class QR_Shader_OpenGL : public QR_Shader
    {
        public:
            /**
            * Called when a generic attribute should be associated with a named variable
            *@param programID - shader program identifier
            *@param type - shader type
            */
            typedef void (*ITfOnBindAttribute)(QR_UIntPtrT programID, IEType type);

            QR_Shader_OpenGL();
            virtual ~QR_Shader_OpenGL();

            /**
            * Creates program
            */
            virtual void CreateProgram();

            /**
            * Gets shader program identifier
            *@return shader program identifier
            */
            virtual QR_UIntPtrT GetProgramID() const;

            /**
            * Attaches shader to program from file
            *@param fileName - shader file
            *@param type - shader type
            *@return compiled shader identifier
            */
            virtual QR_UIntPtrT AttachFile(const std::string& fileName, IEType type);

            /**
            * Attaches shader to program
            *@param source - shader source code
            *@param type - shader type
            *@return compiled shader identifier
            */
            virtual QR_UIntPtrT Attach(const std::string& source, IEType type);

            /**
            * Links all attached shader and keep program ready to run
            *@param use - if true, program will be used immediately (in case link succeeded)
            *@return true on success, otherwise false
            */
            virtual bool Link(bool use);

            /**
            * Uses the program
            *@param use - if true, program will be used, released otherwise
            */
            virtual void Use(bool use);

            /**
            * Converts shader type to OpenGL shader type
            *@param type - shader type to convert
            *@return converted OpenGL shader type
            */
            static GLenum ShaderTypeToOpenGLShaderType(IEType type);

            /**
            * Converts OpenGL shader type to shader type
            *@param type - OpenGL shader type to convert
            *@return converted shader type
            */
            static IEType OpenGLTypeToShaderType(GLenum type);

            /**
            * Sets OnBindAttribute callback
            *@param fHandler - function handler
            */
            virtual void Set_OnBindAttribute(ITfOnBindAttribute fHandler);

        protected:
            /**
            * Compiles shader from file
            *@param shaderFile - shader file to compile
            *@param type - shader type
            *@return compiled shader identifier
            */
            virtual GLuint CompileFile(const std::string& shaderFile, GLenum type);

            /**
            * Compiles shader
            *@param source - shader source code
            *@param type - shader type
            *@return compiled shader identifier
            */
            virtual GLuint Compile(const std::string& source, GLenum type);

        private:
            GLuint             m_ProgramID;
            GLuint             m_VertexID;
            GLuint             m_FragmentID;
            ITfOnBindAttribute m_fOnBindAttribute;
    };
#endif // USE_OPENGL_DIRECT_MODE

#endif // QR_SHADER_OPENGLH
