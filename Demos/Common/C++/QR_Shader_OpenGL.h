/**************************************************************************************************
 * ==> QR_Shader_OpenGL --------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : OpenGL shader language                                                           *
 * Developer   : Jean-Milost Reymond                                                              *
 **************************************************************************************************/

#ifndef QR_SHADER_OPENGLH
#define QR_SHADER_OPENGLH

// std
#include <string>

// Mels library
#include <UTQR3D.hpp>

// openGL
#define GLEW_STATIC
#include "glew.h"
#include <gl\gl.h>
#include <gl\glu.h>

/**
* OpenGL shader language
*@author Jean-Milost Reymond
*/
class QR_Shader_OpenGL : public TQRShader
{
    public:
        /**
        * Called when a generic attribute should be associated with a named variable
        *@param programID - shader program identifier
        *@param type - shader type
        */
        typedef void (*ITfOnBindAttribute)(std::size_t programID, EQRShaderType type);

        __fastcall QR_Shader_OpenGL();
        virtual __fastcall ~QR_Shader_OpenGL();

        /**
        * Creates program
        */
        virtual void CreateProgram();

        /**
        * Gets shader program identifier
        *@return shader program identifier
        */
        virtual NativeUInt __fastcall GetProgramID();

        /**
        * Attaches shader to program from stream
        *@param pStream - stream containing shader
        *@param type - shader type
        *@return compiled shader identifier
        */
        virtual NativeUInt __fastcall AttachFile(TStream* pStream, EQRShaderType shaderType);

        /**
        * Attaches shader to program from file
        *@param fileName - shader file
        *@param type - shader type
        *@return compiled shader identifier
        */
        virtual NativeUInt __fastcall AttachFile(const TFileName fileName, EQRShaderType shaderType);

        /**
        * Attaches shader to program
        *@param source - shader source code
        *@param type - shader type
        *@return compiled shader identifier
        */
        virtual NativeUInt __fastcall Attach(const UnicodeString source, EQRShaderType shaderType);

        /**
        * Links all attached shader and keep program ready to run
        *@param use - if true, program will be used immediately (in case link succeeded)
        *@return true on success, otherwise false
        */
        virtual bool __fastcall Link(bool use);

        /**
        * Uses the program
        *@param use - if true, program will be used, released otherwise
        */
        virtual void __fastcall Use(bool use);

        /**
        * Converts shader type to OpenGL shader type
        *@param type - shader type to convert
        *@return converted OpenGL shader type
        */
        static GLenum ShaderTypeToOpenGLShaderType(EQRShaderType type);

        /**
        * Converts OpenGL shader type to shader type
        *@param type - OpenGL shader type to convert
        *@return converted shader type
        */
        static EQRShaderType OpenGLTypeToShaderType(GLenum type);

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

#endif // QR_SHADER_OPENGLH
