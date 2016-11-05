// *************************************************************************************************
// * ==> QR_Shader_OpenGL -------------------------------------------------------------------------*
// *************************************************************************************************
// * MIT License - The Mels Library, a free and easy-to-use 3D Models library                      *
// *                                                                                               *
// * Permission is hereby granted, free of charge, to any person obtaining a copy of this software *
// * and associated documentation files (the "Software"), to deal in the Software without          *
// * restriction, including without limitation the rights to use, copy, modify, merge, publish,    *
// * distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the *
// * Software is furnished to do so, subject to the following conditions:                          *
// *                                                                                               *
// * The above copyright notice and this permission notice shall be included in all copies or      *
// * substantial portions of the Software.                                                         *
// *                                                                                               *
// * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING *
// * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND    *
// * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,  *
// * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      *
// * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *
// *************************************************************************************************

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
