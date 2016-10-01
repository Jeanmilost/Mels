/**************************************************************************************************
 * ==> QR_Shader_OpenGL --------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : OpenGL shader language                                                           *
 * Developer   : Jean-Milost Reymond                                                              *
 **************************************************************************************************/

#include "QR_Shader_OpenGL.h"

// std
#include <memory>
#include <fstream>
#include <sstream>

//--------------------------------------------------------------------------------------------------
// QR_Shader_OpenGL
//--------------------------------------------------------------------------------------------------
__fastcall QR_Shader_OpenGL::QR_Shader_OpenGL() :
    TQRShader(),
    m_ProgramID(0),
    m_VertexID(0),
    m_FragmentID(0),
    m_fOnBindAttribute(NULL)
{}
//--------------------------------------------------------------------------------------------------
__fastcall QR_Shader_OpenGL::~QR_Shader_OpenGL()
{
    // delete vertex shader, if needed
    if (m_VertexID)
    {
        glDetachShader(m_ProgramID, m_VertexID);
        glDeleteShader(m_VertexID);
        m_VertexID = 0;
    }

    // delete fragment shader, if needed
    if (m_FragmentID)
    {
        glDetachShader(m_ProgramID, m_FragmentID);
        glDeleteShader(m_FragmentID);
        m_FragmentID = 0;
    }

    // delete program, if needed
    if (m_ProgramID)
    {
        glDeleteProgram(m_ProgramID);
        m_ProgramID = 0;
    }
}
//--------------------------------------------------------------------------------------------------
void QR_Shader_OpenGL::CreateProgram()
{
    // create new shader program
    m_ProgramID = glCreateProgram();

    // succeeded?
    if (!m_ProgramID)
        throw "Failed to create shader program";
}
//--------------------------------------------------------------------------------------------------
NativeUInt __fastcall QR_Shader_OpenGL::GetProgramID()
{
    return m_ProgramID;
}
//--------------------------------------------------------------------------------------------------
NativeUInt __fastcall QR_Shader_OpenGL::AttachFile(TStream* pStream, EQRShaderType shaderType)
{
    // no stream to read from?
    if (!pStream)
        return 0;

    // create string list and read shader from stream
    std::auto_ptr<TStringList> pStringList(new TStringList());
    pStringList->LoadFromStream(pStream);

    // attach shader file content
    return Attach(pStringList->Text, shaderType);
}
//--------------------------------------------------------------------------------------------------
NativeUInt __fastcall QR_Shader_OpenGL::AttachFile(const TFileName fileName, EQRShaderType shaderType)
{
    AnsiString fileNameA(fileName);

    // compile shader
    GLuint shaderID = CompileFile(fileNameA.c_str() ? fileNameA.c_str() : "",
                                  ShaderTypeToOpenGLShaderType(shaderType));

    // succeeded?
    if (!shaderID)
        return 0;

    // attach shader to program
    glAttachShader(m_ProgramID, shaderID);

    // do bind attributes?
    if (m_fOnBindAttribute)
        m_fOnBindAttribute(m_ProgramID, shaderType);

    // search for shader type
    switch (shaderType)
    {
        case EQR_ST_Vertex:
            m_VertexID = shaderID;
            break;

        case EQR_ST_Fragment:
            m_FragmentID = shaderID;
            break;

        default:
            throw "Unknown shader type";
    }

    return shaderID;
}
//--------------------------------------------------------------------------------------------------
NativeUInt __fastcall QR_Shader_OpenGL::Attach(const UnicodeString source, EQRShaderType shaderType)
{
    // get source as ansi string
    AnsiString sourceA(source);

    // compile shader
    GLuint shaderID = Compile(sourceA.c_str() ? sourceA.c_str() : "",
                              ShaderTypeToOpenGLShaderType(shaderType));

    // succeeded?
    if (!shaderID)
        return 0;

    // attach shader to program
    glAttachShader(m_ProgramID, shaderID);

    // do bind attributes?
    if (m_fOnBindAttribute)
        m_fOnBindAttribute(m_ProgramID, shaderType);

    // search for shader type
    switch (shaderType)
    {
        case EQR_ST_Vertex:
            m_VertexID = shaderID;
            break;

        case EQR_ST_Fragment:
            m_FragmentID = shaderID;
            break;

        default:
            throw "Unknown shader type";
    }

    return shaderID;
}
//--------------------------------------------------------------------------------------------------
bool __fastcall QR_Shader_OpenGL::Link(bool use)
{
    // link program
    glLinkProgram(m_ProgramID);

    GLint linked;

    // query program to know if link succeeded
    glGetProgramiv(m_ProgramID, GL_LINK_STATUS, &linked);

    // succeeded?
    if (!linked)
        return false;

    // do use linked program immediately?
    if (use)
        Use(true);

    return true;
}
//--------------------------------------------------------------------------------------------------
void __fastcall QR_Shader_OpenGL::Use(bool use)
{
    // do use program and program exists?
    if (use && m_ProgramID)
        // bind program
        glUseProgram(m_ProgramID);
    else
        // unbind program
        glUseProgram(0);
}
//--------------------------------------------------------------------------------------------------
GLenum QR_Shader_OpenGL::ShaderTypeToOpenGLShaderType(EQRShaderType type)
{
    switch (type)
    {
        case EQR_ST_Vertex:
            return GL_VERTEX_SHADER;

        case EQR_ST_Fragment:
            return GL_FRAGMENT_SHADER;

        default:
            throw "Unknown shader type";
    }
}
//--------------------------------------------------------------------------------------------------
EQRShaderType QR_Shader_OpenGL::OpenGLTypeToShaderType(GLenum type)
{
    switch (type)
    {
        case GL_VERTEX_SHADER:
            return EQR_ST_Vertex;

        case GL_FRAGMENT_SHADER:
            return EQR_ST_Fragment;

        default:
            throw "Unknown shader type";
    }
}
//--------------------------------------------------------------------------------------------------
void QR_Shader_OpenGL::Set_OnBindAttribute(ITfOnBindAttribute fHandler)
{
    m_fOnBindAttribute = fHandler;
}
//--------------------------------------------------------------------------------------------------
GLuint QR_Shader_OpenGL::CompileFile(const std::string& fileName, GLenum type)
{
    // open shader file
    std::ifstream shaderFile(fileName.c_str());

    std::ostringstream sstr;
    std::string        line;

    // iterate through file lines
    while (std::getline(shaderFile, line))
        // add line to source buffer to compile
        sstr << line << "\n";

    return Compile(sstr.str(), type);
}
//--------------------------------------------------------------------------------------------------
GLuint QR_Shader_OpenGL::Compile(const std::string& source, GLenum type)
{
    // create new shader
    GLuint shader = glCreateShader(type);

    // get source buffer
    const char* pSource = source.c_str();

    // compile shader
    glShaderSource(shader, 1, &pSource, NULL);
    glCompileShader(shader);

    // failed?
    if (!shader)
        return 0;

    // return shader index
    return shader;
}
//--------------------------------------------------------------------------------------------------
